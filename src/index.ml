(* The MIT License

Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                   Thomas Gazagnaire <thomas@tarides.com>
                   Ioana Cristescu <ioana@tarides.com>
                   Clément Pascutto <clement@tarides.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software. *)

include Index_intf
open! Import

module Key = struct
  module type S = Key

  module String_fixed = Data.String_fixed
end

module Value = struct
  module type S = Value

  module String_fixed = Data.String_fixed
end

exception RO_not_allowed
(** Raised whenever a read-only instance performs a write action. *)

exception RW_not_allowed
(** Raised whenever a read-write instance performs a reserved read-only action. *)

exception Closed
(** Raised whenever a closed instance is used. *)

module Make_private
    (K : Key)
    (V : Value)
    (Platform : PLATFORM)
    (Cache : Cache.S) =
struct
  open Platform

  type 'a async = 'a Thread.t

  let await = Thread.await

  type key = K.t
  type value = V.t

  let pp_key = Repr.pp K.t
  let pp_value = Repr.pp V.t

  module Entry = struct
    include Data.Entry.Make (K) (V)
    module Key = K
    module Value = V

    let to_key { key; _ } = key
    let to_value { value; _ } = value
  end

  let entry_sizeL = Int63.of_int Entry.encoded_size

  module Stats = struct
    include Stats.Make (Clock)
    include Stats
  end

  module Tbl = Hashtbl.Make (K)

  module IO = struct
    include Io.Extend (IO)

    let iter ?min ?max f =
      let page_size = Int63.(mul entry_sizeL (of_int 1_000)) in
      iter ~page_size ?min ?max (fun ~off ~buf ~buf_off ->
          let entry = Entry.decode buf buf_off in
          f off entry;
          Entry.encoded_size)
  end

  let iter_io ?min ?max f = IO.iter ?min ?max (fun _ -> f)

  type throttle = [ `Overcommit_memory | `Block_writes ]

  type config = {
    log_size : int;  (** The log maximal size before triggering a [merge]. *)
    readonly : bool;
    fresh : bool;
        (** Whether the index was created from scratch, erasing existing data,
            if any. *)
    throttle : throttle;  (** The throttle strategy used when merges stack. *)
    flush_callback : unit -> unit;
        (** A callback called whenever the index is flushed. Useful to sync
            index's flushes with the flushes of larger systems. *)
  }

  module IOArray = Io_array.Make (IO) (Entry)

  module Search =
    Search.Make (Entry) (IOArray)
      (struct
        type t = int

        module Entry = Entry

        let compare : int -> int -> int = compare
        let of_entry e = e.Entry.key_hash
        let of_key = K.hash

        let linear_interpolate ~low:(low_index, low_metric)
            ~high:(high_index, high_metric) key_metric =
          let low_in = float_of_int low_metric in
          let high_in = float_of_int high_metric in
          let target_in = float_of_int key_metric in
          let low_out = Int63.to_float low_index in
          let high_out = Int63.to_float high_index in
          (* Fractional position of [target_in] along the line from [low_in] to [high_in] *)
          let proportion = (target_in -. low_in) /. (high_in -. low_in) in
          (* Convert fractional position to position in output space *)
          let position = low_out +. (proportion *. (high_out -. low_out)) in
          let rounded = ceil (position -. 0.5) +. 0.5 in
          Int63.of_float rounded
      end)

  (** A [Data_file] is a persisted, sorted sequence of entries with a
      corresponding persistant [Fan] for fast approximate lookup. *)
  module Data_file : sig
    type t

    val load : string -> t option
    val find : t -> key -> value
    val length : t -> int
    val clear : t -> unit
    val close : t -> unit

    type data_file := t

    module Iterator : sig
      type t

      val create : data_file -> t

      val pop_exn : t -> Entry.t
      (** May raise [End_of_file]. *)
    end

    module Builder : sig
      type t

      val create : path:string -> generation:int63 -> estimated_length:int -> t
      val add : t -> Entry.t -> unit
      val finalise : t -> data_file
    end
  end = struct
    type 'perms t_with_perms = {
      io : IO.t;  (** The disk file handler. *)
      fan_out : 'perms Fan.t;
          (** The fan-out, used to map keys to small intervals in the file, in
              constant time. This is an in-memory object, also encoded in the
              header of [io]. *)
      path : string;
      generation : int63;
    }

    type t = [ `Read ] t_with_perms

    let load path =
      match IO.v_readonly path with
      | Error `No_file_on_disk -> failwith "TODO: race condition?"
      | Ok io ->
          let fan_out = Fan.import ~hash_size:K.hash_size (IO.get_fanout io) in
          let generation = IO.get_generation io in
          (* We maintain that [index] is [None] if the file is empty. *)
          if IO.offset io = Int63.zero then None
          else Some { fan_out; io; path; generation }

    let find index key =
      let hashed_key = K.hash key in
      let low_bytes, high_bytes = Fan.search index.fan_out hashed_key in
      let low, high =
        Int63.(div low_bytes entry_sizeL, div high_bytes entry_sizeL)
      in
      Search.interpolation_search (IOArray.v index.io) key ~low ~high

    let length t = Int63.(to_int_exn (IO.offset t.io / entry_sizeL))

    let clear { io; generation; _ } =
      IO.clear ~generation:Int63.(generation + one) ~reopen:false io

    let close t = IO.close t.io

    module Iterator = struct
      type t = { mutable cursor_offset : int63; instance : t }
    end

    module Builder = struct
      type nonrec t = [ `Write ] t_with_perms

      let create ~path ~generation ~estimated_length =
        let fan_out =
          Fan.v ~hash_size:K.hash_size ~entry_size:Entry.encoded_size
            estimated_length
        in
        let fan_size = Int63.of_int (Fan.exported_size fan_out) in
        let io = IO.v ~fresh:true ~generation ~fan_size path in
        { io; fan_out; path; generation }

      let add t entry =
        Fan.update t.fan_out entry.Entry.key_hash (IO.offset t.io);
        Entry.encode entry (IO.append t.io)

      let finalise t =
        let fan_out = Fan.finalize (* TODO: US/UK consistency *) t.fan_out in
        IO.set_fanout t.io (Fan.export fan_out);
        { t with fan_out }
    end
  end

  module Data_array : sig
    type t

    (* TODO: do we really need this? *)
    val empty : root:string -> t
    val load : root:string -> t
    val find : t -> key -> value
    val sync : t -> unit

    val add : t -> generation:int63 -> Entry.t array -> unit
    (** The entries to be added are assumed to be pre-sorted.

        TODO: keep [generation] internally? *)

    (* TODO: expose [strategy:[`Minimal | `Complete]] parameter, which will be
             necessary for [filter] to retain its current semantics. *)

    val clear : t -> unit
  end = struct
    type file_vector = Data_file.t option Vector.t
    (* The files are arranged in increasing order of size by powers of two.

       Specifically, for any index [i] into the vector: if that slot is occupied
       by a data file, then the file contains a number of bindings in the range
       [[2^{i+1}, 2^{i+2}\]].

       TODO: keep vector relative to some smallest index, to avoid iterating
             over many [None]s at the start of the vector in the common case.

       TODO: how to handle duplicate bindings lowering the slot count after the
             merge is complete? I think this is probably fine: selected inputs
             will be a contiguous region of the vector, and it's not possible
             for the resulting output to be smaller than the largest input
             selected from that region. *)

    type slot = Slot of int [@@unboxed]
    (* An index into a [file_vector]. *)

    let next_slot (Slot n) = Slot (n + 1)

    let get_slot vec (Slot n) =
      if n >= Vector.length vec then None else Vector.get vec n

    type t = { files : file_vector; root : string }

    let slot_of_size n =
      assert (n > 0);
      let rec loop acc = function
        | 1 -> Slot acc
        | n -> loop (succ acc) (n / 2)
      in
      loop 0 n

    type merge_spec = { inputs : (slot * Data_file.t) list }

    let compute_input_slots_for_merge :
        file_vector -> batch_size:int -> merge_spec =
     (* TODO: have some notion of "smallest merge granularity" to account for
              forced merges with when the log size is smaller than the limit. We
              don't want to accumulate many small data files unless the user
              actually needs this -- i.e. the [log_size] parameter is very
              small. *)
     fun vec ~batch_size ->
      let rec loop acc i =
        match get_slot vec i with
        | None ->
            (* This slot is unoccupied: we can aim to write directly to it. *)
            { inputs = acc }
        | Some df -> loop ((i, df) :: acc) (next_slot i)
      in
      let initial_slot = slot_of_size batch_size in
      loop [] initial_slot

    module Name = struct
      type t = { fname : string; slot : slot }

      let create_exn fname =
        let invalid () = Fmt.failwith "Invalid file name format: %s" fname in
        match String.split_on_char '-' fname with
        | [ "data"; size ] ->
            let index = int_of_string size in
            if index < 0 then invalid ();
            { fname; slot = Slot index }
        | _ -> invalid ()

      let compare { slot = Slot a; _ } { slot = Slot b; _ } = Int.compare a b
      let create i = Fmt.str "data-%d" i
    end

    let empty ~root = { files = Vector.create (); root }

    let load ~root =
      let path = Layout.data ~root in
      let occupied_slots = Sys.readdir path |> Array.map Name.create_exn in
      Array.sort Name.compare occupied_slots;
      let (Slot largest_slot_idx) =
        occupied_slots.(Array.length occupied_slots - 1).slot
      in
      let files = Vector.make (largest_slot_idx + 1) None in
      ArrayLabels.iter occupied_slots ~f:(fun slot ->
          let data_file =
            let path = Filename.concat root slot.Name.fname in
            Data_file.load path
          in
          let (Slot slot_idx) = slot.Name.slot in
          Vector.set files slot_idx data_file);

      { files; root }

    let find t key =
      (* Find by searching in each file in increasing order. The ordering is
         important for correctness, as smaller data-files may contain bindings
         that shadow stale bindings (for the same key) held in larger files. *)
      let nb_files = Vector.length t.files in
      let rec loop i =
        if i > nb_files then assert false;
        if i = nb_files then raise Not_found;
        match Vector.get t.files i with
        | None -> loop (i + 1)
        | Some data_file -> (
            try Data_file.find data_file key with Not_found -> loop (i + 1))
      in
      loop 0

    let sync t =
      (* Close the file handler to be able to reload it, as the file may have
         changed after a merge. *)
      let new_files = (load ~root:t.root).files in
      Vector.swap t.files new_files

    let add t ~generation entries =
      let batch_size = Array.length entries in
      let spec = compute_input_slots_for_merge t.files ~batch_size in
      match spec.inputs with
      | [] -> assert false (* Just dump log straight to [output] *)
      | _ :: _ ->
          let nb_input_entries =
            ListLabels.fold_left spec.inputs ~init:0 ~f:(fun acc (_slot, df) ->
                acc + Data_file.length df)
          in
          let output_file =
            let path = Layout.merge ~root:t.root in
            let estimated_length = nb_input_entries + batch_size in
            Data_file.Builder.create ~path ~generation ~estimated_length
          in
          assert false

    let clear t =
      (* TODO: consider race conditions *)
      Vector.iter (Option.iter Data_file.clear) t.files;
      Vector.clear t.files
  end

  type log = {
    io : IO.t;  (** The disk file handler. *)
    mem : value Tbl.t;  (** The in-memory counterpart of [io]. *)
  }

  type instance = {
    config : config;
    root : string;  (** The root location of the index *)
    mutable generation : int63;
        (** The generation is a counter of rewriting operations (e.g. [clear]
            and [merge]). It is used to sync RO instances. *)
    mutable data : Data_array.t;
        (** The main index file contains old sorted bindings. It is [None] when
            no [merge] occurred yet. On RO instances, this is also [None] when
            the file is empty, e.g. after a [clear]. *)
    mutable log : log option;
        (** The log file contains the most recent bindings. It can be [None]
            when a read-only instance is created before its RW counterpart; in
            that case the [log] creation is pending. *)
    mutable log_async : log option;
        (** The log_async file contains bindings added concurrently to a [merge]
            operation. It is only present when a merge is ongoing. *)
    mutable open_instances : int;
        (** The number of open instances that are shared through the [Cache.t]. *)
    writer_lock : IO.Lock.t option;
        (** A lock that prevents multiple RW instances to be open at the same
            time. *)
    sync_lock : Semaphore.t;
        (** A lock that prevents multiple [sync] to happen at the same time. *)
    merge_lock : Semaphore.t;
        (** A lock that prevents multiple [merges] to happen at the same time. *)
    rename_lock : Semaphore.t;
        (** A lock used to protect a critical bit when finalizing a [merge]
            operation. All operations should be guarded by this lock. *)
    mutable pending_cancel : bool;
        (** A signal for the merging thread to terminate prematurely. *)
  }

  include Private_types

  let check_pending_cancel instance =
    match instance.pending_cancel with true -> `Abort | false -> `Continue

  (* [t] is an [option ref] to handle [close] operations. A closed index is None. *)
  type t = instance option ref

  let check_open t =
    match !t with Some instance -> instance | None -> raise Closed

  (** {1 Clear} *)

  let clear' ~hook t =
    let t = check_open t in
    Log.debug (fun l -> l "clear %S" t.root);
    if t.config.readonly then raise RO_not_allowed;
    t.pending_cancel <- true;
    hook `Abort_signalled;
    Semaphore.with_acquire "clear" t.merge_lock (fun () ->
        t.pending_cancel <- false;
        t.generation <- Int63.succ t.generation;
        let log = Option.get t.log in
        let hook () = hook `IO_clear in
        IO.clear ~generation:t.generation ~hook ~reopen:true log.io;
        Tbl.clear log.mem;
        Option.iter
          (fun (l : log) ->
            IO.clear ~generation:t.generation ~reopen:false l.io)
          t.log_async;

        Data_array.clear t.data;
        t.log_async <- None)

  let clear = clear' ~hook:(fun _ -> ())

  (** {1 Flush} *)

  let flush_instance ?no_async ?no_callback ?(with_fsync = false) instance =
    Log.debug (fun l ->
        l "[%s] flushing instance" (Filename.basename instance.root));
    if instance.config.readonly then raise RO_not_allowed;
    instance.log
    |> Option.iter (fun log ->
           Log.debug (fun l ->
               l "[%s] flushing log" (Filename.basename instance.root));
           IO.flush ?no_callback ~with_fsync log.io);

    match (no_async, instance.log_async) with
    | Some (), _ | None, None -> ()
    | None, Some log ->
        Log.debug (fun l ->
            l "[%s] flushing log_async" (Filename.basename instance.root));
        IO.flush ?no_callback ~with_fsync log.io

  let flush ?no_callback ?(with_fsync = false) t =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] flush" (Filename.basename t.root));
    Semaphore.with_acquire "flush" t.rename_lock (fun () ->
        flush_instance ?no_callback ~with_fsync t)

  (** {1 RO instances syncing} *)

  (** Loads the log file at [path], if it exists. Used by RO instances to load
      the temporary [log_async], or to fill the [log] field when they have been
      created before their RW counterpart. *)
  let try_load_log t path =
    match IO.v_readonly path with
    | Error `No_file_on_disk -> None
    | Ok io ->
        let mem = Tbl.create 0 in
        iter_io (fun e -> Tbl.replace mem e.key e.value) io;
        Log.debug (fun l ->
            l "[%s] loaded %d entries from %s" (Filename.basename t.root)
              (Tbl.length mem) (Filename.basename path));
        Some { io; mem }

  (** Loads the entries from [log]'s IO into memory, starting from offset [min]. *)
  let sync_log_entries ?min log =
    let add_log_entry (e : Entry.t) = Tbl.replace log.mem e.key e.value in
    if min = None then Tbl.clear log.mem;
    iter_io ?min add_log_entry log.io

  (** Syncs the [log_async] of the instance by checking on-disk changes. *)
  let sync_log_async ~hook t =
    match t.log_async with
    | None ->
        hook `Reload_log_async;
        t.log_async <- try_load_log t (Layout.log_async ~root:t.root)
    | Some log ->
        let offset = IO.offset log.io in
        let h = IO.Header.get log.io in
        if
          (* the generation has changed *)
          h.generation > Int63.succ t.generation
          || (* the last sync was done between clear(log) and clear(log_async) *)
          (h.generation = Int63.succ t.generation && h.offset = Int63.zero)
        then (
          (* close the file .*)
          IO.close log.io;
          (* check that file is on disk, reopen and reload everything. *)
          hook `Reload_log_async;
          t.log_async <- try_load_log t (Layout.log_async ~root:t.root)
          (* else if the disk offset is greater, reload the newest data. *))
        else if offset < h.offset then sync_log_entries ~min:offset log
          (* else if the offset is lesser, that means the [log_async] was
             cleared, and the generation should have changed. *)
        else if offset > h.offset then assert false

  (** Syncs the [index] of the instance by checking on-disk changes. *)
  let sync_index t = Data_array.sync t.data

  (** Syncs an instance entirely, by checking on-disk changes for [log], [sync],
      and [log_async]. *)
  let sync_instance ?(hook = fun _ -> ()) t =
    Semaphore.with_acquire "sync" t.sync_lock @@ fun () ->
    Log.debug (fun l ->
        l "[%s] checking for changes on disk (generation=%a)"
          (Filename.basename t.root) Int63.pp t.generation);
    (* the first sync needs to load the log file. *)
    let () =
      match t.log with
      | None -> t.log <- try_load_log t (Layout.log ~root:t.root)
      | Some _ -> ()
    in
    (* There is a race between sync and merge:

       - At the end of the merge, the entries in log_async are copied
       into log. [merge] starts by calling IO.Header.set(log) with a
       new generation number, copies all the entries and then clear
       log_async.

       - so here we need to make sure we do the same thing in reverse:
       start by syncing [log_async], then read [log]'s headers. At
       worse, [log_async] and [log] might contain duplicated entries,
       but we won't miss any. These entries will be added to [log.mem]
       using Tbl.replace where they will be deduplicated. *)
    sync_log_async ~hook t;
    match t.log with
    | None -> ()
    | Some log ->
        (* This one is the cached offset, from the previous sync. *)
        let log_offset = IO.offset log.io in
        hook `Before_offset_read;
        let h = IO.Header.get log.io in
        hook `After_offset_read;
        if t.generation <> h.generation then (
          (* If the generation has changed, then we need to reload both the
             [index] and the [log]. The new generation is the one on disk. *)
          Log.debug (fun l ->
              l "[%s] generation has changed: %a -> %a"
                (Filename.basename t.root) Int63.pp t.generation Int63.pp
                h.generation);
          hook `Reload_log;
          t.generation <- h.generation;
          IO.close log.io;
          t.log <- try_load_log t (Layout.log ~root:t.root);
          (* The log file is never removed (even by clear). *)
          assert (t.log <> None);
          sync_index t)
        else if log_offset < h.offset then (
          (* else if the disk offset is greater, we read the newest bindings. *)
          Log.debug (fun l ->
              l "[%s] new entries detected, reading log from disk"
                (Filename.basename t.root));
          sync_log_entries ~min:log_offset log)
        else
          (* Here the disk offset should be equal to the known one. A smaller
             offset should not be possible, because that would mean a [clear] or
             [merge] occurred, which should have changed the generation. *)
          (* TODO: Handle the "impossible" case differently? *)
          Log.debug (fun l ->
              l "[%s] no changes detected" (Filename.basename t.root))

  (** {1 Find and Mem}*)

  (** Finds the value associated to [key] in [t]. In order, checks in
      [log_async] (in memory), then [log] (in memory), then [data] (on disk). *)
  let find_instance t key =
    let find_if_exists ~name ~find db =
      match db with
      | None -> raise Not_found
      | Some e ->
          let ans = find e key in
          Log.debug (fun l ->
              l "[%s] found in %s" (Filename.basename t.root) name);
          ans
    in
    let find_log_async () =
      find_if_exists ~name:"log_async"
        ~find:(fun log -> Tbl.find log.mem)
        t.log_async
    in
    let find_log () =
      find_if_exists ~name:"log" ~find:(fun log -> Tbl.find log.mem) t.log
    in
    let find_data () = Data_array.find t.data key in
    Semaphore.with_acquire "find_instance" t.rename_lock @@ fun () ->
    match find_log_async () with
    | e -> e
    | exception Not_found -> (
        match find_log () with e -> e | exception Not_found -> find_data ())

  let find t key =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] find %a" (Filename.basename t.root) pp_key key);
    find_instance t key

  let mem t key =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] mem %a" (Filename.basename t.root) pp_key key);
    match find_instance t key with _ -> true | exception Not_found -> false

  let sync' ?hook t =
    let f t =
      Stats.incr_nb_sync ();
      let t = check_open t in
      Log.info (fun l -> l "[%s] sync" (Filename.basename t.root));
      if t.config.readonly then sync_instance ?hook t else raise RW_not_allowed
    in
    Stats.sync_with_timer (fun () -> f t)

  let sync = sync' ?hook:None

  (** {1 Index creation} *)

  let v_no_cache ?(flush_callback = fun () -> ()) ~throttle ~fresh ~readonly
      ~log_size root =
    Log.debug (fun l ->
        l "[%s] not found in cache, creating a new instance"
          (Filename.basename root));
    let writer_lock =
      if not readonly then Some (IO.Lock.lock (Layout.lock ~root)) else None
    in
    let config =
      {
        log_size = log_size * Entry.encoded_size;
        readonly;
        fresh;
        throttle;
        flush_callback;
      }
    in
    let log_path = Layout.log ~root in
    let log =
      if readonly then if fresh then raise RO_not_allowed else None
      else
        let io =
          IO.v ~flush_callback ~fresh ~generation:Int63.zero
            ~fan_size:Int63.zero log_path
        in
        let entries = Int63.(to_int_exn (IO.offset io / entry_sizeL)) in
        Log.debug (fun l ->
            l "[%s] log file detected. Loading %d entries"
              (Filename.basename root) entries);
        let mem = Tbl.create entries in
        iter_io (fun e -> Tbl.replace mem e.key e.value) io;
        Some { io; mem }
    in
    let generation =
      match log with None -> Int63.zero | Some log -> IO.get_generation log.io
    in
    let log_async_path = Layout.log_async ~root in
    (* If we are in readonly mode, the log_async will be read during sync_log so
       there is no need to do it here. *)
    if (not readonly) && IO.exists log_async_path then (
      let io =
        IO.v ~flush_callback ~fresh ~generation:Int63.zero ~fan_size:Int63.zero
          log_async_path
      in
      let entries = Int63.div (IO.offset io) entry_sizeL in
      Log.debug (fun l ->
          l "[%s] log_async file detected. Loading %a entries"
            (Filename.basename root) Int63.pp entries);
      (* If we are not in fresh mode, we move the contents of log_async to
         log. *)
      if not fresh then
        Option.iter
          (fun log ->
            let append_io = IO.append log.io in
            iter_io
              (fun e ->
                Tbl.replace log.mem e.key e.value;
                Entry.encode e append_io)
              io;
            IO.flush log.io;
            IO.clear ~generation ~reopen:false io)
          log);
    let data =
      let root = Layout.data ~root in
      if readonly then Data_array.empty ~root else Data_array.load ~root
    in
    {
      config;
      generation;
      log;
      log_async = None;
      root;
      data;
      open_instances = 1;
      merge_lock = Semaphore.make true;
      rename_lock = Semaphore.make true;
      sync_lock = Semaphore.make true;
      writer_lock;
      pending_cancel = false;
    }

  type cache = (string * bool, instance) Cache.t

  let empty_cache = Cache.create

  let v ?(flush_callback = fun () -> ()) ?(cache = empty_cache ())
      ?(fresh = false) ?(readonly = false) ?(throttle = `Block_writes) ~log_size
      root =
    let new_instance () =
      let instance =
        v_no_cache ~flush_callback ~fresh ~readonly ~log_size ~throttle root
      in
      if readonly then sync_instance instance;
      Cache.add cache (root, readonly) instance;
      ref (Some instance)
    in
    Log.info (fun l ->
        l "[%s] v fresh=%b readonly=%b log_size=%d" (Filename.basename root)
          fresh readonly log_size);
    match (Cache.find cache (root, readonly), IO.exists (Layout.log ~root)) with
    | None, _ -> new_instance ()
    | Some _, false ->
        Log.debug (fun l ->
            l "[%s] does not exist anymore, cleaning up the fd cache"
              (Filename.basename root));
        Cache.remove cache (root, true);
        Cache.remove cache (root, false);
        new_instance ()
    | Some t, true -> (
        match t.open_instances with
        | 0 ->
            Cache.remove cache (root, readonly);
            new_instance ()
        | _ ->
            Log.debug (fun l ->
                l "[%s] found in cache" (Filename.basename root));
            t.open_instances <- t.open_instances + 1;
            if readonly then sync_instance t;
            let t = ref (Some t) in
            if fresh then clear t;
            t)

  (** {1 Merges} *)

  (** Appends the entry encoded in [buf] into [dst_io] and registers it in
      [fan_out] with hash [hash]. *)
  let append_substring_fanout fan_out hash dst_io buf ~off ~len =
    Fan.update fan_out hash (IO.offset dst_io);
    IO.append_substring dst_io buf ~off ~len

  (** Appends [entry] into [dst_io] and registers it in [fan_out]. *)
  let append_entry_fanout fan_out entry dst_io =
    Fan.update fan_out entry.Entry.key_hash (IO.offset dst_io);
    Entry.encode entry (IO.append dst_io)

  (** Appends the [log] values into [dst_io], from [log_i] to the first value
      which hash is higher than or equal to [hash_e] (the current value in
      [data]), excluded, and returns its index. Also registers the appended
      values to [fan_out]. *)
  let rec merge_from_log fan_out log log_i hash_e dst_io =
    if log_i >= Array.length log then log_i
    else
      let v = log.(log_i) in
      if v.Entry.key_hash >= hash_e then log_i
      else (
        append_entry_fanout fan_out v dst_io;
        (merge_from_log [@tailcall]) fan_out log (log_i + 1) hash_e dst_io)

  (** Appends the [log] values into [dst_io], from [log_i] to the end, and
      registers them in [fan_out]. *)
  let append_remaining_log fan_out log log_i dst_io =
    for log_i = log_i to Array.length log - 1 do
      append_entry_fanout fan_out log.(log_i) dst_io
    done

  (** Merges [log] with [index] into [dst_io], ignoring bindings that do not
      satisfy [filter (k, v)]. [log] must be sorted by key hashes. *)
  let merge_with ~hook ~yield ~filter log index_io fan_out dst_io =
    (* We read the [index] by page; the [refill] function is in charge of
       refilling the page buffer when empty. *)
    let len = 10_000 * Entry.encoded_size in
    let buf = Bytes.create len in
    let refill off = ignore (IO.read index_io ~off ~len buf) in
    let index_end = IO.offset index_io in
    refill Int63.zero;
    let filter =
      Option.fold
        ~none:(fun _ _ -> true)
        ~some:(fun f key entry_off ->
          (* When the filter is not provided, we don't need to decode the value. *)
          let value =
            Entry.decode_value (Bytes.unsafe_to_string buf) entry_off
          in
          f (key, value))
        filter
    in
    (* This performs the merge. [index_offset] is the offset of the next entry
       to process in [index], [buf_offset] is its counterpart in the page
       buffer. [log_i] is the index of the next entry to process in [log]. *)
    let rec go first_entry index_offset buf_offset log_i =
      (* If the index is fully read, we append the rest of the [log]. *)
      if index_offset >= index_end then (
        append_remaining_log fan_out log log_i dst_io;
        `Completed)
      else
        let index_offset = Int63.add index_offset entry_sizeL in
        let index_key, index_key_hash =
          Entry.decode_key (Bytes.unsafe_to_string buf) buf_offset
        in
        let log_i = merge_from_log fan_out log log_i index_key_hash dst_io in
        match yield () with
        | `Abort -> `Aborted
        | `Continue ->
            (* This yield is used to balance the resources between the merging
               thread (here) and the main thread. *)
            Thread.yield ();
            (* If the log entry has the same key as the index entry, we do not
               add the index one, respecting the [replace] semantics. *)
            if
              (log_i >= Array.length log
              ||
              let log_key = log.(log_i).key in
              not K.(equal log_key index_key))
              && filter index_key buf_offset
            then
              append_substring_fanout fan_out index_key_hash dst_io
                (Bytes.unsafe_to_string buf)
                ~off:buf_offset ~len:Entry.encoded_size;
            if first_entry then hook `After_first_entry;
            let buf_offset =
              let n = buf_offset + Entry.encoded_size in
              (* If the buffer is entirely consumed, refill it. *)
              if n >= Bytes.length buf then (
                refill index_offset;
                0)
              else n
            in
            (go [@tailcall]) false index_offset buf_offset log_i
    in
    (go [@tailcall]) true Int63.zero 0 0

  (** Increases and returns the merge counter. *)
  let merge_counter =
    let n = ref 0 in
    fun () ->
      incr n;
      !n

  (** Merges the entries in [t.log] into the data file, ensuring that concurrent
      writes are not lost.

      The caller must ensure the following:

      - [t.log] has been loaded;
      - [t.log_async] has been created;
      - [t.merge_lock] is acquired before entry, and released immediately after
        this function returns or raises an exception. *)
  let unsafe_perform_merge ~witness ~filter ~hook t =
    hook `Before;
    let log = Option.get t.log in
    let generation = Int63.succ t.generation in
    let log_array =
      Option.iter
        (fun f ->
          Tbl.filter_map_inplace
            (fun key value -> if f (key, value) then Some value else None)
            log.mem)
        filter;
      let b = Array.make (Tbl.length log.mem) witness in
      Tbl.fold
        (fun key value i ->
          b.(i) <- Entry.v key value;
          i + 1)
        log.mem 0
      |> ignore;
      Array.fast_sort Entry.compare b;
      b
    in
    let fan_size, old_fan_nb =
      match t.index with
      | None -> (Tbl.length log.mem, None)
      | Some index ->
          ( Int63.(to_int_exn (IO.offset index.io / entry_sizeL))
            + Array.length log_array,
            Some (Fan.nb_fans index.fan_out) )
    in
    let fan_out =
      Fan.v ~hash_size:K.hash_size ~entry_size:Entry.encoded_size fan_size
    in
    let () =
      match old_fan_nb with
      | Some on ->
          let new_fan_nb = Fan.nb_fans fan_out in
          if new_fan_nb <> on then
            Log.info (fun m ->
                m "the number of fan-out entries has changed: %d to %d" on
                  new_fan_nb)
      | _ -> ()
    in
    let merge =
      let merge_path = Layout.merge ~root:t.root in
      IO.v ~fresh:true ~generation
        ~fan_size:(Int63.of_int (Fan.exported_size fan_out))
        merge_path
    in
    let merge_result : [ `Index_io of IO.t | `Aborted ] =
      match t.index with
      | None -> (
          match check_pending_cancel t with
          | `Abort -> `Aborted
          | `Continue ->
              let io =
                IO.v ~fresh:true ~generation ~fan_size:Int63.zero
                  (Layout.data ~root:t.root)
              in
              append_remaining_log fan_out log_array 0 merge;
              `Index_io io)
      | Some index -> (
          match
            merge_with ~hook
              ~yield:(fun () -> check_pending_cancel t)
              ~filter log_array index.io fan_out merge
          with
          | `Completed -> `Index_io index.io
          | `Aborted -> `Aborted)
    in
    match merge_result with
    | `Aborted -> (`Aborted, Mtime.Span.zero)
    | `Index_io io ->
        let fan_out = Fan.finalize fan_out in
        let index = { io; fan_out } in
        IO.set_fanout merge (Fan.export index.fan_out);
        let before_rename_lock = Clock.counter () in
        let rename_lock_duration =
          Semaphore.with_acquire "merge-rename" t.rename_lock (fun () ->
              let rename_lock_duration = Clock.count before_rename_lock in
              IO.rename ~src:merge ~dst:index.io;
              t.index <- Some index;
              t.generation <- generation;
              IO.clear ~generation ~reopen:true log.io;
              Tbl.clear log.mem;
              hook `After_clear;
              let log_async = Option.get t.log_async in
              let append_io = IO.append log.io in
              Tbl.iter
                (fun key value ->
                  Tbl.replace log.mem key value;
                  Entry.encode' key value append_io)
                log_async.mem;
              (* NOTE: It {i may} not be necessary to trigger the
                 [flush_callback] here. If the instance has been recently
                 flushed (or [log_async] just reached the [auto_flush_limit]),
                 we're just moving already-persisted values around. However, we
                 trigger the callback anyway for simplicity. *)
              (* `fsync` is necessary, since bindings in `log_async` may have
                 been explicitely `fsync`ed during the merge, so we need to
                 maintain their durability. *)
              IO.flush ~with_fsync:true log.io;
              IO.clear ~generation:(Int63.succ generation) ~reopen:false
                log_async.io;
              (* log_async.mem does not need to be cleared as we are discarding
                 it. *)
              t.log_async <- None;
              rename_lock_duration)
        in
        hook `After;
        (`Completed, rename_lock_duration)

  let merge' ?(blocking = false) ?filter ?(hook = fun _ -> ()) ~witness
      ?(force = false) t =
    let merge_started = Clock.counter () in
    let merge_id = merge_counter () in
    let msg = Fmt.strf "merge { id=%d }" merge_id in
    Semaphore.acquire msg t.merge_lock;
    let merge_lock_wait = Clock.count merge_started in
    Log.info (fun l ->
        let pp_forced ppf () = if force then Fmt.string ppf "; force=true" in
        l "[%s] merge started { id=%d%a }" (Filename.basename t.root) merge_id
          pp_forced ());
    Stats.incr_nb_merge ();
    let log_async =
      let io =
        let log_async_path = Layout.log_async ~root:t.root in
        IO.v ~flush_callback:t.config.flush_callback ~fresh:true
          ~generation:(Int63.succ t.generation) ~fan_size:Int63.zero
          log_async_path
      in
      let mem = Tbl.create 0 in
      { io; mem }
    in
    t.log_async <- Some log_async;
    (* NOTE: We flush [log] {i after} enabling [log_async] to ensure that no
       unflushed bindings make it into [log] before being merged into the index.
       This satisfies the invariant that all bindings are {i first} persisted in
       a log, so that the [index] IO doesn't need to trigger the
       [flush_callback]. *)
    flush_instance ~no_async:() ~with_fsync:true t;
    let go () =
      let merge_result, rename_lock_wait =
        Fun.protect
          (fun () -> unsafe_perform_merge ~filter ~hook ~witness t)
          ~finally:(fun () -> Semaphore.release t.merge_lock)
      in
      let total_duration = Clock.count merge_started in
      let merge_duration = Mtime.Span.abs_diff total_duration merge_lock_wait in
      Stats.add_merge_duration merge_duration;
      Log.info (fun l ->
          let action =
            match merge_result with
            | `Aborted -> "aborted"
            | `Completed -> "completed"
          in
          l
            "[%s] merge %s { id=%d; total-duration=%a; merge-duration=%a; \
             merge-lock=%a; rename-lock=%a }"
            (Filename.basename t.root) action merge_id Mtime.Span.pp
            total_duration Mtime.Span.pp merge_duration Mtime.Span.pp
            merge_lock_wait Mtime.Span.pp rename_lock_wait);
      merge_result
    in
    if blocking then go () |> Thread.return else Thread.async go

  let get_witness t =
    match t.log with
    | None -> None
    | Some log -> (
        let exception Found of Entry.t in
        (* Gets an arbitrary element in [log]. *)
        match
          Tbl.iter (fun key value -> raise (Found (Entry.v key value))) log.mem
        with
        | exception Found e -> Some e
        | () -> (
            (* If [log] is empty, get an entry in [index]. *)
            match t.index with
            | None -> None
            | Some index ->
                let buf = Bytes.create Entry.encoded_size in
                let n =
                  IO.read index.io ~off:Int63.zero ~len:Entry.encoded_size buf
                in
                assert (n = Entry.encoded_size);
                Some (Entry.decode (Bytes.unsafe_to_string buf) 0)))

  (** This triggers a merge if the [log] exceeds [log_size], or if the [log]
      contains entries and [force] is true *)
  let try_merge_aux ?hook ?(force = false) t =
    let t = check_open t in
    let witness =
      Semaphore.with_acquire "witness" t.rename_lock (fun () -> get_witness t)
    in
    match witness with
    | None ->
        Log.debug (fun l -> l "[%s] index is empty" (Filename.basename t.root));
        Thread.return `Completed
    | Some witness -> (
        match t.log with
        | None ->
            Log.debug (fun l ->
                l "[%s] log is empty" (Filename.basename t.root));
            Thread.return `Completed
        | Some log ->
            if
              force
              || Int63.compare (IO.offset log.io)
                   (Int63.of_int t.config.log_size)
                 > 0
            then merge' ~force ?hook ~witness t
            else Thread.return `Completed)

  let merge t = ignore (try_merge_aux ?hook:None ~force:true t : _ async)
  let try_merge t = ignore (try_merge_aux ?hook:None ~force:false t : _ async)

  let instance_is_merging t =
    (* [merge_lock] is used to detect an ongoing merge. Other operations can
       take this lock, but as they are not async, we consider this to be a good
       enough approximation. *)
    Semaphore.is_held t.merge_lock

  let is_merging t =
    let t = check_open t in
    if t.config.readonly then raise RO_not_allowed;
    instance_is_merging t

  (** {1 Replace} *)

  let replace' ?hook ?(overcommit = false) t key value =
    let t = check_open t in
    Stats.incr_nb_replace ();
    Log.debug (fun l ->
        l "[%s] replace %a %a" (Filename.basename t.root) pp_key key pp_value
          value);
    if t.config.readonly then raise RO_not_allowed;
    let log_limit_reached =
      Semaphore.with_acquire "replace" t.rename_lock (fun () ->
          let log =
            match t.log_async with Some log -> log | None -> Option.get t.log
          in
          Entry.encode' key value (IO.append log.io);
          Tbl.replace log.mem key value;
          Int63.compare (IO.offset log.io) (Int63.of_int t.config.log_size) > 0)
    in
    if log_limit_reached && not overcommit then
      let is_merging = instance_is_merging t in
      match (t.config.throttle, is_merging) with
      | `Overcommit_memory, true ->
          (* Do not start a merge, overcommit the memory instead. *)
          None
      | `Overcommit_memory, false | `Block_writes, _ ->
          (* Start a merge, blocking if one is already running. *)
          let hook = hook |> Option.map (fun f stage -> f (`Merge stage)) in
          Some (merge' ?hook ~witness:(Entry.v key value) t)
    else None

  let replace ?overcommit t key value =
    ignore (replace' ?hook:None ?overcommit t key value : _ async option)

  let replace_with_timer ?sampling_interval t key value =
    match sampling_interval with
    | None -> replace t key value
    | Some sampling_interval ->
        Stats.start_replace ();
        replace t key value;
        Stats.end_replace ~sampling_interval

  (** {1 Filter} *)

  (** [filter] is implemented with a [merge], during which bindings that do not
      satisfy the predicate are not merged. *)
  let filter t f =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] filter" (Filename.basename t.root));
    if t.config.readonly then raise RO_not_allowed;
    let witness =
      Semaphore.with_acquire "witness" t.rename_lock (fun () -> get_witness t)
    in
    match witness with
    | None ->
        Log.debug (fun l -> l "[%s] index is empty" (Filename.basename t.root))
    | Some witness -> (
        match Thread.await (merge' ~blocking:true ~filter:f ~witness t) with
        | Ok (`Aborted | `Completed) -> ()
        | Error (`Async_exn exn) ->
            Fmt.failwith "filter: asynchronous exception during merge (%s)"
              (Printexc.to_string exn))

  (** {1 Iter} *)

  let iter f t =
    let t = check_open t in
    Log.debug (fun l -> l "[%s] iter" (Filename.basename t.root));
    match t.log with
    | None -> ()
    | Some log ->
        Tbl.iter f log.mem;
        Option.iter
          (fun (i : index) -> iter_io (fun e -> f e.key e.value) i.io)
          t.index;
        Semaphore.with_acquire "iter" t.rename_lock (fun () ->
            match t.log_async with None -> () | Some log -> Tbl.iter f log.mem)

  (** {1 Close} *)

  let close' ~hook ?immediately it =
    let abort_merge = Option.is_some immediately in
    match !it with
    | None -> Log.debug (fun l -> l "close: instance already closed")
    | Some t ->
        Log.debug (fun l -> l "[%s] close" (Filename.basename t.root));
        if abort_merge then (
          t.pending_cancel <- true;
          hook `Abort_signalled);
        Semaphore.with_acquire "close" t.merge_lock (fun () ->
            (* The instance is set to [None] prior to closing the resources to
               ensure other clients see it as atomic. *)
            it := None;
            t.open_instances <- t.open_instances - 1;
            (* Resources are closed only if this is the last open instance. *)
            if t.open_instances = 0 then (
              Log.debug (fun l ->
                  l "[%s] last open instance: closing the file descriptor"
                    (Filename.basename t.root));
              if not t.config.readonly then flush_instance ~with_fsync:true t;
              Option.iter
                (fun l ->
                  Tbl.clear l.mem;
                  IO.close l.io)
                t.log;
              Option.iter (fun (i : index) -> IO.close i.io) t.index;
              Option.iter (fun lock -> IO.Lock.unlock lock) t.writer_lock))

  let close = close' ~hook:(fun _ -> ())

  module Checks = Checks.Make (K) (V) (Platform)
end

module Cache = Cache
module Checks = Checks
module Make = Make_private
module Stats = Stats

module Private = struct
  module Fan = Fan
  module Io = Io
  module Io_array = Io_array
  module Search = Search
  module Data = Data
  module Layout = Layout
  module Logs = Logs

  module Hook = struct
    type 'a t = 'a -> unit

    let v f = f
  end

  module type S = Private with type 'a hook := 'a Hook.t

  module Make = Make_private
end
