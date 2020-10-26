open Tezos_crypto

module Key = struct
  type t = Block_hash.t

  let t = Repr.partial ~pp:Block_hash.pp ()

  let equal = Block_hash.equal

  let hash = Hashtbl.hash

  let hash_size = 30

  let encode bh = Block_hash.to_string bh

  let encoded_size = Block_hash.size (* in bytes *)

  let decode str off =
    let str = String.sub str off encoded_size in
    Block_hash.of_string_exn str
end

module Value = struct
  type t = int32

  let t = Repr.int32

  let equal x y = Int32.equal x y

  let hash = Hashtbl.hash

  let hash_size = 30

  let encoded_size = 4

  let encode v =
    Bytes.unsafe_to_string Data_encoding.(Binary.to_bytes_exn int32 v)

  let decode str i =
    let str = Bytes.unsafe_of_string str in
    Data_encoding.(Binary.of_bytes_exn int32 (Bytes.sub str i encoded_size))
end

module Index = Index_unix.Make (Key) (Value) (Index.Cache.Noop)

let total_blocks = 300_000l

let live_major_bytes () = (Gc.stat ()).live_words * (Sys.word_size / 8)

let test ~trace ~log_size =
  let name = Format.sprintf "/tmp/index-%ld" (Random.int32 Int32.max_int) in
  let message = Format.sprintf "{ path = %s; log_size = %#d }" name log_size in
  let index = Index.v ~readonly:false ~log_size name in
  Progress.with_reporters
    (Progress_unix.counter ~message ~mode:`UTF8
       ~total:(Int64.of_int32 total_blocks)
       ())
    (fun report ->
      let create_block i =
        let b = Bytes.create 32 in
        Bytes.set_int32_be b 0 i;
        Block_hash.of_bytes_exn b
      in
      let rec loop i =
        (* store 1000 blocks then flush *)
        let target = Int32.add i 1000l in
        let rec inner j =
          if Int32.equal j target then Index.flush index
          else (
            Index.replace index (create_block j) j;
            inner (Int32.succ j))
        in
        inner i;
        Gc.major ();
        Format.fprintf trace "%d,%ld,%d\n%!" log_size i (live_major_bytes ());
        report 1000L;
        if target >= total_blocks then () else loop target
      in
      loop 0l);
  Index.close index;

  (* Format.eprintf "Index stats:\n\n%a%!"
   *   (Repr.pp_dump Index.Stats.t)
   *   (Index.Stats.get ()); *)
  ()

type 'a msgf = (('a, Format.formatter, unit, unit) format4 -> 'a) -> unit

let with_trace_file f =
  let oc = open_out "rusage.csv" in
  let ppf = Format.formatter_of_out_channel oc in
  f ppf;
  Format.pp_print_flush ppf ();
  close_out oc

let () =
  Random.self_init ();
  Memtrace.trace_if_requested ();
  Format.printf "Entry sizes: { key = %d; value = %d }\n%!" Key.encoded_size
    Value.encoded_size;
  with_trace_file @@ fun trace ->
  Format.fprintf trace "log_size,blocks,live_major_bytes\n";
  List.iter
    (fun log_size -> test ~trace ~log_size)
    [
      100;
      300;
      500;
      1_000;
      2_000;
      3_000;
      4_000;
      5_000;
      7_500;
      10_000;
      12_500;
      15_000;
      17_500;
      20_000;
      22_500;
      25_000;
      27_500;
      30_000;
    ]

let log_size = 100
