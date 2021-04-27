open! Import
module Stats = Index.Stats

let ( ++ ) = Int63.add

type t = {
  fd : Unix.file_descr;
  uring : request Uring.t;
  mutable active_submissions : int;
}

(* Context about ongoing operations with the Uring. Reads and writes may be
   partial; we need to be able to re-queue them if so. *)
and request = {
  opcode : [ `read | `write ];
  fd_off : Int63.t;
  buf_off_initial : int;
  len : int;
  mutable buf : bytes;
  mutable buf_off : int;
  mutable remaining : int;
  parent : t;
}

module Request = struct
  type t = request

  let pp =
    let int_of_file_descr : Unix.file_descr -> int = Obj.magic in
    let open Fmt.Dump in
    let pp_opcode =
      Fmt.of_to_string (function `read -> "`read" | `write -> "`write")
    in
    record
      [
        field "opcode" (fun t -> t.opcode) pp_opcode;
        field "fd" (fun t -> int_of_file_descr t.parent.fd) Fmt.int;
        field "fd_off" (fun t -> t.fd_off) Int63.pp;
        (* field "buf"
         *   (fun t -> t.buf)
         *   (fun ppf x -> Fmt.pf ppf "%S" (Bytes.unsafe_to_string x)); *)
        field "buf_off" (fun t -> t.buf_off) Fmt.int;
        field "buf_off_initial" (fun t -> t.buf_off_initial) Fmt.int;
        field "len" (fun t -> t.len) Fmt.int;
        field "remaining" (fun t -> t.remaining) Fmt.int;
      ]

  let advance t n =
    t.buf_off <- t.buf_off + n;
    t.remaining <- t.remaining - n;
    if t.len < 0 then assert false

  let enqueue ({ opcode; fd_off; buf; buf_off; remaining; parent; _ } as t)
      uring =
    (* Logs.debug (fun f -> f "Enqueue"); *)
    t.parent.active_submissions <- succ t.parent.active_submissions;
    match opcode with
    | `read ->
        Uring.read uring ~fd:parent.fd ~fd_off ~buf ~buf_off ~len:remaining t
    | `write ->
        Uring.write uring ~fd:parent.fd ~fd_off
          ~buf:(Bytes.unsafe_to_string buf)
          ~buf_off ~len:remaining t

  (* TODO: avoid the need to construct one of these. *)
  (* let dummy : t =
   *   {
   *     opcode = `read;
   *     buf_off_initial = -1;
   *     fd_off = Int63.minus_one;
   *     buf = Bytes.create 0;
   *     buf_off = -1;
   *     len = -1;
   *     remaining = -1;
   *     parent = Obj.magic ();
   *   } *)
end

let v fd =
  let uring = Uring.create ~fixed_buf_len:0 ~queue_depth:64 () in
  { fd; uring; active_submissions = 0 }

(* TODO compile time check *)
let eagain = -11

let eintr = -4

let handle_completion =
  let requeue t req =
    let success = Request.enqueue req t.uring in
    assert success;
    None
  in
  fun t ((req : Request.t), res) ->
    Logs.debug (fun f ->
        f "Handling completion of request:@, %a, res = %d" Request.pp req res);
    t.active_submissions <- pred t.active_submissions;
    match res with
    | 0 ->
        (* EOF *)
        Some (match req.opcode with `read -> `read req.buf | `write -> `write)
    | n when n = eagain || n = eintr -> requeue t req
    | n when n < 0 -> Fmt.failwith "unix errorno %d" n
    | n when n < req.remaining (* short read / write *) ->
        Request.advance req n;
        requeue t req
    | n when n = req.remaining ->
        (* Logs.debug (fun f -> f "Done"); *)
        Some (match req.opcode with `read -> `read req.buf | `write -> `write)
    | n -> Fmt.failwith "unexpected: %d (req = %a)" n Request.pp req

let submit_until_read t =
  let submitted = Uring.submit t.uring in
  assert (submitted > 0);

  let rec aux read_result =
    if t.active_submissions = 0 then read_result
    else (
      Logs.debug (fun f -> f "Waiting");
      (if Option.is_some read_result then Uring.peek t.uring
      else Uring.wait t.uring)
      |> function
      | None -> read_result
      | Some item -> (
          match (handle_completion t item, read_result) with
          | None, None ->
              Logs.debug (fun f -> f "Submitting");
              ignore (Uring.submit t.uring : int);
              aux read_result
          | (None | Some `write), _ -> aux read_result
          | (Some (`read _) as read_result), None -> aux read_result
          | Some (`read _), Some _prev ->
              Fmt.failwith
                "Asynchronous read completion. Reads should be flushed on \
                 entry."))
  in
  aux None

(* TODO: avoid duplication with the above *)
let submit_all t =
  ignore (Uring.submit t.uring : int);
  let rec aux () =
    if t.active_submissions > 0 then
      Uring.wait t.uring |> function
      | None -> ()
      | Some item ->
          let (_ : _ option) = handle_completion t item in
          aux ()
  in
  aux ();
  assert (t.active_submissions = 0)

let flush t = submit_all t

let fsync t =
  (* Uring.fsync t.fd; TODO *)
  flush t

let close t =
  flush t;
  Unix.close t.fd;
  Uring.exit t.uring

let fstat t = Unix.fstat t.fd

let unsafe_write t ~off:fd_off buf =
  let buf_off = 0 and buf_off_initial = 0 and len = String.length buf in
  let (req : Request.t) =
    {
      opcode = `write;
      fd_off;
      buf = Bytes.unsafe_of_string buf;
      buf_off;
      buf_off_initial;
      len;
      remaining = len;
      parent = t;
    }
  in
  let success = Request.enqueue req t.uring in
  assert success

let unsafe_read t ~off:fd_off ~len buf =
  (* if len > 100000 then assert false; *)
  let buf_off = 0 and buf_off_initial = 0 in
  let (req : Request.t) =
    {
      opcode = `read;
      fd_off;
      buf;
      buf_off;
      buf_off_initial;
      len;
      remaining = len;
      parent = t;
    }
  in
  let () = submit_all t in
  let success = Request.enqueue req t.uring in
  Logs.debug (fun f -> f "Enqueueing a read");
  assert success;
  let (`read _) = Option.get (submit_until_read t) in
  ()

let encode_int63 n =
  let buf = Bytes.create Int63.encoded_size in
  Int63.encode buf ~off:0 n;
  Bytes.unsafe_to_string buf

let decode_int63 buf = Int63.decode ~off:0 buf

exception Not_written

module Offset = struct
  let off = Int63.zero

  let set t n = unsafe_write t ~off (encode_int63 n)

  let get t =
    let len = 8 in
    let buf = Bytes.create len in
    unsafe_read t ~off ~len buf;
    decode_int63 (Bytes.unsafe_to_string buf)
end

module Version = struct
  let off = Int63.of_int 8

  let get t =
    let len = 8 in
    let buf = Bytes.create len in
    unsafe_read t ~off ~len buf;
    Bytes.unsafe_to_string buf

  let set t v = unsafe_write t ~off v
end

module Generation = struct
  let off = Int63.of_int 16

  let get t =
    let len = 8 in
    let buf = Bytes.create len in
    unsafe_read t ~off ~len buf;
    decode_int63 (Bytes.unsafe_to_string buf)

  let set t gen = unsafe_write t ~off (encode_int63 gen)
end

module Fan = struct
  let off = Int63.of_int 24

  let set t buf =
    let size = encode_int63 (Int63.of_int (String.length buf)) in
    unsafe_write t ~off size;
    if buf <> "" then unsafe_write t ~off:(off ++ Int63.of_int 8) buf

  let get_size t =
    let len = 8 in
    let size_buf = Bytes.create len in
    unsafe_read t ~off ~len size_buf;
    decode_int63 (Bytes.unsafe_to_string size_buf)

  let set_size t size =
    let buf = encode_int63 size in
    unsafe_write t ~off buf

  let get t =
    let size = Int63.to_int (get_size t) in
    Logs.debug (fun f -> f "Getting fanout of size: %d" size);
    let buf = Bytes.create size in
    unsafe_read t ~off:(off ++ Int63.of_int 8) ~len:size buf;
    Bytes.unsafe_to_string buf
end

module Header = struct
  type t = { offset : int63; version : string; generation : int63 }

  (** NOTE: These functions must be equivalent to calling the above [set] /
      [get] functions individually. *)

  let total_header_length = 8 + 8 + 8

  let read_word buf off =
    let result = Bytes.create 8 in
    Bytes.blit buf off result 0 8;
    Bytes.unsafe_to_string result

  let get t =
    let header = Bytes.create total_header_length in
    unsafe_read t ~off:Int63.zero ~len:total_header_length header;
    let offset = read_word header 0 |> decode_int63 in
    let version = read_word header 8 in
    let generation = read_word header 16 |> decode_int63 in
    { offset; version; generation }

  let set t { offset; version; generation } =
    assert (String.length version = 8);
    let b = Bytes.create total_header_length in
    Bytes.blit_string (encode_int63 offset) 0 b 0 8;
    Bytes.blit_string version 0 b 8 8;
    Bytes.blit_string (encode_int63 generation) 0 b 16 8;
    unsafe_write t ~off:Int63.zero (Bytes.unsafe_to_string b)
end
