(** Extracted from [src/dune_stats/dune_stats.ml]. *)

module Event = Chrome_trace.Event

module Json = struct
  include Chrome_trace.Json

  let quote_string_to_buf s buf =
    (* TODO: escaping is wrong here, in particular for control characters *)
    Buffer.add_string buf (Fmt.str "%S" s)

  let rec to_buf t buf =
    match t with
    | `String s -> quote_string_to_buf s buf
    | `Int i -> Buffer.add_string buf (string_of_int i)
    | `Float f -> Buffer.add_string buf (Fmt.str "%.17g" f)
    | `Bool b -> Buffer.add_string buf (string_of_bool b)
    | `List l ->
        Buffer.add_char buf '[';
        array_body_to_buf l buf;
        Buffer.add_char buf ']'
    | `Assoc o ->
        Buffer.add_char buf '{';
        object_body_to_buf o buf;
        Buffer.add_char buf '}'

  and array_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ x ] -> to_buf x buf
    | x :: xs ->
        to_buf x buf;
        Buffer.add_char buf ',';
        array_body_to_buf xs buf

  and object_body_to_buf t buf =
    match t with
    | [] -> ()
    | [ (x, y) ] ->
        quote_string_to_buf x buf;
        Buffer.add_char buf ':';
        to_buf y buf
    | (x, y) :: xs ->
        quote_string_to_buf x buf;
        Buffer.add_char buf ':';
        to_buf y buf;
        Buffer.add_char buf ',';
        object_body_to_buf xs buf

  let to_string t =
    let buf = Buffer.create 0 in
    to_buf t buf;
    Buffer.contents buf
end

type dst =
  | Out of out_channel
  | Custom of { write : string -> unit; close : unit -> unit }

type state = {
  print : string -> unit;
  close : unit -> unit;
  buffer : Buffer.t;
  mutable after_first_event : bool;
}

type t = Inactive | Active of state

let noop = Inactive

let create dst =
  let print =
    match dst with Out out -> Stdlib.output_string out | Custom c -> c.write
  in
  let close =
    match dst with
    | Out out -> fun () -> Stdlib.close_out out
    | Custom c -> c.close
  in
  let buffer = Buffer.create 1024 in
  Active { print; close; after_first_event = false; buffer }

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
      t.after_first_event <- true;
      '['

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c

let emit t event =
  match t with
  | Inactive -> ()
  | Active t -> printf t "%s" (Json.to_string (Event.to_json event))

let close = function
  | Inactive -> ()
  | Active { print; close; _ } ->
      print "]\n";
      close ()
