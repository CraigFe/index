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

module Headers = struct
  type t = { offset : int64; generation : int64 }

  let pp ppf { offset; generation } =
    Format.fprintf ppf "{ offset = %Ld; generation = %Ld }" offset generation
end

module type S = sig
  type t

  val v :
    readonly:bool ->
    fresh:bool ->
    generation:int64 ->
    fan_size:int64 ->
    string ->
    t

  val name : t -> string

  val version : t -> string

  val offset : t -> int64
  (** [offset t] is the total size of valid data stored by [t] in bytes. {b
      NOTE:} this does not observe any entries added concurrently via other
      instances (this is achieved by {!read_headers}). *)

  val readonly : t -> bool

  val read : t -> off:int64 -> len:int -> bytes -> int

  val clear : ?keep_generation:bool -> t -> unit

  val sync : ?with_fsync:bool -> t -> unit

  val read_headers : t -> Headers.t

  val set_generation : t -> int64 -> unit

  val set_fanout : t -> string -> unit

  val get_fanout : t -> string

  val rename : src:t -> dst:t -> unit

  val append : t -> string -> unit

  val close : t -> unit

  type lock

  val lock : string -> lock

  val unlock : lock -> unit
end
