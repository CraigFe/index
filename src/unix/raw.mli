(** [Raw] wraps a file-descriptor with an file-format used internally by Index. *)

type t
(** The type of [raw] file handles. *)

val v : Unix.file_descr -> t
(** Construct a [raw] value from a file descriptor. *)

val unsafe_write : t -> off:int64 -> string -> unit

val unsafe_read : t -> off:int64 -> len:int -> bytes -> int

val fsync : t -> unit

val close : t -> unit

type raw = t

module Headers : sig
  type t = {
    offset : int64;  (** The length of the file containing valid data *)
    version : string;  (** Format version *)
    generation : int64;  (** Generation number *)
    fan_size : int64;  (** Length of the subsequent [fan] field *)
  }

  val get : raw -> t

  val set : raw -> t -> unit

  module Version : sig
    val get : raw -> string

    val set : raw -> string -> unit
  end

  module Offset : sig
    val get : raw -> int64

    val set : raw -> int64 -> unit
  end

  module Generation : sig
    val get : raw -> int64

    val set : raw -> int64 -> unit
  end

  module Fan : sig
    val get : raw -> string

    val set : raw -> string -> unit

    val get_size : raw -> int64

    val set_size : raw -> int64 -> unit
  end
end
