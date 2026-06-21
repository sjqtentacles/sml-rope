signature ROPE =
sig
  type t
  val empty      : t
  val fromString : string -> t
  val toString   : t -> string
  val size   : t -> int
  val concat : t -> t -> t
  val split  : t -> int -> t * t      (* raises Subscript on out-of-range *)
  val sub    : t -> int -> char
  val sub'   : t -> int * int -> string   (* substring (start, len) *)
  val balance  : t -> t
  val depth    : t -> int
end
