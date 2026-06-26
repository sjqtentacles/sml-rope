signature ROPE =
sig
  type t
  val empty      : t
  val fromString : string -> t
  val toString   : t -> string
  val size   : t -> int
  val concat : t -> t -> t
  val concatAll : t list -> t
  val appendString : t -> string -> t
  val split  : t -> int -> t * t      (* raises Subscript on out-of-range *)
  val sub    : t -> int -> char
  val sub'   : t -> int * int -> string   (* substring (start, len) *)

  (* --- editing (raise Subscript on out-of-range index/range) --- *)
  val insert  : t -> int -> string -> t        (* insert at index *)
  val delete  : t -> int * int -> t            (* remove (start, len) *)
  val replace : t -> int * int -> string -> t  (* delete (start,len) then insert *)

  (* --- iteration / transformation --- *)
  val app   : (char -> unit) -> t -> unit
  val foldl : (char * 'b -> 'b) -> 'b -> t -> 'b
  val foldr : (char * 'b -> 'b) -> 'b -> t -> 'b
  val map   : (char -> char) -> t -> t

  (* --- line helpers (lines split on '\n'; a trailing '\n' does not add an
         empty final line, matching String.fields-style counting) --- *)
  val lineCount : t -> int                 (* number of newline-separated lines *)
  val lineAt    : t -> int -> string       (* 0-indexed line, no '\n'; raises Subscript *)

  val balance  : t -> t
  val depth    : t -> int
end

