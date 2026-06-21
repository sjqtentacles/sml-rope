# sml-rope

Persistent rope data structure with Fibonacci rebalancing in pure Standard ML

## Installation

```
smlpkg add github.com/sjqtentacles/sml-rope
smlpkg sync
```

## Usage

```sml
open Rope

(* Build from strings and concatenate in O(log n). *)
val r = concat (fromString "foo") (fromString "bar")
val () = print (toString r ^ "\n")          (* foobar *)

(* Split into two ropes at an index. *)
val (l, right) = split (fromString "foobar") 3
val () = print (toString l ^ "|" ^ toString right ^ "\n")  (* foo|bar *)

(* Indexed access and substring. *)
val c = sub (fromString "foobar") 3          (* #"b" *)
val s = sub' (fromString "foobar") (1, 3)    (* "oob" *)

(* Rebalance after many concatenations; depth stays logarithmic. *)
fun build i acc =
  if i >= 100 then acc
  else build (i + 1) (concat acc (fromString "0123456789"))
val big = build 0 empty
val () = print (Int.toString (size big) ^ " chars, depth "
                ^ Int.toString (depth (balance big)) ^ "\n")
```

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
