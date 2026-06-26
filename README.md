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

(* Editing: insert / delete / replace by index/range. *)
val a = insert  (fromString "foobar") 3 "XYZ"   (* "fooXYZbar" *)
val b = delete  (fromString "foobar") (1, 2)    (* "fbar"      *)
val d = replace (fromString "foobar") (0, 3) "B"(* "Bbar"      *)

(* Convenience builders. *)
val j  = concatAll [fromString "a", fromString "b", fromString "c"]  (* "abc" *)
val ap = appendString (fromString "hi") " there"                     (* "hi there" *)

(* Iterate / transform without flattening to a string first. *)
val nchars = foldl (fn (_, n) => n + 1) 0 r       (* 6 *)
val up     = map Char.toUpper r                   (* "FOOBAR" *)
val ()     = app (fn c => print (String.str c)) r (* foobar *)

(* Line helpers (split on '\n'). *)
val nlines = lineCount (fromString "a\nb\nc")     (* 3 *)
val line1  = lineAt    (fromString "a\nb\nc") 1   (* "b" *)

(* Rebalance after many concatenations; depth stays logarithmic. *)
fun build i acc =
  if i >= 100 then acc
  else build (i + 1) (concat acc (fromString "0123456789"))
val big = build 0 empty
val () = print (Int.toString (size big) ^ " chars, depth "
                ^ Int.toString (depth (balance big)) ^ "\n")
```

## API

| Function | Description |
| --- | --- |
| `empty`, `fromString`, `toString` | Construction and flattening. |
| `size`, `sub`, `sub'`, `split` | Length, indexed char, substring `(start,len)`, split at index. |
| `concat`, `concatAll`, `appendString` | Concatenation (O(log n)), join a list, append a string. |
| `insert`, `delete`, `replace` | In-place-style edits returning a new rope (persistent). |
| `app`, `foldl`, `foldr`, `map` | Iterate / fold / transform characters. |
| `lineCount`, `lineAt` | Newline-separated line count and 0-indexed line access. |
| `balance`, `depth` | Explicit Fibonacci rebalance and current tree depth. |

## Scope and limitations

- The structure is **persistent** (immutable): every edit returns a new rope and
  shares unchanged subtrees; the original is unaffected.
- `insert`/`delete`/`replace` are defined via `split`/`concat`. `concat` already
  applies a depth-vs-size rebalance heuristic, so edited ropes stay logarithmic
  in depth without an explicit `balance` call (verified by a randomized
  400-edit oracle against a flat string). Call `balance` for a guaranteed
  Fibonacci-balanced tree.
- Indices are character (byte) offsets into the raw string; there is no
  UTF-8/grapheme awareness. `lineAt`/`lineCount` split on the single byte `'\n'`
  and a lone trailing newline does not add an empty final line.
- All range-checked operations (`split`, `sub`, `sub'`, `insert`, `delete`,
  `replace`, `lineAt`) raise `Subscript` on out-of-range arguments.
- Single-threaded; no concurrency concerns since values are immutable.

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
