(* demo.sml - build, edit, and query a rope (efficient string
   concatenation/splitting/indexing/line access). Deterministic: identical
   output on every run and both compilers. *)

structure Ro = Rope

val r1 = Ro.fromString "The quick brown fox"
val r2 = Ro.fromString " jumps over the lazy dog."
val r  = Ro.concat r1 r2

val () = print ("size            = " ^ Int.toString (Ro.size r) ^ "\n")
val () = print ("toString        = \"" ^ Ro.toString r ^ "\"\n")
val () = print ("sub 4           = '" ^ String.str (Ro.sub r 4) ^ "'\n")
val () = print ("sub' (4,5)      = \"" ^ Ro.sub' r (4, 5) ^ "\"\n")

val (front, back) = Ro.split r 20
val () = print ("split @20       = \"" ^ Ro.toString front ^ "\" / \""
                ^ Ro.toString back ^ "\"\n")

val edited = Ro.replace r (4, 6) "slow, "
val () = print ("replace(4,6)    = \"" ^ Ro.toString edited ^ "\"\n")

val inserted = Ro.insert edited 0 ">> "
val () = print ("insert@0        = \"" ^ Ro.toString inserted ^ "\"\n")

val deleted = Ro.delete inserted (0, 3)
val () = print ("delete(0,3)     = \"" ^ Ro.toString deleted ^ "\"\n")

val upper = Ro.map Char.toUpper r
val () = print ("map toUpper     = \"" ^ Ro.toString upper ^ "\"\n")

val spaces = Ro.foldl (fn (c, n) => if c = #" " then n + 1 else n) 0 r
val () = print ("foldl spaces    = " ^ Int.toString spaces ^ "\n")

val ml = Ro.fromString "alpha\nbeta\ngamma\n"
val () = print ("lineCount       = " ^ Int.toString (Ro.lineCount ml) ^ "\n")
val () = print ("lineAt 0,1,2    = \"" ^ Ro.lineAt ml 0 ^ "\", \""
                ^ Ro.lineAt ml 1 ^ "\", \"" ^ Ro.lineAt ml 2 ^ "\"\n")

val big = Ro.concatAll (List.tabulate (5, fn _ => r))
val () = print ("concatAll depth = " ^ Int.toString (Ro.depth big) ^ "\n")
val () = print ("balance depth   = " ^ Int.toString (Ro.depth (Ro.balance big)) ^ "\n")
