(* test.sml *)
structure Tests =
struct
  open Rope

  (* Helper: build a flat reference string of length n with varied chars. *)
  fun mkString n =
    CharVector.tabulate (n, fn i => Char.chr (Char.ord #"a" + (i mod 26)))

  fun run () =
    let
      val () = Harness.reset ()

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "construction and toString"
      val () = Harness.checkString "fromString/toString roundtrip"
                 ("hello", toString (fromString "hello"))
      val () = Harness.checkString "empty toString" ("", toString empty)
      val () = Harness.checkInt "empty size" (0, size empty)
      val () = Harness.checkInt "size of hello" (5, size (fromString "hello"))
      val () = Harness.checkString "long roundtrip"
                 (mkString 1000, toString (fromString (mkString 1000)))

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "concat"
      val () = Harness.checkString "concat foo bar"
                 ("foobar", toString (concat (fromString "foo") (fromString "bar")))
      val () = Harness.checkInt "concat size"
                 (6, size (concat (fromString "foo") (fromString "bar")))
      val () = Harness.checkString "concat with empty left"
                 ("bar", toString (concat empty (fromString "bar")))
      val () = Harness.checkString "concat with empty right"
                 ("foo", toString (concat (fromString "foo") empty))

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "split"
      val (l, r) = split (fromString "foobar") 3
      val () = Harness.checkString "split left" ("foo", toString l)
      val () = Harness.checkString "split right" ("bar", toString r)
      val (l0, r0) = split (fromString "foobar") 0
      val () = Harness.checkString "split at 0 left" ("", toString l0)
      val () = Harness.checkString "split at 0 right" ("foobar", toString r0)
      val (le, re) = split (fromString "foobar") 6
      val () = Harness.checkString "split at end left" ("foobar", toString le)
      val () = Harness.checkString "split at end right" ("", toString re)
      val sref = mkString 500
      val (sl, sr) = split (fromString sref) 137
      val () = Harness.checkString "split big left"
                 (String.substring (sref, 0, 137), toString sl)
      val () = Harness.checkString "split big right"
                 (String.substring (sref, 137, 500 - 137), toString sr)

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "large rope sub and size"
      (* Build a 10000-char rope from 1000 ten-char leaves. *)
      val flat = mkString 10000
      fun tenChars i = String.substring (flat, i * 10, 10)
      fun buildRope i acc =
        if i >= 1000 then acc
        else buildRope (i + 1) (concat acc (fromString (tenChars i)))
      val big = buildRope 0 empty
      val () = Harness.checkInt "big size" (10000, size big)
      val () = Harness.checkString "big toString" (flat, toString big)
      val subOk =
        let
          fun chk i = sub big i = String.sub (flat, i)
          val idxs = [0, 1, 9, 10, 99, 100, 500, 1234, 5000, 7777, 9998, 9999]
        in List.all chk idxs end
      val () = Harness.checkBool "big sub matches flat" (true, subOk)

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "balance and depth"
      (* 100 concats produce a balanced-enough rope: depth <= 30. *)
      fun build100 i acc =
        if i >= 100 then acc
        else build100 (i + 1) (concat acc (fromString "0123456789"))
      val r100 = build100 0 empty
      val () = Harness.checkInt "100-concat size" (1000, size r100)
      val () = Harness.checkBool "100-concat depth <= 30" (true, depth r100 <= 30)
      val balanced = balance big
      val () = Harness.checkString "balance preserves content" (flat, toString balanced)
      val () = Harness.checkInt "balance preserves size" (10000, size balanced)

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "sub' substring and errors"
      val () = Harness.checkString "sub' middle"
                 (String.substring (sref, 100, 50), sub' (fromString sref) (100, 50))
      val () = Harness.checkString "sub' from start"
                 (String.substring (sref, 0, 10), sub' (fromString sref) (0, 10))
      val () = Harness.checkString "sub' to end"
                 (String.substring (sref, 490, 10), sub' (fromString sref) (490, 10))
      val () = Harness.checkRaises "split ~1 raises"
                 (fn () => split (fromString "foobar") ~1)
      val () = Harness.checkRaises "split size+1 raises"
                 (fn () => split (fromString "foobar") 7)
      val () = Harness.checkRaises "sub negative raises"
                 (fn () => sub (fromString "foobar") ~1)
      val () = Harness.checkRaises "sub out of range raises"
                 (fn () => sub (fromString "foobar") 6)
    in
      Harness.run ()
    end
end
