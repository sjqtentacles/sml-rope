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

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "concatAll and appendString"
      val () = Harness.checkString "concatAll"
                 ("abcdef",
                  toString (concatAll [fromString "ab", fromString "cd", fromString "ef"]))
      val () = Harness.checkString "concatAll empty" ("", toString (concatAll []))
      val () = Harness.checkString "concatAll singleton"
                 ("x", toString (concatAll [fromString "x"]))
      val () = Harness.checkString "appendString"
                 ("hello world", toString (appendString (fromString "hello") " world"))

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "insert/delete/replace"
      val () = Harness.checkString "insert middle"
                 ("foXXobar", toString (insert (fromString "foobar") 2 "XX"))
      val () = Harness.checkString "insert at 0"
                 ("XXfoobar", toString (insert (fromString "foobar") 0 "XX"))
      val () = Harness.checkString "insert at end"
                 ("foobarXX", toString (insert (fromString "foobar") 6 "XX"))
      val () = Harness.checkString "delete middle"
                 ("fobar", toString (delete (fromString "foobar") (2, 1)))
      val () = Harness.checkString "delete prefix"
                 ("bar", toString (delete (fromString "foobar") (0, 3)))
      val () = Harness.checkString "delete to end"
                 ("foo", toString (delete (fromString "foobar") (3, 3)))
      val () = Harness.checkString "replace middle"
                 ("foZZZar", toString (replace (fromString "foobar") (2, 2) "ZZZ"))
      val () = Harness.checkString "replace with empty (=delete)"
                 ("foar", toString (replace (fromString "foobar") (2, 2) ""))
      val () = Harness.checkRaises "insert out of range raises"
                 (fn () => insert (fromString "foo") 4 "x")
      val () = Harness.checkRaises "delete out of range raises"
                 (fn () => delete (fromString "foo") (2, 5))
      val () = Harness.checkRaises "replace negative raises"
                 (fn () => replace (fromString "foo") (~1, 1) "x")

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "iteration: app/foldl/foldr/map"
      val () = Harness.checkInt "foldl counts chars"
                 (5, foldl (fn (_, n) => n + 1) 0 (fromString "hello"))
      val () = Harness.checkString "foldl builds reversed"
                 ("olleh", foldl (fn (c, acc) => String.str c ^ acc) "" (fromString "hello"))
      val () = Harness.checkString "foldr builds in order"
                 ("hello", foldr (fn (c, acc) => String.str c ^ acc) "" (fromString "hello"))
      val () = Harness.checkString "map uppercases"
                 ("HELLO", toString (map Char.toUpper (fromString "hello")))
      val () = Harness.checkString "map preserves content over big rope"
                 (String.map Char.toUpper (mkString 1000),
                  toString (map Char.toUpper (fromString (mkString 1000))))
      val appAcc = ref []
      val () = app (fn c => appAcc := c :: !appAcc) (fromString "abc")
      val () = Harness.checkString "app visits in order"
                 ("cba", String.implode (!appAcc))

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "line helpers"
      val () = Harness.checkInt "lineCount empty" (0, lineCount empty)
      val () = Harness.checkInt "lineCount single" (1, lineCount (fromString "abc"))
      val () = Harness.checkInt "lineCount three"
                 (3, lineCount (fromString "a\nb\nc"))
      val () = Harness.checkInt "lineCount trailing newline"
                 (2, lineCount (fromString "a\nb\n"))
      val () = Harness.checkString "lineAt 0" ("a", lineAt (fromString "a\nb\nc") 0)
      val () = Harness.checkString "lineAt 1" ("b", lineAt (fromString "a\nb\nc") 1)
      val () = Harness.checkString "lineAt 2" ("c", lineAt (fromString "a\nb\nc") 2)
      val () = Harness.checkString "lineAt with trailing newline"
                 ("b", lineAt (fromString "a\nb\n") 1)
      val () = Harness.checkRaises "lineAt out of range raises"
                 (fn () => lineAt (fromString "a\nb") 5)

      (* ---------------------------------------------------------------- *)
      val () = Harness.section "randomized edit-sequence oracle vs string"
      (* A tiny deterministic LCG so the test is reproducible across runs. *)
      val seed = ref 0w1234567
      fun nextRand () =
        ( seed := Word.andb (!seed * 0w1103515245 + 0w12345, 0wx7FFFFFFF)
        ; Word.toInt (!seed) )
      fun randInt bound = if bound <= 0 then 0 else nextRand () mod bound
      val alphabet = "abcdefgh"
      fun randStr len =
        String.implode (List.tabulate (len, fn _ =>
          String.sub (alphabet, randInt (String.size alphabet))))

      (* Apply N random edits to both a rope and a reference string, asserting
         agreement after every step, plus a depth bound to catch blowup. *)
      val oracleOk = ref true
      val depthOk = ref true
      fun applyEdits 0 _ _ = ()
        | applyEdits k rope str =
            let
              val n = String.size str
              val op_ = randInt 3
              val (rope', str') =
                case op_ of
                    0 => (* insert *)
                      let
                        val i = randInt (n + 1)
                        val s = randStr (1 + randInt 4)
                      in
                        (insert rope i s,
                         String.substring (str, 0, i) ^ s
                           ^ String.substring (str, i, n - i))
                      end
                  | 1 => (* delete *)
                      if n = 0 then (rope, str)
                      else
                        let
                          val i = randInt n
                          val len = 1 + randInt (n - i)
                        in
                          (delete rope (i, len),
                           String.substring (str, 0, i)
                             ^ String.substring (str, i + len, n - i - len))
                        end
                  | _ => (* replace *)
                      if n = 0 then
                        let val s = randStr (1 + randInt 4)
                        in (insert rope 0 s, s) end
                      else
                        let
                          val i = randInt n
                          val len = 1 + randInt (n - i)
                          val s = randStr (randInt 4)
                        in
                          (replace rope (i, len) s,
                           String.substring (str, 0, i) ^ s
                             ^ String.substring (str, i + len, n - i - len))
                        end
              val () = if toString rope' <> str' then oracleOk := false else ()
              (* depth bound: log-ish in size; allow generous slack *)
              val sz = size rope'
              val bound = 64 + 4 * (if sz <= 1 then 1
                                    else let fun lg n a = if n <= 1 then a else lg (n div 2) (a+1)
                                         in lg sz 0 end)
              val () = if depth rope' > bound then depthOk := false else ()
            in
              applyEdits (k - 1) rope' str'
            end
      val () = applyEdits 400 (fromString "seedtext") "seedtext"
      val () = Harness.checkBool "oracle: rope tracks string over 400 edits"
                 (true, !oracleOk)
      val () = Harness.checkBool "oracle: depth stays bounded" (true, !depthOk)
    in
      Harness.run ()
    end
end
