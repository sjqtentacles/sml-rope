structure Rope :> ROPE =
struct
  datatype rope = Leaf of string
                | Node of {left: rope, right: rope, size: int, depth: int}

  type t = rope

  val leafMax = 64

  val empty = Leaf ""

  fun size (Leaf s) = String.size s
    | size (Node {size, ...}) = size

  fun depth (Leaf _) = 0
    | depth (Node {depth, ...}) = depth

  (* Split a long string into a balanced tree of <= leafMax leaves. *)
  fun leavesOf s =
    let
      val n = String.size s
      fun build (lo, hi) =
        let val len = hi - lo
        in
          if len <= leafMax then Leaf (String.substring (s, lo, len))
          else
            let
              val mid = lo + len div 2
              val l = build (lo, mid)
              val r = build (mid, hi)
            in
              Node {left = l, right = r, size = len,
                    depth = 1 + Int.max (depth l, depth r)}
            end
        end
    in
      if n = 0 then empty else build (0, n)
    end

  fun fromString s = leavesOf s

  fun toString r =
    let
      fun go (Leaf s) acc = s :: acc
        | go (Node {left, right, ...}) acc = go left (go right acc)
    in
      String.concat (go r [])
    end

  (* Collect all leaf strings left-to-right. *)
  fun leafList r =
    let
      fun go (Leaf s) acc = if s = "" then acc else s :: acc
        | go (Node {left, right, ...}) acc = go left (go right acc)
    in
      go r []
    end

  (* Merge a non-empty list of leaves bottom-up into a balanced tree. *)
  fun balance r =
    let
      val leaves = List.map Leaf (leafList r)
      fun mkNode (l, rr) =
        Node {left = l, right = rr, size = size l + size rr,
              depth = 1 + Int.max (depth l, depth rr)}
      fun pairUp [] = []
        | pairUp [x] = [x]
        | pairUp (a :: b :: rest) = mkNode (a, b) :: pairUp rest
      fun loop [] = empty
        | loop [x] = x
        | loop xs = loop (pairUp xs)
    in
      loop leaves
    end

  fun log2 x = Math.ln x / Math.ln 2.0

  fun concat l r =
    let
      val sz = size l + size r
      val d = 1 + Int.max (depth l, depth r)
      val node = Node {left = l, right = r, size = sz, depth = d}
    in
      if sz = 0 then empty
      else if d > 1 andalso real d > 1.44 * log2 (real sz + 2.0)
      then balance node
      else node
    end

  fun split r i =
    let
      val n = size r
      val () = if i < 0 orelse i > n then raise Subscript else ()
      fun go (Leaf s) i =
            (Leaf (String.substring (s, 0, i)),
             Leaf (String.substring (s, i, String.size s - i)))
        | go (Node {left, right, ...}) i =
            let val ls = size left
            in
              if i < ls then
                let val (ll, lr) = go left i
                in (ll, concat lr right) end
              else if i > ls then
                let val (rl, rr) = go right (i - ls)
                in (concat left rl, rr) end
              else (left, right)
            end
    in
      go r i
    end

  fun sub r i =
    let
      val () = if i < 0 orelse i >= size r then raise Subscript else ()
      fun go (Leaf s) i = String.sub (s, i)
        | go (Node {left, right, ...}) i =
            let val ls = size left
            in if i < ls then go left i else go right (i - ls) end
    in
      go r i
    end

  fun sub' r (start, len) =
    let
      val n = size r
      val () = if start < 0 orelse len < 0 orelse start + len > n
               then raise Subscript else ()
      val (_, rest) = split r start
      val (mid, _) = split rest len
    in
      toString mid
    end
end
