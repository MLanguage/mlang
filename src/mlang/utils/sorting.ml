let mergeSort cmp a =
  let merge cmp a iLeft iRight iEnd b =
    let rec aux i j k =
      if k < iEnd then
        if i < iRight && (j >= iEnd || cmp a.(i) a.(j)) then (
          b.(k) <- a.(i);
          aux (i + 1) j (k + 1))
        else (
          b.(k) <- a.(j);
          aux i (j + 1) (k + 1))
    in
    aux iLeft iRight iLeft
  in
  let b = Array.copy a in
  let n = Array.length a in
  let rec aux a b cp width =
    if width < n then (
      let rec aux' i =
        if i < n then (
          merge cmp a i (min (i + width) n) (min (i + (2 * width)) n) b;
          aux' (i + (2 * width)))
      in
      aux' 0;
      aux b a (not cp) (2 * width))
    else if cp then Array.blit a 0 b 0 n
  in
  aux a b false 1
