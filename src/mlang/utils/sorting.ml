let mergeSort cmp aBeg aEnd a =
  let n = Array.length a in
  let b = Array.copy a in
  let aBeg = max 0 (min aBeg n) in
  let aEnd = max 0 (min aEnd n) in
  let aBeg, aEnd = (min aBeg aEnd, max aBeg aEnd) in
  let merge a iLeft iRight iEnd b =
    let rec aux i j k =
      if k < iEnd then
        if i < iRight && (j >= iEnd || cmp i a.(i) j a.(j)) then (
          b.(k) <- a.(i);
          aux (i + 1) j (k + 1))
        else (
          b.(k) <- a.(j);
          aux i (j + 1) (k + 1))
    in
    aux iLeft iRight iLeft
  in
  let rec aux a b width =
    if width < aEnd then (
      let rec aux' i =
        if i < aEnd then (
          merge a i (min (i + width) aEnd) (min (i + (2 * width)) aEnd) b;
          aux' (i + (2 * width)))
      in
      aux' aBeg;
      Array.blit b aBeg a aBeg (aEnd - aBeg);
      aux a b (2 * width))
  in
  aux a b 1
