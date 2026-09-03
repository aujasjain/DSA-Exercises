type digit =
  | Zero | One | Two | Three | Four
  | Five | Six | Seven | Eight | Nine

type pos_nat =
  | Zero
  | Cons of digit * pos_nat

let rec canonicalise (n : pos_nat) : pos_nat =
  match n with
  | Zero -> Zero
  | Cons (d, xs) ->
      match canonicalise xs with
      | Zero when d = Zero -> Zero
      | ys -> Cons (d, ys)

let digit_plus (d1 : digit) (d2 : digit) : digit * digit =
  match d1, d2 with
  | (Zero, d) | (d, Zero) -> (d, Zero)
  | (One, One) -> (Two, Zero)
  | (One, Two) | (Two, One) -> (Three, Zero)
  | (One, Three) | (Three, One) -> (Four, Zero)
  | (One, Four) | (Four, One) -> (Five, Zero)
  | (One, Five) | (Five, One) -> (Six, Zero)
  | (One, Six) | (Six, One) -> (Seven, Zero)
  | (One, Seven) | (Seven, One) -> (Eight, Zero)
  | (One, Eight) | (Eight, One) -> (Nine, Zero)
  | (One, Nine) | (Nine, One) -> (Zero, One)
  | (Two, Two) -> (Four, Zero)
  | (Two, Three) | (Three, Two) -> (Five, Zero)
  | (Two, Four) | (Four, Two) -> (Six, Zero)
  | (Two, Five) | (Five, Two) -> (Seven, Zero)
  | (Two, Six) | (Six, Two) -> (Eight, Zero)
  | (Two, Seven) | (Seven, Two) -> (Nine, Zero)
  | (Two, Eight) | (Eight, Two) -> (Zero, One)
  | (Two, Nine) | (Nine, Two) -> (One, One)
  | (Three, Three) -> (Six, Zero)
  | (Three, Four) | (Four, Three) -> (Seven, Zero)
  | (Three, Five) | (Five, Three) -> (Eight, Zero)
  | (Three, Six) | (Six, Three) -> (Nine, Zero)
  | (Three, Seven) | (Seven, Three) -> (Zero, One)
  | (Three, Eight) | (Eight, Three) -> (One, One)
  | (Three, Nine) | (Nine, Three) -> (Two, One)
  | (Four, Four) -> (Eight, Zero)
  | (Four, Five) | (Five, Four) -> (Nine, Zero)
  | (Four, Six) | (Six, Four) -> (Zero, One)
  | (Four, Seven) | (Seven, Four) -> (One, One)
  | (Four, Eight) | (Eight, Four) -> (Two, One)
  | (Four, Nine) | (Nine, Four) -> (Three, One)
  | (Five, Five) -> (Zero, One)
  | (Five, Six) | (Six, Five) -> (One, One)
  | (Five, Seven) | (Seven, Five) -> (Two, One)
  | (Five, Eight) | (Eight, Five) -> (Three, One)
  | (Five, Nine) | (Nine, Five) -> (Four, One)
  | (Six, Six) -> (Two, One)
  | (Six, Seven) | (Seven, Six) -> (Three, One)
  | (Six, Eight) | (Eight, Six) -> (Four, One)
  | (Six, Nine) | (Nine, Six) -> (Five, One)
  | (Seven, Seven) -> (Four, One)
  | (Seven, Eight) | (Eight, Seven) -> (Five, One)
  | (Seven, Nine) | (Nine, Seven) -> (Six, One)
  | (Eight, Eight) -> (Six, One)
  | (Eight, Nine) | (Nine, Eight) -> (Seven, One)
  | (Nine, Nine) -> (Eight, One)

let threedigitplus (x : digit) (y : digit) (c : digit) : digit * digit =
  let (s1, c1) = digit_plus x y in
  let (s2, c2) = digit_plus s1 c in
  match c1, c2 with
  | Zero, Zero -> (s2, Zero)
  | Zero, One -> (s2, One)
  | One, Zero -> (s2, One)
  | One, One -> (s2, One)
  | _ -> (s2, One)

let rec pos_plus (n1 : pos_nat) (n2 : pos_nat) (c : digit) : pos_nat =
  match n1, n2, c with
  | Zero, Zero, Zero -> Zero

  | Zero, Zero, c ->
      Cons (c, Zero)

  | Zero, Cons (y, ys), c ->
      let (s, c') = digit_plus y c in
      Cons (s, pos_plus Zero ys c')

  | Cons (x, xs), Zero, c ->
      let (s, c') = digit_plus x c in
      Cons (s, pos_plus xs Zero c')

  | Cons (x, xs), Cons (y, ys), c ->
      let (s, c') = threedigitplus x y c in
      Cons (s, pos_plus xs ys c')

let plus (n1 : pos_nat) (n2 : pos_nat) : pos_nat =
  canonicalise (pos_plus n1 n2 Zero)

let x =
  plus
    (Cons (One, Cons (Two, Cons (Three, Zero))))
    (Cons (Four, Cons (Five, Cons (Six, Zero))))

let _ = x
