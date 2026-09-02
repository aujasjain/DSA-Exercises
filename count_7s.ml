(* Problem: How many 7s exist in the factorial of 6400? *)

type myint = int list

let mult_myint num x =
  let rec multiply_digits digits carry =
    match digits with
    | [] ->
        if carry = 0 then []
        else
          let digit = carry mod 10 in
            digit :: multiply_digits [] (carry / 10)

    | d :: rest ->
        let value = d * x + carry in
          let digit = value mod 10 in
            digit :: multiply_digits rest (value / 10) in 
              multiply_digits num 0

let myint_to_string num =
  let chars = List.map (fun d -> Char.chr (d + Char.code '0')) num
    in let chars = List.rev chars in 
      String.of_seq (List.to_seq chars)

let count_sevens s =
  let count = ref 0 in
    String.iter
      (fun c ->
        if c = '7' then
          incr count)
      s;

    !count

let factorial n =
  let rec multiply_digits i result =
    if i > n then
      result
    else
      multiply_digits (i + 1) (mult_myint result i)
    in multiply_digits 1 [1]


let () =
  let fact = factorial 6400 in
    let s = myint_to_string fact in
      Printf.printf "%d\n" (count_sevens s)
