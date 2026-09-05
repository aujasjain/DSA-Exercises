(* Given three positive integers:, n, b, and k, print all the positive integers x less than n such that
the digit sum of x in base b is exactly k. *)

let digit_sum b x =
  let rec a sum n =
    if n = 0 then sum
    else a (sum + (n mod b)) (n / b)
      in a 0 x

let print_nums n b k =
  for x = 1 to n - 1 do
    if digit_sum b x = k then
      Printf.printf "%d\n" x
  done

let _ = print_nums 30 3 4
