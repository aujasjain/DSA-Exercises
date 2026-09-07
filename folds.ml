let rec sum lis = 
    match lis with
    | [] -> 0
    | hd :: tl -> hd + (sum tl)

let rec concat lis =
    match lis with
    | [] -> ""
    | hd :: tl -> hd ^ (concat tl)

let rec combine init op lis =
    match lis with
    | [] -> init
    | hd :: tl -> op hd (combine init op tl)

let sum' lis = combine 0 ( + ) lis
let concat' lis = combine "" ( ^ ) lis

(* RIGHT FOLD: Accumalates values starting from the right *)
let rec fold_right lis acc f =
    match lis with 
    | [] -> acc
    | hd :: tl -> f hd (fold_right tl acc f)

(* LEFT FOLD: Accumalates values starting from the left *)
let rec fold_left f acc lis=
    match lis with 
    | [] -> acc
    | hd :: tl -> fold_left f (f acc hd) tl