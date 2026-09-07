let rec map_list f l =
    match l with
    | [] -> []
    | hd :: tl -> (f hd) :: (map_list f tl)

module type MAPPABLE = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
end

module ListM : MAPPABLE = struct
    type 'a t = Nil | Cons of 'a * 'a t
    let rec map (f : 'a -> 'b) (xs : 'a t) : 'b t=
        match xs with
        | Nil -> Nil
        | Cons (x, ys) -> Cons (f x, map f ys)
end

module OptionM : MAPPABLE = struct
    type 'a t = None | Some of 'a

    let map f x =
        match x with
        | None -> None
        | Some y -> Some (f y)
    end

module Make (M : MAPPABLE) = struct
    let twice f x =
        M.map f (M.map f x)
end

