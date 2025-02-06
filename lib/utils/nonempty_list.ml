type 'a t = 'a * 'a list

let singleton x = (x, [])
let hd (x, _) = x
let cons x1 (x2, xs) = (x1, x2 :: xs)
let of_list = function [] -> None | x :: xs -> Some (x, xs)
let to_list (x, xs) = x :: xs
