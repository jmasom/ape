(*module type Validated_type (* TODO: find a better name *) = sig
    open Flags

    type +'a t constraint 'a = [< any ]
    type error_report

    val valid : any t -> valid t option
    val to_error_report : any t -> error_report
  end

  module type Bank = sig
    type 'a t
    type key

    val lookup : key -> 'a t -> 'a
    val fold : ('acc -> 'a -> 'b) -> 'acc -> 'a t -> 'b
  end*)

let ( let* ) = Option.bind
let ( |? ) opt default = Option.value ~default opt
let ( % ) f g x = f (g x)

(* tail-recursive aux function *)
let rec for_all_map' prefix f = function
  | [] -> Some (prefix [])
  | x :: xs ->
      let* y = f x in
      let prefix' ys = prefix (y :: ys) in
      for_all_map' prefix' f xs

let for_all_map f = for_all_map' Fun.id f

(* tail-recursive aux function *)
let rec map_concat' prefix f = function
  | [] -> prefix []
  | x :: xs ->
      let prefix' ys = prefix (f x @ ys) in
      map_concat' prefix' f xs

let map_concat f = map_concat' Fun.id f

let rec rev_map_concat suffix f = function
  | [] -> [] @ suffix
  | x :: xs -> rev_map_concat (f x @ suffix) f xs

let rev_map_concat f = rev_map_concat [] f

module Choice = Choice
module Flags = Flags
module Named = Named
module Nonempty_list = Nonempty_list
module String_map = Map.Make (String)
