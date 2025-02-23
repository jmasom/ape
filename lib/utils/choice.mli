type 'a t

val empty : 'a t
val add : 'a -> float -> 'a t -> 'a t
val of_weighted_list : ('a * float) list -> 'a t
val to_list : 'a t -> 'a list
val choose : 'a t -> 'a option
