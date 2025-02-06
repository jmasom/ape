type 'a t

val singleton : 'a -> 'a t
val hd : 'a t -> 'a
val cons : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t option
val to_list : 'a t -> 'a list
