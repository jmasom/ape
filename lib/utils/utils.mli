val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( !! ) : 'a Lazy.t -> 'a
val ( % ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val for_all_map : ('a -> 'b option) -> 'a list -> 'b list option
val map_concat : ('a -> 'b list) -> 'a list -> 'b list
val rev_map_concat : ('a -> 'b list) -> 'a list -> 'b list

module Choice : module type of Choice
module Flags : module type of Flags
module Nonempty_list : module type of Nonempty_list
module String_map : Map.S with type key = string
