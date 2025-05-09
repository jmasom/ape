open Utils
open Flags

type +'j t constraint 'j = [< any ]

module Col : sig
  type 'sc t
  type schema

  val make : sc:('sc, schema) Named.t -> string -> 'sc t
end
with type schema := any t

val empty : any t
val add : 'sc Col.t -> ('sc, any t) Named.t -> any t
val get_idx : string -> any t -> int option
val check : any t -> valid t option
