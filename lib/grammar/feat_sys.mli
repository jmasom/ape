open Utils
open Flags

type +'j t constraint 'j = [< any ]

module Feat : sig
  type 'fs t
  type feat_sys

  val make : fs:('fs, feat_sys) Named.t -> string -> 'fs t

  (*  module Set : sig
    type 'fs t
    type 'fs feat

    val empty : 'fs t
    val add : 'fs feat -> 'fs t -> 'fs t
    val mem : 'fs feat -> 'fs t -> bool
  end*)
end
with type feat_sys := any t

val empty : any t
val add : 'fs Feat.t -> ('fs, any t) Named.t -> any t
val get_idx : string -> any t -> int option
val size : any t -> int
val check : any t -> valid t option
