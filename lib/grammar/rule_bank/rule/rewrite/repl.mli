open Utils
open Flags
open Module_types

module Make (E : EXPR) : sig
  type +'j t constraint 'j = [< any ]

  val make : ex:('ex, any E.t) Named.t -> Parsing.Repl.ast -> any t
  val check : any t -> valid t option
  val compile : valid t -> Re.Group.t -> string
end
