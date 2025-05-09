open Utils
open Flags
open Module_types

module Make
    (E : EXPR) : sig
  type +'j t constraint 'j = [< any ]
  type 'ex named_expr

  (** A replacement operation to perform on a matched substring. *)
  module Repl : sig
    type 'ex t

    val make : ex:'ex named_expr -> Parsing.Repl.ast -> 'ex t
  end
  (** Expression identifier *)

  val make : 'ex named_expr * 'ex Repl.t -> any t
  val check : any t -> valid t option
  val compile : E.Rb.rule_bank -> valid t -> string -> string
end
with type 'ex named_expr := ('ex, any E.t) Named.t
