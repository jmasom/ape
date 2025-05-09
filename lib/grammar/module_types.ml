open Utils
open Flags

module type RULE_BANK = sig
  type +'j t constraint 'j = [< any ]
  type rule_bank

  module Idx : sig
    type +'j t constraint 'j = [< any ]

    val check : any t -> valid t option
  end

  val get_idx : ('rb, any t) Named.t -> string -> any Idx.t
  val get : valid Idx.t -> rule_bank -> Types.rule
end

module type RULE = sig
  module Rb : RULE_BANK

  type +'j t constraint 'j = [< any ]

  (** An expression for initial word generation and matching. *)
  module Expr : sig
    type t

    val make : rb:('rb, any Rb.t) Named.t -> Parsing.Expr.ast -> t
    (** Constructor *)
  end

  (** An operation for finding and replacing substrings. *)
  module Rewrite : sig
    type t
    type 'ex named_expr

    (** A replacement operation to perform on a matched substring. *)
    module Repl : sig
      type 'ex t

      val make : ex:'ex named_expr -> Parsing.Repl.ast -> 'ex t
    end

    val make : 'ex named_expr * 'ex Repl.t -> t
    (** Constructor *)
  end
  with type 'ex named_expr := ('ex, Expr.t) Named.t

  val make : Expr.t * Rewrite.t list * Expr.t -> any t
  (** Constructor *)

  val check : any t -> valid t option
  val compile : Rb.rule_bank -> valid t -> Types.rule
end

module type EXPR = sig
  module Rb : RULE_BANK

  type 'j t constraint 'j = [< any ]
  type 'ex idx = private int

  val make : rb:('rb, any Rb.t) Named.t -> Parsing.Expr.ast -> any t
  val label_map_of : any t -> int String_map.t
  val check : any t -> valid t option
  val compile : Rb.rule_bank -> valid t -> Types.expr
end
