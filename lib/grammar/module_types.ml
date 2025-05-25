open Utils
open Flags

module type RULE_BANK = sig
  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]
  type rule_bank

  module Idx : sig
    type +'j t constraint 'j = [< any ]

    val check : any t -> valid t option
  end

  val get_idx : ('rb, (_, any) t) Named.t -> string -> any Idx.t
  val get : valid Idx.t -> rule_bank -> Types.rule
end

module type RULE = sig
  module Rb : RULE_BANK

  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]

  (** An expression for initial word generation and matching. *)
  module Expr : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >

    val make :
      fs:('fs, any Feat_sys.t) Named.t ->
      rb:('rb, (< fs : 'fs >, any) Rb.t) Named.t ->
      Parsing.Expr.ast ->
      < fs : 'fs ; rb : 'rb > t
    (** Constructor *)
  end

  (** An operation for finding and replacing substrings. *)
  module Rewrite : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >

    (** A replacement operation to perform on a matched substring. *)
    module Repl : sig
      type 'ctx t constraint 'ctx = < fs : 'fs ; ex : 'ex >

      val make :
        fs:('fs, any Feat_sys.t) Named.t ->
        ex:('ex, < fs : 'fs ; rb : 'rb > Expr.t) Named.t ->
        Parsing.Repl.ast ->
        < fs : 'fs ; ex : 'ex > t
    end

    val make :
      ('ex, < fs : 'fs ; rb : 'rb > Expr.t) Named.t
      * < fs : 'fs ; ex : 'ex > Repl.t ->
      < fs : 'fs ; rb : 'rb > t
    (** Constructor *)
  end

  val make :
    < fs : 'fs ; rb : 'rb > Expr.t
    * < fs : 'fs ; rb : 'rb > Rewrite.t list
    * < fs : 'fs ; rb : 'rb > Expr.t ->
    (< fs : 'fs ; rb : 'rb >, any) t
  (** Constructor *)

  val check : ('ctx, any) t -> ('ctx, valid) t option
  val compile : Rb.rule_bank -> (< >, valid) t -> Types.rule
end

module type EXPR = sig
  module Rb : RULE_BANK

  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]
  type 'ex idx = private int

  val make :
    fs:('fs, any Feat_sys.t) Named.t ->
    rb:('rb, (< fs : 'fs >, any) Rb.t) Named.t ->
    Parsing.Expr.ast ->
    (< fs : 'fs ; rb : 'rb >, any) t

  val label_map_of : (_, any) t -> int String_map.t
  val check : ('ctx, any) t -> ('ctx, valid) t option
  val compile : Rb.rule_bank -> (_, valid) t -> Types.expr
end
