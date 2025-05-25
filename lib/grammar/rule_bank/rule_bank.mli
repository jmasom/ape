open Utils
open Flags
include Module_types.RULE_BANK

module Rule : sig
  type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >

  type (+'ctx, +'j) rule_bank
    constraint 'ctx = < fs : 'fs > constraint 'j = [< any ]

  module Expr : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >

    val make :
      fs:('fs, any Feat_sys.t) Named.t ->
      rb:('rb, (< fs : 'fs >, any) rule_bank) Named.t ->
      Parsing.Expr.ast ->
      < fs : 'fs ; rb : 'rb > t
  end

  module Rewrite : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >

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
  end

  val make :
    < fs : 'fs ; rb : 'rb > Expr.t
    * < fs : 'fs ; rb : 'rb > Rewrite.t list
    * < fs : 'fs ; rb : 'rb > Expr.t ->
    < fs : 'fs ; rb : 'rb > t
end
with type (+'ctx, +'j) rule_bank := ('ctx, 'j) t

val empty : (_, any) t

val add :
  string ->
  < fs : 'fs ; rb : 'rb > Rule.t ->
  ('rb, (< fs : 'fs >, any) t) Named.t ->
  (< fs : 'fs >, any) t

val idx_map_of :
  ('rb, (< fs : 'fs >, any) t) Named.t -> valid Idx.t String_map.t

val check : ('a, any) t -> ('a, valid) t option
val compile : (< >, valid) t -> rule_bank
