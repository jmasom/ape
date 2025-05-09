open Utils
open Flags
include Module_types.RULE_BANK

module Rule : sig
  type 'rb t
  type 'rb named_rb

  module Expr : sig
    type 'rb t

    val make : rb:'rb named_rb -> Parsing.Expr.ast -> 'rb t
  end

  module Rewrite : sig
    type 'rb t

    module Repl : sig
      type 'ex t

      val make : ex:('ex, 'rb Expr.t) Named.t -> Parsing.Repl.ast -> 'ex t
    end

    val make : ('ex, 'rb Expr.t) Named.t * 'ex Repl.t -> 'rb t
  end

  val make : 'rb Expr.t * 'rb Rewrite.t list * 'rb Expr.t -> 'rb t
end
with type 'rb named_rb := ('rb, any t) Named.t

val with_seg_tbl : any Segment_table.t -> any t
val add : string -> 'rb Rule.t -> ('rb, any t) Named.t -> any t
val idx_map_of : ('rb, any t) Named.t -> valid Idx.t String_map.t
val check : any t -> valid t option
val compile : valid t -> rule_bank
