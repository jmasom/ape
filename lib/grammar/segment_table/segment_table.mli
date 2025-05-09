open Utils
open Flags

type +'j t constraint 'j = [< any ]

module Schema : sig
  type t

  module Col : sig
    type 'sc t
    type schema

    val make : sc:('sc, schema) Named.t -> string -> 'sc t
  end
  with type schema := t

  val empty : t
  val add : 'sc Col.t -> ('sc, t) Named.t -> t
end

module Seg : sig
  type 'tbl t
  type seg_tbl

  module Value : sig
    type 'tbl t

    val make : tbl:('tbl, seg_tbl) Named.t -> string -> 'tbl t
  end

  module Spec : sig
    type 'tbl t

    val make : tbl:('tbl, seg_tbl) Named.t -> bool list -> 'tbl t
  end

  val make : 'tbl Value.t * 'tbl Spec.t -> 'tbl t
end
with type seg_tbl := any t

val with_schema : Schema.t -> any t
val add : 'tbl Seg.t -> ('tbl, any t) Named.t -> any t

module Col_idx : sig
  type +'j t constraint 'j = [< any ]

  val check : any t -> valid t option
end

val get_col_idx : ('tbl, any t) Named.t -> string -> any Col_idx.t
val check : any t -> valid t option
