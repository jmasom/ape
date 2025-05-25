open Utils
open Flags

type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]

module Seg : sig
  type 'ctx t constraint 'ctx = < fs : 'fs ; tbl : 'tbl >
  type 'fs seg_tbl

  module Value : sig
    type 'tbl t

    val make : tbl:('tbl, _ seg_tbl) Named.t -> string -> 'tbl t
  end

  module Spec : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; tbl : 'tbl >

    val make :
      fs:('fs, any Feat_sys.t) Named.t ->
      tbl:('tbl, 'fs seg_tbl) Named.t ->
      bool list ->
      < fs : 'fs ; tbl : 'tbl > t
  end

  val make :
    'tbl Value.t * < fs : 'fs ; tbl : 'tbl > Spec.t * float ->
    < fs : 'fs ; tbl : 'tbl > t
end
with type 'fs seg_tbl := (< fs : 'fs >, any) t

val empty : (_, any) t

val add :
  < fs : 'fs ; tbl : 'tbl > Seg.t ->
  ('tbl, (< fs : 'fs >, any) t) Named.t ->
  (< fs : 'fs >, any) t

val check : ('ctx, any) t -> ('ctx, valid) t option
