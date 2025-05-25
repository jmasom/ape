open Utils
open Flags

module Blist_map = Map.Make (struct
  type t = bool list

  let compare = List.compare Bool.compare
end)

module rec Seg' : sig
  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]

  module Value : sig
    type 'tbl t

    val make : tbl:('tbl, (_, any) Seg_tbl.t) Named.t -> string -> 'tbl t
  end

  module Spec : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; tbl : 'tbl >

    val make :
      fs:('fs, any Feat_sys.t) Named.t ->
      tbl:('tbl, (< fs : 'fs >, any) Seg_tbl.t) Named.t ->
      bool list ->
      < fs : 'fs ; tbl : 'tbl > t
  end

  val make :
    'tbl Value.t * < fs : 'fs ; tbl : 'tbl > Spec.t * float ->
    (< fs : 'fs ; tbl : 'tbl >, any) t

  val raw_value_of : (_, any) t -> string
  val raw_spec_of : (_, any) t -> bool list
  val check : ('ctx, any) t -> ('ctx, valid) t option
end = struct
  module Value' = struct
    type +'j t = Valid of string | Invalid of string constraint 'j = [< any ]

    let make ~tbl rv =
      let tbl = Named.unpack tbl in
      if Seg_tbl.has_value rv tbl then Invalid rv else Valid rv
  end

  module Spec' = struct
    type +'j t =
      | Valid of bool list
      | Invalid of bool list
      constraint 'j = [< any ]

    let make ~fs ~tbl rs =
      let fs = Named.unpack fs in
      if List.length rs <> Feat_sys.size fs then Invalid rs
      else
        let tbl = Named.unpack tbl in
        if Seg_tbl.has_spec rs tbl then Invalid rs else Valid rs
  end

  type (+'ctx, +'j) t =
    | Valid of (valid Value'.t * valid Spec'.t * float)
    | Invalid of (any Value'.t * any Spec'.t * float)
    constraint 'ctx = < .. > constraint 'j = [< any ]

  let make = function
    | (Value'.Valid _ as v), (Spec'.Valid _ as s), w -> Valid (v, s, w)
    | (_ as v), (_ as s), w -> Invalid (v, s, w)

  let raw_value_of = function
    | Valid (Valid s, _, _)
    | Valid (Invalid s, _, _)
    | Invalid (Valid s, _, _)
    | Invalid (Invalid s, _, _) ->
        s

  let raw_spec_of = function
    | Valid (_, Valid bs, _)
    | Valid (_, Invalid bs, _)
    | Invalid (_, Valid bs, _)
    | Invalid (_, Invalid bs, _) ->
        bs

  let check = function Valid _ as v -> Some v | Invalid _ -> None

  module Value = struct
    include Value'

    type 'tbl t = any Value'.t
  end

  module Spec = struct
    include Spec'

    type 'ctx t = any Spec'.t constraint 'ctx = < fs : 'fs ; tbl : 'tbl >
  end
end

and Seg_tbl : sig
  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]

  val empty : (_, any) t

  val add :
    (< fs : 'fs ; tbl : 'tbl >, any) Seg'.t ->
    ('tbl, (< fs : 'fs >, any) t) Named.t ->
    (< fs : 'fs >, any) t

  val has_value : string -> (_, any) t -> bool
  val has_spec : bool list -> (_, any) t -> bool
  val check : ('ctx, any) t -> ('ctx, valid) t option
end = struct
  type (+'ctx, +'j) data = {
    segs : ('ctx, 'j) Seg'.t list;
    val_map : int String_map.t;
    spec_map : int Blist_map.t;
    size : int;
  }

  type (+'ctx, +'j) t =
    | Valid of ('ctx, valid) data
    | Invalid of ('ctx, any) data
    constraint 'ctx = < .. > constraint 'j = [< any ]

  let empty =
    Valid
      {
        segs = [];
        val_map = String_map.empty;
        spec_map = Blist_map.empty;
        size = 0;
      }

  let val_map_of = function
    | Valid { val_map; _ } | Invalid { val_map; _ } -> val_map

  let spec_map_of = function
    | Valid { spec_map; _ } | Invalid { spec_map; _ } -> spec_map

  let size_of = function Valid { size; _ } | Invalid { size; _ } -> size

  let add :
      (< fs : 'fs ; tbl : 'tbl >, any) Seg'.t ->
      ('tbl, (< fs : 'fs >, any) t) Named.t ->
      (< fs : 'fs >, any) t =
   fun seg ntbl ->
    let tbl = Named.unpack ntbl in
    (* Inspect *)
    let curr_val_map = val_map_of tbl
    and curr_spec_map = spec_map_of tbl
    and curr_size = size_of tbl in
    (* Modify maps and size *)
    let val_map = String_map.add (Seg'.raw_value_of seg) curr_size curr_val_map
    and spec_map = Blist_map.add (Seg'.raw_spec_of seg) curr_size curr_spec_map
    and size = curr_size + 1 in
    match tbl with
    | Valid { segs; _ } -> (
        match Seg'.check seg with
        | Some vseg ->
            Valid
              {
                segs = (vseg :> (< fs : 'fs >, valid) Seg'.t) :: segs;
                val_map;
                spec_map;
                size;
              }
        | None ->
            Invalid
              {
                segs =
                  (seg :> (< fs : 'fs >, any) Seg'.t)
                  :: (segs :> (< fs : 'fs >, any) Seg'.t list);
                val_map;
                spec_map;
                size;
              })
    | Invalid { segs; _ } ->
        Invalid
          {
            segs = (seg :> (< fs : 'fs >, any) Seg'.t) :: segs;
            val_map;
            spec_map;
            size;
          }

  let has_value s tbl = String_map.mem s (val_map_of tbl)
  let has_spec bs tbl = Blist_map.mem bs (spec_map_of tbl)
  let check = function Valid _ as v -> Some v | Invalid _ -> None
end

include Seg_tbl

module Seg = struct
  include Seg'

  type 'ctx t = ('ctx, any) Seg'.t constraint 'ctx = < fs : 'fs ; tbl : 'tbl >
end
