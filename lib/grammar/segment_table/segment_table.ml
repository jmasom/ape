open Utils
open Flags

module Blist_map = Map.Make (struct
  type t = bool list

  let compare = List.compare Bool.compare
end)

module rec Seg' : sig
  type +'j t constraint 'j = [< any ]

  module Value : sig
    type 'tbl t

    val make : tbl:('tbl, any Seg_tbl.t) Named.t -> string -> 'tbl t
  end

  module Spec : sig
    type 'tbl t

    val make : tbl:('tbl, any Seg_tbl.t) Named.t -> bool list -> 'tbl t
  end

  val make : 'tbl Value.t * 'tbl Spec.t -> any t
  val raw_value_of : any t -> string
  val raw_spec_of : any t -> bool list
  val check : any t -> valid t option
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

    let make ~tbl rs =
      let tbl = Named.unpack tbl in
      if Seg_tbl.has_spec rs tbl then Invalid rs else Valid rs
  end

  type +'j t =
    | Valid of (valid Value'.t * valid Spec'.t)
    | Invalid of (any Value'.t * any Spec'.t)
    constraint 'j = [< any ]

  let make = function
    | (Value'.Valid _ as v), (Spec'.Valid _ as s) -> Valid (v, s)
    | (_ as v), (_ as s) -> Invalid (v, s)

  let raw_value_of = function
    | Valid (Valid s, _)
    | Valid (Invalid s, _)
    | Invalid (Valid s, _)
    | Invalid (Invalid s, _) ->
        s

  let raw_spec_of = function
    | Valid (_, Valid bs)
    | Valid (_, Invalid bs)
    | Invalid (_, Valid bs)
    | Invalid (_, Invalid bs) ->
        bs

  let check = function Valid _ as v -> Some v | Invalid _ -> None

  module Value = struct
    include Value'

    type 'tbl t = any Value'.t
  end

  module Spec = struct
    include Spec'

    type 'tbl t = any Spec'.t
  end
end

and Seg_tbl : sig
  type +'j t constraint 'j = [< any ]

  val with_schema : any Schema.t -> any t
  val add : any Seg'.t -> ('tbl, any t) Named.t -> any t
  val has_value : string -> any t -> bool
  val has_spec : bool list -> any t -> bool

  module Col_idx : sig
    type +'j t constraint 'j = [< any ]

    val check : any t -> valid t option
  end

  val get_col_idx : ('tbl, any t) Named.t -> string -> any Col_idx.t
  val check : any t -> valid t option
end = struct
  type 'j data = {
    schema : 'j Schema.t;
    segs : 'j Seg'.t list;
    val_map : int String_map.t;
    spec_map : int Blist_map.t;
    size : int;
  }

  type +'j t =
    | Valid of valid data
    | Invalid of any data
    constraint 'j = [< any ]

  let with_schema sc =
    match Schema.check sc with
    | Some vsc ->
        Valid
          {
            schema = vsc;
            segs = [];
            val_map = String_map.empty;
            spec_map = Blist_map.empty;
            size = 0;
          }
    | None ->
        Invalid
          {
            schema = sc;
            segs = [];
            val_map = String_map.empty;
            spec_map = Blist_map.empty;
            size = 0;
          }

  let schema_of = function
    | Valid { schema; _ } -> (schema :> any Schema.t)
    | Invalid { schema; _ } -> schema

  let val_map_of = function
    | Valid { val_map; _ } | Invalid { val_map; _ } -> val_map

  let spec_map_of = function
    | Valid { spec_map; _ } | Invalid { spec_map; _ } -> spec_map

  let size_of = function Valid { size; _ } | Invalid { size; _ } -> size

  let add : any Seg'.t -> ('tbl, any t) Named.t -> any t =
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
    | Valid { schema; segs; _ } -> (
        match Seg'.check seg with
        | Some vseg ->
            Valid { schema; segs = vseg :: segs; val_map; spec_map; size }
        | None ->
            Invalid
              {
                schema :> any Schema.t;
                segs = seg :: (segs :> any Seg'.t list);
                val_map;
                spec_map;
                size;
              })
    | Invalid { schema; segs; _ } ->
        Invalid { schema; segs = seg :: segs; val_map; spec_map; size }

  let has_value s tbl = String_map.mem s (val_map_of tbl)
  let has_spec bs tbl = Blist_map.mem bs (spec_map_of tbl)

  module Col_idx = struct
    type +'j t = Valid of int | Invalid of string constraint 'j = [< any ]

    let check = function Valid _ as v -> Some v | Invalid _ -> None
  end

  let get_col_idx : ('tbl, any t) Named.t -> string -> any Col_idx.t =
   fun ntbl cn ->
    let schema = schema_of (Named.unpack ntbl) in
    match Schema.get_idx cn schema with
    | Some n -> Col_idx.Valid n
    | None -> Col_idx.Invalid cn

  let check = function Valid _ as v -> Some v | Invalid _ -> None
end

include Seg_tbl

module Schema = struct
  include Schema

  type t = any Schema.t
end

module Seg = struct
  include Seg'

  type 'tbl t = any Seg'.t
end
