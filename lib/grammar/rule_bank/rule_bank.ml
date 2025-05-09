open Utils
open Flags
open Types
open Module_types

module rec M : sig
  include RULE_BANK with type rule_bank = rule option array

  val with_seg_tbl : any Segment_table.t -> any t
  val add : string -> any R.t -> ('rb, any t) Named.t -> any t
  val idx_map_of : ('rb, any t) Named.t -> valid Idx.t String_map.t
  val check : any t -> valid t option
  val compile : valid t -> rule_bank
  val get : valid Idx.t -> rule_bank -> rule
end = struct
  module Idx = struct
    type +'j t = Valid of int | Invalid of string constraint 'j = [< any ]

    let check = function Valid _ as v -> Some v | Invalid _ -> None
  end

  type +'j shape = {
    seg_tbl : 'j Segment_table.t;
    idx_map : valid Idx.t String_map.t;
    rules : 'j R.t list;
    size : int;
  }

  type +'j t =
    | Valid of valid shape
    | Invalid of any shape
    constraint 'j = [< any ]

  type rule_bank = rule option array

  let idx_map_of nrb =
    match Named.unpack nrb with
    | Valid { idx_map; _ } | Invalid { idx_map; _ } -> idx_map

  let size_of = function Valid { size; _ } | Invalid { size; _ } -> size

  let with_seg_tbl tbl =
    match Segment_table.check tbl with
    | Some vtbl ->
        Valid
          { seg_tbl = vtbl; idx_map = String_map.empty; rules = []; size = 0 }
    | None ->
        Invalid
          { seg_tbl = tbl; idx_map = String_map.empty; rules = []; size = 0 }

  let add name r nrb =
    let rb = Named.unpack nrb in
    let idx_map' = idx_map_of nrb in
    let size' = size_of rb in
    let idx_map = String_map.add name (Idx.Valid size') idx_map' in
    let size = size' + 1 in
    match (rb, R.check r) with
    | Valid { seg_tbl; rules; _ }, Some vr ->
        Valid { seg_tbl; rules = vr :: rules; idx_map; size }
    | Valid { seg_tbl; rules; _ }, None ->
        Invalid
          {
            seg_tbl :> any Segment_table.t;
            rules = r :: (rules :> any R.t list);
            idx_map;
            size;
          }
    | Invalid { seg_tbl; rules; _ }, _ ->
        Invalid { seg_tbl; rules = r :: rules; idx_map; size }

  let get_idx nrb name =
    match String_map.find_opt name (idx_map_of nrb) with
    | Some idx -> (idx :> any Idx.t)
    | None -> Idx.Invalid name

  let check = function Valid _ as v -> Some v | Invalid _ -> None

  let compile = function
    | Invalid _ -> assert false
    | Valid { rules; size; _ } ->
        (* Needs to be done this way instead of with rev_map so that R.compile
         * can look up compiled rules each time it's called *)
        let arr = Array.make size None in
        rules |> List.rev
        |> List.iteri (fun i r -> arr.(i) <- Some (R.compile arr r));
        arr

  let get idx rb =
    match idx with
    | Idx.Valid i -> Option.get rb.(i)
    | Idx.Invalid _ -> assert false
end

and R : sig
  type +'j t constraint 'j = [< any ]

  (** An expression for initial word generation and matching. *)
  module Expr : sig
    type t

    val make : rb:('rb, any M.t) Named.t -> Parsing.Expr.ast -> t
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
  val compile : M.rule_bank -> valid t -> rule
end =
  Rule.Make (M)

include M

module Rule = struct
  include R

  type 'rb t = any R.t

  module Expr = struct
    include Expr

    type 'rb t = Expr.t
  end

  module Rewrite = struct
    include Rewrite

    type 'rb t = Rewrite.t
  end
end
