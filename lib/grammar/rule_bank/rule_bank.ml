open Utils
open Flags
open Types
open Module_types

module rec M : sig
  include RULE_BANK with type rule_bank = rule option array

  val empty : (_, any) t

  val add :
    string ->
    (< fs : 'fs ; rb : 'rb >, any) R.t ->
    ('rb, (< fs : 'fs >, any) t) Named.t ->
    (< fs : 'fs >, any) t

  val idx_map_of :
    ('rb, (< fs : 'fs >, any) t) Named.t -> valid Idx.t String_map.t

  val check : ('a, any) t -> ('a, valid) t option
  val compile : (< >, valid) t -> rule_bank
  val get : valid Idx.t -> rule_bank -> rule
end = struct
  module Idx = struct
    type +'j t = Valid of int | Invalid of string constraint 'j = [< any ]

    let check = function Valid _ as v -> Some v | Invalid _ -> None
  end

  type +'j shape = {
    idx_map : valid Idx.t String_map.t;
    rules : (< >, 'j) R.t list;
    size : int;
  }

  type (+'ctx, +'j) t =
    | Valid of valid shape
    | Invalid of any shape
    constraint 'ctx = < .. > constraint 'j = [< any ]

  type rule_bank = rule option array

  let idx_map_of nrb =
    match Named.unpack nrb with
    | Valid { idx_map; _ } | Invalid { idx_map; _ } -> idx_map

  let size_of = function Valid { size; _ } | Invalid { size; _ } -> size
  let empty = Valid { idx_map = String_map.empty; rules = []; size = 0 }

  let add name r nrb =
    let rb = Named.unpack nrb in
    let idx_map' = idx_map_of nrb in
    let size' = size_of rb in
    let idx_map = String_map.add name (Idx.Valid size') idx_map' in
    let size = size' + 1 in
    match (rb, R.check r) with
    | Valid { rules; _ }, Some vr ->
        Valid { rules = (vr :> (< >, valid) R.t) :: rules; idx_map; size }
    | Valid { rules; _ }, None ->
        Invalid
          {
            rules = (r :> (< >, any) R.t) :: (rules :> (< >, any) R.t list);
            idx_map;
            size;
          }
    | Invalid { rules; _ }, _ ->
        Invalid { rules = (r :> (< >, any) R.t) :: rules; idx_map; size }

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
  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]

  (** An expression for initial word generation and matching. *)
  module Expr : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >

    val make :
      fs:('fs, any Feat_sys.t) Named.t ->
      rb:('rb, (< fs : 'fs >, any) M.t) Named.t ->
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
  val compile : M.rule_bank -> (< >, valid) t -> rule
end =
  Rule.Make (M)

include M

module Rule = struct
  include R

  type 'ctx t = ('ctx, any) R.t constraint 'ctx = < fs : 'fs ; rb : 'rb >
end
