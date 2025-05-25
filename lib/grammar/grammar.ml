open Utils
open Flags

type 'j t =
  | Valid of
      (< >, valid) Segment_table.t
      * (< >, valid) Rule_bank.t
      * valid Rule_bank.Idx.t
  | Invalid of
      (< >, any) Segment_table.t * (< >, any) Rule_bank.t * any Rule_bank.Idx.t
  constraint 'j = [< any ]

type grammar = Types.rule

(* Constructor *)
let make (tbl, nrb, idx) =
  let tbl = (tbl :> (< >, any) Segment_table.t) in
  let rb = (Named.unpack nrb :> (< >, any) Rule_bank.t) in
  match
    (Segment_table.check tbl, Rule_bank.check rb, Rule_bank.Idx.check idx)
  with
  | Some tbl, Some vrb, Some vidx -> Valid (tbl, vrb, vidx)
  | _, _, _ -> Invalid (tbl, rb, idx)

(* Analysis *)
let check = function Valid _ as v -> Some v | Invalid _ -> None
let report_errs _ = assert false (* TODO: implement *)

(* Compilation and usage *)
let compile = function
  | Invalid _ -> assert false
  | Valid (_, rb, idx) ->
      let rb' = Rule_bank.compile rb in
      Rule_bank.get idx rb'

let gen_word (g : grammar) = g#gen_word
let gen_chain (g : grammar) = g#gen_chain

(* Masked sub-components *)
module Feat_sys = struct
  include Feat_sys

  type t = any Feat_sys.t
end

module Segment_table = struct
  include Segment_table

  type 'fs t = (< fs : 'fs >, any) Segment_table.t
end

module Rule_bank = struct
  include Rule_bank

  type 'fs t = (< fs : 'fs >, any) Rule_bank.t
  type 'rb idx = any Idx.t

  let idx_map_of rb = (idx_map_of rb :> 'rb idx String_map.t)
end
