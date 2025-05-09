open Utils
open Flags

type 'j t =
  | Valid of valid Rule_bank.t * valid Rule_bank.Idx.t
  | Invalid of any Rule_bank.t * any Rule_bank.Idx.t
  constraint 'j = [< any ]

type grammar = Types.rule

(* Constructor *)
let make (nrb, idx) =
  let rb = Named.unpack nrb in
  match (Rule_bank.check rb, Rule_bank.Idx.check idx) with
  | Some vrb, Some vidx -> Valid (vrb, vidx)
  | _, _ -> Invalid (rb, idx)

(* Analysis *)
let check = function
  | Valid (vrb, idx) -> Some (Valid (vrb, idx))
  | Invalid _ -> None

let report_errs _ = assert false (* TODO: implement *)

(* Compilation and usage *)
let compile : valid t -> grammar = function
  | Invalid _ -> assert false
  | Valid (rb, idx) ->
      let rb' = Rule_bank.compile rb in
      Rule_bank.get idx rb'

let gen_word (g : grammar) = g#gen_word
let gen_chain (g : grammar) = g#gen_chain

(* Masked sub-components *)
module Segment_table = struct
  include Segment_table

  type t = any Segment_table.t
end

module Rule_bank = struct
  include Rule_bank

  type t = any Rule_bank.t
  type 'rb idx = any Idx.t

  let idx_map_of rb = (idx_map_of rb :> 'rb idx String_map.t)
end
