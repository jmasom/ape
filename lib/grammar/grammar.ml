open Utils
open Flags

type 'j t =
  | Valid :
      ('rb, valid) Rule_bank.t * ('rb, valid) Rule_bank.Idx.t
      -> [> valid ] t
  | Invalid :
      ('rb, any) Rule_bank.t * ('rb, any) Rule_bank.Idx.t
      -> [> invalid ] t

type grammar = Types.rule

(* Constructor *)
let make (rb, idx) =
  match (Rule_bank.valid rb, Rule_bank.Idx.valid idx) with
  | Some vrb, Some vidx -> Valid (vrb, vidx)
  | _, _ -> Invalid (rb, idx)

(* Analysis *)
let valid = function
  | Valid (vrb, idx) -> Some (Valid (vrb, idx))
  | Invalid _ -> None

let report_errs _ = assert false (* TODO: implement *)

(* Compilation and usage *)
let compile : valid t -> grammar = function
  | Valid (rb, idx) ->
      let rb' = Rule_bank.compile rb in
      Rule_bank.get idx rb'

let gen_word (g : grammar) = g#gen_word
let gen_chain (g : grammar) = g#gen_chain

(* Masked sub-components *)
module Rule_bank = struct
  include Rule_bank

  type 'id t = ('id, any) Rule_bank.t
  type 'rb idx = ('rb, any) Idx.t

  let idx_map_of rb = (idx_map_of rb :> 'rb idx String_map.t)
end
