open Utils
open Flags

type 'a t =
| Valid : ('rb, valid) Rule_bank.t * 'rb Rule_bank.idx -> [> valid ] t
| Invalid : ('rb, any) Rule_bank.t * 'rb Rule_bank.idx -> [> invalid ] t
type grammar = Rule_bank.Rule.rule

let make (rb, idx) = match Rule_bank.valid rb with
| Some vrb -> Valid (vrb, idx)
| None -> Invalid (rb, idx)

let valid : any t -> valid t option = function
| Valid (vrb, idx) -> Some (Valid (vrb, idx))
| Invalid _ -> None

let report_errs = assert false (* TODO: implement *)
let compile : valid t -> grammar = function
| Valid (rb, idx) ->
  let rb' = Rule_bank.compile rb in
  Rule_bank.get rb' idx

module Rule_bank = struct
  include Rule_bank

  type 'id t = ('id, any) Rule_bank.t
end
