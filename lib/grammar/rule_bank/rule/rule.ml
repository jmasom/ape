open Utils
open Flags
open Module_types

module Make (Rb : RULE_BANK) = struct
  module E = Expr.Make (Rb)
  module Rw = Rewrite.Make (E)

  type +'j t =
    | Valid of {
        expr : valid E.t;
        rewrites : valid Rw.t list;
        excl : valid E.t;
      }
    | Invalid of { expr : any E.t; rewrites : any Rw.t list; excl : any E.t }
    constraint 'j = [< any ]

  let check = function Valid _ as v -> Some v | Invalid _ -> None

  let make (expr, rewrites, excl) =
    match (E.check expr, for_all_map Rw.check rewrites, E.check excl) with
    | Some expr, Some rewrites, Some excl -> Valid { expr; rewrites; excl }
    | _, _, _ -> Invalid { expr; rewrites; excl }

  let compile rb = function
    | Invalid _ -> assert false
    | Valid { expr; rewrites; excl } ->
        let expr' = E.compile rb expr in
        let rewrites' = List.map (Rw.compile rb) rewrites in
        let excl_re = Re.compile (E.compile rb excl)#to_re in

        object
          method gen_word =
            let* init_word = expr'#gen_word in
            let word = List.fold_left ( |> ) init_word rewrites' in
            if Re.execp excl_re word then None else Some word

          method to_re = expr'#to_re

          method gen_chain =
            Nonempty_list.(
              let* init_word = expr'#gen_word in
              let init_chain = singleton init_word in
              let chain =
                List.fold_left
                  (fun c rw -> cons (rw (hd c)) c)
                  init_chain rewrites'
              in
              if Re.execp excl_re (hd chain) then None else Some chain)
        end

  (* Hide nested validity judgements *)
  module Expr = struct
    include E

    type t = any E.t
  end

  module Rewrite = struct
    include Rw

    type t = any Rw.t
  end
end
