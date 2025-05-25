open Utils
open Flags
open Module_types

module Make (Rb : RULE_BANK) = struct
  type (+'ctx, +'j) node =
    | ELiteral of string
    | ERule_idx of 'j Rb.Idx.t
    | ELabeled of string * ('ctx, 'j) t
    | EOpt of ('ctx, 'j) t * float
    | ERep of ('ctx, 'j) t * int * int
    | EChoice of (('ctx, 'j) t * float) list
    | ESeq of ('ctx, 'j) t list
    constraint 'ctx = < .. > constraint 'j = [< any ]

  and (+'ctx, +'j) t =
    | Valid of ('ctx, valid) node
    | Invalid of ('ctx, any) node
    constraint 'ctx = < .. > constraint 'j = [< any ]

  type 'ex idx = int

  let check = function Valid _ as v -> Some v | Invalid _ -> None

  let rec make ~fs ~rb =
    Types.(
      function
      | Texpr_literal s -> Valid (ELiteral s)
      | Texpr_feature_spec _ -> assert false
      | Texpr_rule_ref rn -> (
          let idx = Rb.get_idx rb rn in
          match Rb.Idx.check idx with
          | Some vidx -> Valid (ERule_idx vidx)
          | None -> Invalid (ERule_idx idx))
      | Texpr_labeled (l, ast) -> (
          match make ~fs ~rb ast with
          | Valid _ as ve -> Valid (ELabeled (l, ve))
          | Invalid _ as e -> Invalid (ELabeled (l, e)))
      | Texpr_optional (ast, prob) -> (
          let prob = prob |? 0.5 in
          match make ~fs ~rb ast with
          | Valid _ as ve -> Valid (EOpt (ve, prob))
          | Invalid _ as e -> Invalid (EOpt (e, prob)))
      | Texpr_repeated (ast, bounds) -> (
          let lo, hi =
            match bounds with
            | Trb_range (lo, hi) -> (lo |? 0, hi)
            | Trb_count n -> (n, n)
          in
          match make ~fs ~rb ast with
          | Valid _ as ve -> Valid (ERep (ve, lo, hi))
          | Invalid _ as e -> Invalid (ERep (e, lo, hi)))
      | Texpr_choice wasts -> (
          (* Adapt ops to weighted space *)
          let wvalid = function
            | (Valid _, _) as wv -> Some wv
            | Invalid _, _ -> None
          in
          let parse_wast (ast, w) = (make ~fs ~rb ast, w |? 1.) in
          (* Parse subnodes *)
          let wes = List.map parse_wast wasts in
          match for_all_map wvalid wes with
          | Some vwes -> Valid (EChoice vwes)
          | None -> Invalid (EChoice wes))
      | Texpr_seq asts -> (
          let es = List.map (make ~fs ~rb) asts in
          match for_all_map check es with
          | Some ves -> Valid (ESeq ves)
          | None -> Invalid (ESeq es)))

  (* Can this be memoized or embedded in the node? *)
  let rec get_labels e =
    let n =
      match e with Valid vn -> (vn :> ('ctx, any) node) | Invalid n -> n
    in
    match n with
    | ELiteral _ | ERule_idx _ | EOpt _ | ERep _ | EChoice _ -> []
    | ELabeled (l, _) -> [ l ]
    | ESeq ns -> map_concat get_labels ns

  let label_map_of e =
    e |> get_labels |> List.mapi (fun i l -> (l, i + 1)) |> String_map.of_list

  class virtual expr =
    object
      val virtual re : Re.t Lazy.t
      method virtual gen_word : string option
      method to_re = Lazy.force re
    end

  let gen_word_from (e : expr) = e#gen_word
  let sub_re_of (e : expr) = Re.no_group e#to_re

  let rec compile rb e =
    let compile' = compile rb in
    match e with
    | Invalid _ -> assert false
    | Valid node -> (
        match node with
        | ELiteral s ->
            object
              inherit expr
              method gen_word = Some s
              val re = lazy (Re.str s)
            end
        | ERule_idx idx -> (Rb.get idx rb :> expr)
        | ELabeled (_, e) ->
            let expr = compile' e in
            object
              inherit expr
              val re = lazy (Re.group (sub_re_of expr))
              method gen_word = expr#gen_word
            end
        | EOpt (e, prob) ->
            let expr = compile' e in
            object
              inherit expr
              val re = lazy (Re.opt (sub_re_of expr))

              method gen_word =
                let sample = Random.float 1.0 in
                if sample < prob then expr#gen_word else Some ""
            end
        | ERep (e, lo, hi) ->
            let expr = compile' e in
            object
              inherit expr
              val re = lazy (Re.repn (sub_re_of expr) lo (Some hi))

              method gen_word =
                let sample = Random.int (hi + 1 - lo) + lo in
                let* sub_words =
                  expr |> Fun.const |> List.init sample
                  |> List.map gen_word_from |> for_all_map Fun.id
                in
                Some (String.concat "" sub_words)
            end
        | EChoice wes ->
            let wexprs = List.map (fun (e, w) -> (compile' e, w)) wes in
            let choice = Choice.of_weighted_list wexprs in
            object
              inherit expr
              val re = lazy (Re.alt (List.map (sub_re_of % fst) wexprs))

              method gen_word =
                let* chosen = Choice.choose choice in
                chosen#gen_word
            end
        | ESeq es ->
            let exprs = List.map compile' es in
            object
              inherit expr
              val re = lazy (Re.seq (List.map sub_re_of exprs))

              method gen_word =
                let* sub_words =
                  exprs |> List.map gen_word_from |> for_all_map Fun.id
                in
                Some (String.concat "" sub_words)
            end)

  module Rb = Rb
end
