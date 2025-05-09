open Utils
open Flags
open Module_types

module Make (E : EXPR) : sig
  type +'j t constraint 'j = [< any ]

  val make : ex:('ex, any E.t) Named.t -> Parsing.Repl.ast -> any t
  val check : any t -> valid t option
  val compile : valid t -> Re.Group.t -> string
end = struct
  type +'j node =
    | RLiteral of string
    | RLabel_idx of int
    | RUnknown_label of string
    | RRep of 'j t * int
    | RSeq of 'j t list
    constraint 'j = [< any ]

  and +'j t =
    | Valid of valid node
    | Invalid of any node
    constraint 'j = [< any ]

  let check = function Valid _ as v -> Some v | Invalid _ -> None

  let make ~ex ast =
    let e = Named.unpack ex in
    let label_map = E.label_map_of e in
    let rec parse_ast' =
      Types.(
        function
        | Trepl_literal s -> Valid (RLiteral s)
        | Trepl_feature_spec_appl _ -> assert false
        | Trepl_label_ref ln -> (
            match String_map.find_opt ln label_map with
            | Some idx -> Valid (RLabel_idx idx)
            | None -> Invalid (RUnknown_label ln))
        | Trepl_repeated (ast, count) -> (
            match parse_ast' ast with
            | Valid _ as vr -> Valid (RRep (vr, count))
            | Invalid _ as r -> Invalid (RRep (r, count)))
        | Trepl_seq asts -> (
            let rs = List.map parse_ast' asts in
            match for_all_map check rs with
            | Some vrs -> Valid (RSeq vrs)
            | None -> Invalid (RSeq rs)))
    in
    parse_ast' ast

  let rec compile = function
    | Invalid _ -> assert false
    | Valid node -> (
        match node with
        | RLiteral s -> Fun.const s
        | RLabel_idx idx -> fun g -> Re.Group.get g idx
        | RUnknown_label _ -> assert false
        | RRep (r, count) ->
            let sub_repl = compile r in
            fun g ->
              g |> sub_repl |> Fun.const |> List.init count |> String.concat ""
        | RSeq rs ->
            let sub_repls = List.map compile rs in
            fun g -> sub_repls |> List.map (( |> ) g) |> String.concat "")
end
