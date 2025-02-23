(* expression syntax tree *)
type repeat_bounds = Trb_range of int option * int | Trb_count of int

type expr_ast =
  (* literal *)
  | Texpr_literal of string
  (* referring expressions *)
  | Texpr_feature_spec of (string * bool) list
  | Texpr_rule_ref of string
  (* recursive expressions *)
  | Texpr_labeled of string * expr_ast
  | Texpr_optional of expr_ast * float option
  | Texpr_repeated of expr_ast * repeat_bounds
  | Texpr_choice of (expr_ast * float option) list
  | Texpr_seq of expr_ast list

(* replacement syntax tree *)
type repl_ast =
  (* literal *)
  | Trepl_literal of string
  (* referring expressions *)
  | Trepl_label_ref of string
  (* recursive expressions *)
  | Trepl_feature_spec_appl of repl_ast * (string * bool) list
  | Trepl_repeated of repl_ast * int
  | Trepl_seq of repl_ast list

(* compiled generators/matching expressions *)
type expr = < gen_word : string option ; to_re : Re.t >
type rule = < expr ; gen_chain : string Utils.Nonempty_list.t option >
