open Utils
open Flags

type 'a t constraint 'a = [< any ]
type grammar

(** A type-identified collection of rules. *)
module Rule_bank : sig
  type 'id t
  type 'rb idx

  module type T = sig
    type id

    val value : id t
  end

  (** A single rule, tagged with its context bank. *)
  module Rule : sig
    type 'rb t
    type 'rb rule_bank

    (** An expression for initial word generation and matching. *)
    module Expr : sig
      type 'rb t

      val parse_ast : 'rb rule_bank -> Expr_ast.t -> 'rb t
      (** Constructor *)
    end

    (** An operation for finding and replacing substrings. *)
    module Rewrite : sig
      type 'rb t
      type ('ex, 'rb) expr
      type 'rb anon_expr

      (** A matching expression tagged with an identifier type. *)
      module type Expr = sig
        type id
        type rb

        val value : (id, rb) expr
      end

      (** A replacement operation to perform on a matched substring. *)
      module Repl : sig
        type 'ex t

        val parse_ast : ('ex, 'rb) expr -> Repl_expr_ast.t -> 'ex t
      end

      val pack_expr : 'rb anon_expr -> (module Expr with type rb = 'rb)
      (** Expression identifier *)

      val make : ('ex, 'rb) expr * 'ex Repl.t -> 'rb t
      (** Constructor *)
    end
    with type 'rb anon_expr := 'rb Expr.t

    val make : 'rb Expr.t * 'rb Rewrite.t list * 'rb Expr.t -> 'rb t
    (** Constructor *)
  end
  with type 'id rule_bank := 'id t

  (** Constructors *)

  val empty : (module T)
  val add : string -> 'id Rule.t -> 'id t -> (module T)

  (** Index lookup *)

  val get_idx : 'id t -> string -> 'id idx option
  val idx_map : 'id t -> 'id idx String_map.t
end

val make : 'rb Rule_bank.t * 'rb Rule_bank.idx -> any t
(** Constructor *)

(** Validator and handling *)

val valid : any t -> valid t option
val report_errs : any t -> unit
val compile : valid t -> grammar
