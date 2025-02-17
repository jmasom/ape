open Utils
open Flags

type 'a t
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

      val parse_ast : 'rb rule_bank -> Parsing.Expr.ast -> 'rb t
      (** Constructor *)
    end

    (** An operation for finding and replacing substrings. *)
    module Rewrite : sig
      type 'rb t

      (** A replacement operation to perform on a matched substring. *)
      module Repl : sig
        type 'ex t

        val parse_ast : ('ex, 'rb Expr.t) Named.t -> Parsing.Repl.ast -> 'ex t
      end

      val make : ('ex, 'rb Expr.t) Named.t * 'ex Repl.t -> 'rb t
      (** Constructor *)
    end

    val make : 'rb Expr.t * 'rb Rewrite.t list * 'rb Expr.t -> 'rb t
    (** Constructor *)
  end
  with type 'id rule_bank := 'id t

  (** Constructors *)

  val empty : (module T)
  val add : string -> 'id Rule.t -> 'id t -> (module T)

  (** Index lookup *)

  val get_idx : 'id t -> string -> 'id idx option
  val idx_map_of : 'id t -> 'id idx String_map.t
end

val make : 'rb Rule_bank.t * 'rb Rule_bank.idx -> any t
(** Constructor *)

(** Validator and handling *)

val valid : any t -> valid t option
val report_errs : any t -> unit
val compile : valid t -> grammar

val gen_word : grammar -> string option
val gen_chain : grammar -> string Nonempty_list.t option
