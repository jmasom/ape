(** Word-generating grammars.

    At a minimum, a grammar consists of a context-sensitive collection of
    word-generating rules, called a rule bank, and a pointer, or index, to one
    of said rules.

    Ape grammars are built bottom-up and keep track of the validity of their
    sub-components as they are constructed. A grammar's validity judgement is
    encoded in its sole type argument ['j], which is always a subtype of
    [Utils.Flags.any]. A grammar of type [any t] is of unknown validity, whereas
    a grammar of type [valid t] has been checked, and is ultimately able to be
    compiled. A grammar's validity can be inspected via the [valid] function,
    which expects an unchecked grammar and returns a [valid t option].

    Ape grammars, once compiled, generate words, represented either as strings,
    or as lists of strings indicating the series of transformations from
    underlying to surface form. A grammar must have its validity checked before
    it can be compiled. *)

open Utils
open Flags

type 'j t
(** The type of grammars with validity judgement ['j]. *)

(** {1 Construction} *)

(** Type-identified collections of rules. *)
module Rule_bank : sig
  type 'id t
  (** The type of rule banks uniquely identified by type ['id]. *)

  type 'rb idx
  (** The type of the indices in the rule bank ['rb]. *)

  (** A modularly-encoded record containing a rule bank [value] and its
      identifier [id]. Used as a return type for functions that construct new
      rule banks. *)
  module type T = sig
    type id

    val value : id t
  end

  (** Contextualized word generation rules. *)
  module Rule : sig
    type 'rb t
    (** The type of rules constructed in the context of rule bank ['rb]. *)

    type 'rb rule_bank

    (** Contextualized expressions for initial word generation and matching. *)
    module Expr : sig
      type 'rb t
      (** The type of expressions constructed in the context of rule bank ['rb].
      *)

      val parse_ast : 'rb rule_bank -> Parsing.Expr.ast -> 'rb t
      (** [parse_ast rb ast] generates an expression from [ast] in the semantic
          context of [rb]. *)
    end

    (** Contextualized operations for replacing matched substrings. *)
    module Rewrite : sig
      type 'rb t
      (** The type of rewrite operations constructed in the context of rule bank
          ['rb]. *)

      (** Contextualized replacement operations to perform on a matched
          substring. *)
      module Repl : sig
        type 'ex t
        (** The type of replacement operations constructed in the context of
            expression ['ex]. *)

        val parse_ast : ('ex, 'rb Expr.t) Named.t -> Parsing.Repl.ast -> 'ex t
        (** [parse_ast named_ex ast] generates a replacement from [ast] in the
            semantic context of [Named.unpack named_ex]. *)
      end

      val make : ('ex, 'rb Expr.t) Named.t * 'ex Repl.t -> 'rb t
      (** [make named_ex r] constructs a rewrite that applies [r] to matching
          occurrences of [Named.unpack named_ex]. *)
    end

    val make : 'rb Expr.t * 'rb Rewrite.t list * 'rb Expr.t -> 'rb t
    (** [make (expr, rws, excl)] constructs a rule that generates words by
        sequentially applying each of [rws] to an initial word generated from
        [expr], rejecting words matched by [excl], and that also matches strings
        with [expr]. *)
  end
  with type 'id rule_bank := 'id t

  val empty : (module T)
  (** A packed rule bank containing no rules. *)

  val add : string -> 'id Rule.t -> 'id t -> (module T)
  (** [add name r rb] returns a new (packed) rule bank that contains [r] at an
      index denoted by [name], masking any rule previously stored with [name].
  *)

  val get_idx : 'id t -> string -> 'id idx
  (** [get_idx rb name] returns the abstract index of the rule added with name
      [name]. *)

  val idx_map_of : 'id t -> 'id idx String_map.t
  (** [idx_map_of rb] returns a map from names to the indices in [rb]. *)
end

val make : 'rb Rule_bank.t * 'rb Rule_bank.idx -> any t
(** [make rb idx] constructs a grammar whose root is the rule stored in [rb] at
    index [idx]. *)

(** {1 Analysis}*)

val valid : any t -> valid t option
(** [valid g] returns [Some vg] if [g] is valid, and otherwise returns [None].
*)

val report_errs : any t -> unit
(** [report_errs g] reports all errors generated during the construction of [g].
*)

(** {1 Compilation and usage} *)

type grammar
(** The type of compiled grammars. *)

val compile : valid t -> grammar
(** [compile vg] returns a compiled grammar that can generate words. *)

val gen_word : grammar -> string option
(** [gen_word grammar] returns [None] if an excluding expression was matched
    somewhere, and otherwise pseudo-randomly generates [Some word] from the root
    rule of [grammar]. *)

val gen_chain : grammar -> string Nonempty_list.t option
(** [gen_chain grammar] returns [None] if an excluding expression was matched
    somewhere, and otherwise pseudo-randomly generates [Some chain], where
    [chain] represents each rewrite transformation that was applied. *)
