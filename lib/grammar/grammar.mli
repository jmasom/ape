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

type 'j t constraint 'j = [< any ]
(** The type of grammars with validity judgement ['j]. *)

(** {1 Construction} *)

(** Tables containing segments defined by their representations and feature
    specifications. *)
module Segment_table : sig
  type t
  (** The type of segment tables. *)

  (** Collections of the features in a segment table. *)
  module Schema : sig
    type t
    (** The type of schemas. *)

    (** Contextualized phonological features. *)
    module Col : sig
      type 'sc t
      (** The type of features constructed in the context of schema ['sc]. *)

      type schema

      val make : sc:('sc, schema) Named.t -> string -> 'sc t
      (** [make ~sc s] generates a feature from [s] in the semantic context of
          [Named.unpack sc]. *)
    end
    with type schema := t

    val empty : t
    (** A schema containing no features. *)

    val add : 'sc Col.t -> ('sc, t) Named.t -> t
    (** [add c nsc] returns a new schema that contains all the features in
        [Named.unpack nsc] plus [c]. *)
  end

  (** Contextualized phonological segments. *)
  module Seg : sig
    type 'tbl t
    (** The type of segments constructed in the context of segment table ['tbl].
    *)

    type seg_tbl

    (** Contextualized segmental representations. *)
    module Value : sig
      type 'tbl t
      (** The type of representations constructed in the context of segment
          table ['tbl]. *)

      val make : tbl:('tbl, seg_tbl) Named.t -> string -> 'tbl t
      (** [make ~tbl s] generates a representatiaon from [s] in the semantic
          context of [Named.unpack tbl]. *)
    end

    (** Contextualized segmental feature specifications. *)
    module Spec : sig
      type 'tbl t
      (** The type of specifications constructed in the context of segment table
          ['tbl]. *)

      val make : tbl:('tbl, seg_tbl) Named.t -> bool list -> 'tbl t
      (** [make ~tbl bs] generates a specification from [bs] in the semantic
          context of [Named.unpack tbl]. *)
    end

    val make : 'tbl Value.t * 'tbl Spec.t -> 'tbl t
    (** [make (v, s)] constructs a segment with representation [v] and feature
        specification [s]. *)
  end
  with type seg_tbl := t

  val with_schema : Schema.t -> t
  (** [with_schema sc] constructs a segment table with schema [sc]. *)

  val add : 'tbl Seg.t -> ('tbl, t) Named.t -> t
  (** [add s ntbl] returns a new segment table that contains all the segments in
      [Named.unpack ntbl] plus [s]. The returned table has the same schema as
      [Named.unpack ntbl]. *)
end

(** Collections of rules. *)
module Rule_bank : sig
  type t
  (** The type of rule banks. *)

  type 'rb idx
  (** The type of the indices in the rule bank ['rb]. *)

  (** Contextualized word generation rules. *)
  module Rule : sig
    type 'rb t
    (** The type of rules constructed in the context of rule bank ['rb]. *)

    type rule_bank

    (** Contextualized expressions for initial word generation and matching. *)
    module Expr : sig
      type 'rb t
      (** The type of expressions constructed in the context of rule bank ['rb].
      *)

      val make : rb:('rb, rule_bank) Named.t -> Parsing.Expr.ast -> 'rb t
      (** [make ~rb ast] generates an expression from [ast] in the semantic
          context of [Named.unpack rb]. *)
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

        val make : ex:('ex, 'rb Expr.t) Named.t -> Parsing.Repl.ast -> 'ex t
        (** [make ~ex ast] generates a replacement from [ast] in the semantic
            context of [Named.unpack ex]. *)
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
  with type rule_bank := t

  val with_seg_tbl : Segment_table.t -> t
  (** [with_seg_tbl tbl] constructs a rule bank with segment table [tbl]. *)

  val add : string -> 'id Rule.t -> ('id, t) Named.t -> t
  (** [add name r nrb] returns a new rule bank that contains [r] at an index
      denoted by [name], masking any rule previously stored with [name]. *)

  val get_idx : ('id, t) Named.t -> string -> 'id idx
  (** [get_idx nrb name] returns the abstract index of the rule added with name
      [name]. *)

  val idx_map_of : ('id, t) Named.t -> 'id idx String_map.t
  (** [idx_map_of nrb] returns a map from names to the indices in [rb]. *)
end

val make : ('rb, Rule_bank.t) Named.t * 'rb Rule_bank.idx -> any t
(** [make nrb idx] constructs a grammar whose root is the rule stored in [rb] at
    index [idx]. *)

(** {1 Analysis}*)

val check : any t -> valid t option
(** [check g] returns [Some vg] if [g] is valid, and otherwise returns [None].
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
