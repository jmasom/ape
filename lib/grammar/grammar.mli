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

(** Collections of phonological features. *)
module Feat_sys : sig
  type t
  (** The type of feature systems. *)

  (** Contextualized phonological features. *)
  module Feat : sig
    type 'fs t
    (** The type of features constructed in the context of feature system ['fs].
    *)

    type feat_sys

    val make : fs:('fs, feat_sys) Named.t -> string -> 'fs t
    (** [make ~fs s] generates a feature from [s] in the semantic context of
        [Named.unpack fs]. *)
  end
  with type feat_sys := t

  val empty : t
  (** A feature system containing no features. *)

  val add : 'fs Feat.t -> ('fs, t) Named.t -> t
  (** [add f nfs] returns a new feature system that contains all the features in
      [Named.unpack nfs] plus [f]. *)
end

(** Tables containing segments defined by their representations and feature
    specifications. *)
module Segment_table : sig
  type 'fs t
  (** The type of segment tables. *)

  (** Contextualized phonological segments. *)
  module Seg : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; tbl : 'tbl >
    (** The type of segments constructed in the context of feature system ['fs]
        and segment table ['tbl]. *)

    type 'fs seg_tbl

    (** Contextualized segmental representations. *)
    module Value : sig
      type 'tbl t
      (** The type of representations constructed in the context of segment
          table ['tbl]. *)

      val make : tbl:('tbl, 'fs seg_tbl) Named.t -> string -> 'tbl t
      (** [make ~tbl s] generates a representatiaon from [s] in the semantic
          context of [Named.unpack tbl]. *)
    end

    (** Contextualized segmental feature specifications. *)
    module Spec : sig
      type 'ctx t constraint 'ctx = < fs : 'fs ; tbl : 'tbl >
      (** The type of specifications constructed in the context of feature
          system ['fs] and segment table ['tbl]. *)

      val make :
        fs:('fs, Feat_sys.t) Named.t ->
        tbl:('tbl, 'fs seg_tbl) Named.t ->
        bool list ->
        < fs : 'fs ; tbl : 'tbl > t
      (** [make ~fs ~tbl bs] generates a specification from [bs] in the semantic
          context of [Named.unpack fs] and [Named.unpack tbl]. *)
    end

    val make :
      'tbl Value.t * < fs : 'fs ; tbl : 'tbl > Spec.t * float ->
      < fs : 'fs ; tbl : 'tbl > t
    (** [make (v, s, w)] constructs a segment with representation [v], feature
        specification [s], and weight [w]. *)
  end
  with type 'fs seg_tbl := 'fs t

  val empty : 'fs t
  (** A segment table containing no segments. *)

  val add : < fs : 'fs ; tbl : 'tbl > Seg.t -> ('tbl, 'fs t) Named.t -> 'fs t
  (** [add s ntbl] returns a new segment table that contains all the segments in
      [Named.unpack ntbl] plus [s]. The returned table has the same feature
      system as [Named.unpack ntbl]. *)
end

(** Collections of rules. *)
module Rule_bank : sig
  type 'fs t
  (** The type of rule banks. *)

  type 'rb idx
  (** The type of the indices in the rule bank ['rb]. *)

  (** Contextualized word generation rules. *)
  module Rule : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >
    (** The type of rules constructed in the context of feature system ['fs] and
        rule bank ['rb]. *)

    type 'fs rule_bank

    (** Contextualized expressions for initial word generation and matching. *)
    module Expr : sig
      type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >
      (** The type of expressions constructed in the context of feature system
          ['fs] and rule bank ['rb]. *)

      val make :
        fs:('fs, Feat_sys.t) Named.t ->
        rb:('rb, 'fs rule_bank) Named.t ->
        Parsing.Expr.ast ->
        < fs : 'fs ; rb : 'rb > t
      (** [make ~fs ~rb ast] generates an expression from [ast] in the semantic
          context of [Named.unpack fs] and [Named.unpack rb]. *)
    end

    (** Contextualized operations for replacing matched substrings. *)
    module Rewrite : sig
      type 'ctx t constraint 'ctx = < fs : 'fs ; rb : 'rb >
      (** The type of rewrite operations constructed in the context of feature
          system ['fs] and rule bank ['rb]. *)

      (** Contextualized replacement operations to perform on a matched
          substring. *)
      module Repl : sig
        type 'ctx t constraint 'ctx = < fs : 'fs ; ex : 'ex >
        (** The type of replacement operations constructed in the context of
            feature system ['fs] and expression ['ex]. *)

        val make :
          fs:('fs, Feat_sys.t) Named.t ->
          ex:('ex, < fs : 'fs ; rb : 'rb > Expr.t) Named.t ->
          Parsing.Repl.ast ->
          < fs : 'fs ; ex : 'ex > t
        (** [make ~fs ~ex ast] generates a replacement from [ast] in the
            semantic context of [Named.unpack fs] and [Named.unpack ex]. *)
      end

      val make :
        ('ex, < fs : 'fs ; rb : 'rb > Expr.t) Named.t
        * < fs : 'fs ; ex : 'ex > Repl.t ->
        < fs : 'fs ; rb : 'rb > t
      (** [make (nex, r)] constructs a rewrite that applies [r] to matching
          occurrences of [Named.unpack nex]. *)
    end

    val make :
      < fs : 'fs ; rb : 'rb > Expr.t
      * < fs : 'fs ; rb : 'rb > Rewrite.t list
      * < fs : 'fs ; rb : 'rb > Expr.t ->
      < fs : 'fs ; rb : 'rb > t
    (** [make (expr, rws, excl)] constructs a rule that generates words by
        sequentially applying each of [rws] to an initial word generated from
        [expr], rejecting words matched by [excl], and that also matches strings
        with [expr]. *)
  end
  with type 'fs rule_bank := 'fs t

  val empty : 'fs t
  (** A rule bank containing no rules. *)

  val add :
    string -> < fs : 'fs ; rb : 'rb > Rule.t -> ('rb, 'fs t) Named.t -> 'fs t
  (** [add name r nrb] returns a new rule bank that contains [r] at an index
      denoted by [name], masking any rule previously stored with [name]. *)

  val get_idx : ('rb, 'fs t) Named.t -> string -> 'rb idx
  (** [get_idx nrb name] returns the abstract index of the rule added with name
      [name]. *)

  val idx_map_of : ('rb, 'fs t) Named.t -> 'rb idx String_map.t
  (** [idx_map_of nrb] returns a map from names to the indices in [rb]. *)
end

val make :
  'fs Segment_table.t * ('rb, 'fs Rule_bank.t) Named.t * 'rb Rule_bank.idx ->
  any t
(** [make (st, nrb, idx)] constructs a grammar whose root is the rule stored in
    [Named.unpack nrb] at index [idx], using segment table [st]. *)

(** {1 Analysis} *)

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
