open Utils
open Flags
open Module_types

module Make (E : EXPR) : sig
  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]

  (** A replacement operation to perform on a matched substring. *)
  module Repl : sig
    type 'ctx t constraint 'ctx = < fs : 'fs ; ex : 'ex >

    val make :
      fs:('fs, any Feat_sys.t) Named.t ->
      ex:('ex, (< fs : 'fs ; rb : 'rb >, any) E.t) Named.t ->
      Parsing.Repl.ast ->
      < fs : 'fs ; ex : 'ex > t
  end
  (** Expression identifier *)

  val make :
    ('ex, (< fs : 'fs ; rb : 'rb >, any) E.t) Named.t
    * < fs : 'fs ; ex : 'ex > Repl.t ->
    (< fs : 'fs ; rb : 'rb >, any) t

  val check : ('ctx, any) t -> ('ctx, valid) t option
  val compile : E.Rb.rule_bank -> (_, valid) t -> string -> string
end
