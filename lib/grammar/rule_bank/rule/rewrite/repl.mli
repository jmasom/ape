open Utils
open Flags
open Module_types

module Make (E : EXPR) : sig
  type (+'ctx, +'j) t constraint 'ctx = < .. > constraint 'j = [< any ]

  val make :
    fs:('fs, any Feat_sys.t) Named.t ->
    ex:('ex, (< fs : 'fs ; rb : 'rb >, any) E.t) Named.t ->
    Parsing.Repl.ast ->
    (< fs : 'fs ; ex : 'ex >, any) t

  val check : ('ctx, any) t -> ('ctx, valid) t option
  val compile : (_, valid) t -> Re.Group.t -> string
end
