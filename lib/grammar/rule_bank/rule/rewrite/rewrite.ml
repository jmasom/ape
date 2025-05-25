open Utils
open Flags
open Module_types

module Make (E : EXPR) = struct
  module R = Repl.Make (E)

  type (+'ctx, +'j) t =
    | Valid of (< >, valid) E.t * (< >, valid) R.t
    | Invalid of (< >, any) E.t * (< >, any) R.t
    constraint 'ctx = < .. > constraint 'j = [< any ]

  let make (type ex)
      ((ne, r) :
        (ex, (< fs : 'fs ; rb : 'rb >, any) E.t) Named.t
        * (< fs : 'fs ; ex : 'ex >, any) R.t) =
    let e = Named.unpack ne in
    match (E.check e, R.check r) with
    | Some ve, Some vr ->
        Valid ((ve :> (< >, valid) E.t), (vr :> (< >, valid) R.t))
    | _, _ -> Invalid ((e :> (< >, any) E.t), (r :> (< >, any) R.t))

  let check : ('ctx, any) t -> ('ctx, valid) t option = function
    | Valid (e, r) -> Some (Valid (e, r))
    | Invalid _ -> None

  let compile rb : (_, valid) t -> string -> string = function
    | Valid (e, r) ->
        let expr = E.compile rb e in
        let expr_re = Re.compile expr#to_re in
        let repl = R.compile r in
        Re.replace expr_re ~f:repl
    | Invalid _ -> assert false

  module Repl = struct
    include R

    type 'ctx t = ('ctx, any) R.t constraint 'ctx = < fs : 'fs ; ex : 'ex >
  end
end
