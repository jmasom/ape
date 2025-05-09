open Utils
open Flags
open Module_types

module Make (E : EXPR) = struct
  module R = Repl.Make (E)

  type +'j t =
    | Valid of valid E.t * valid R.t
    | Invalid of any E.t * any R.t
    constraint 'j = [< any ]

  let make (type ex) ((ne, r) : (ex, any E.t) Named.t * any R.t) =
    let e = Named.unpack ne in
    match (E.check e, R.check r) with
    | Some ve, Some vr -> Valid (ve, vr)
    | _, _ -> Invalid (e, r)

  let check : any t -> valid t option = function
    | Valid (e, r) -> Some (Valid (e, r))
    | Invalid _ -> None

  let compile rb : valid t -> string -> string = function
    | Valid (e, r) ->
        let expr = E.compile rb e in
        let expr_re = Re.compile expr#to_re in
        let repl = R.compile r in
        Re.replace expr_re ~f:repl
    | Invalid _ -> assert false

  module Repl = struct
    include R

    type 'ex t = any R.t
  end
end
