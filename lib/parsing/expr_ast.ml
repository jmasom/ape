(* raw expressions; syntax tree nodes *)
type t =
(* literal *)
| Literal of string
(* referring expressions *)
| Feature_spec of (string * bool) list
| Rule_ref of string
(* recursive expressions *)
(*| Group of t*)
| Labeled of string * t
| Optional of t * float option
| Repeated of t * repeat_bounds
| Choice of (t * float option) list
| Seq of t list

and repeat_bounds =
| Range of int option * int
| Count of int
