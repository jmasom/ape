(* raw expressions; syntax tree nodes *)
type t =
(* literal *)
| Literal of string
(* referring expressions *)
| Feature_spec_appl of t * (string * bool) list
| Label_ref of string
(* recursive expressions *)
| Repeated of t * int
| Seq of t list