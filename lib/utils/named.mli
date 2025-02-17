type ('id, 'a) t

module type T = sig
  type id
  type t'

  val value : (id, t') t
end

val pack : 'a -> (module T with type t' = 'a)
val unpack : ('id, 'a) t -> 'a
