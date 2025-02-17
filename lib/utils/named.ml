type ('id, 'a) t = 'a

module type T = sig
  type id
  type t'

  val value : (id, t') t
end

let pack (type a) (x : a) = (module struct
  type id
  type t' = a

  let value = x
end : T with type t' = a)

let unpack = Fun.id
