open Utils
open Flags

module rec Col' : sig
  type +'j t constraint 'j = [< any ]

  val make : sc:('sc, any Schema.t) Named.t -> string -> any t
  val to_string : 'j t -> string
  val check : any t -> valid t option
end = struct
  type +'j t = Valid of string | Invalid of string constraint 'j = [< any ]

  let make ~sc cn =
    let sc = Named.unpack sc in
    match Schema.get_idx cn sc with Some _ -> Valid cn | None -> Invalid cn

  let to_string = function Valid s | Invalid s -> s
  let check = function Valid _ as v -> Some v | Invalid _ -> None
end

and Schema : sig
  type +'j t constraint 'j = [< any ]

  val empty : any t
  val add : any Col'.t -> ('sc, any t) Named.t -> any t
  val get_idx : string -> any t -> int option
  val check : any t -> valid t option
end = struct
  type 'j data = {
    idx_map : int String_map.t;
    cols : 'j Col'.t list;
    size : int;
  }

  type +'j t =
    | Valid of valid data
    | Invalid of any data
    constraint 'j = [< any ]

  let empty = Valid { idx_map = String_map.empty; cols = []; size = 0 }

  let add c nsc =
    let sc = Named.unpack nsc in
    match (sc, Col'.check c) with
    | Valid { idx_map; cols; size }, Some vc ->
        Valid
          {
            idx_map = String_map.add (Col'.to_string vc) size idx_map;
            cols = vc :: cols;
            size = size + 1;
          }
    | Valid { idx_map; cols; size }, None ->
        Invalid
          { idx_map; cols = c :: (cols :> any Col'.t list); size = size + 1 }
    | Invalid { idx_map; cols; size }, Some vc ->
        Invalid
          {
            idx_map = String_map.add (Col'.to_string vc) size idx_map;
            cols = c :: cols;
            size = size + 1;
          }
    | Invalid { idx_map; cols; size }, None ->
        Invalid { idx_map; cols = c :: cols; size = size + 1 }

  let get_idx : string -> any t -> int option =
   fun s -> function
    | Valid { idx_map; _ } | Invalid { idx_map; _ } ->
        String_map.find_opt s idx_map

  let check : any t -> valid t option = function
    | Valid _ as v -> Some v
    | Invalid _ -> None
end

include Schema

module Col = struct
  include Col'

  type 'sc t = any Col'.t
end
