open Utils
open Flags

module rec Feat' : sig
  type +'j t constraint 'j = [< any ]

  val make : fs:('fs, any Feat_sys.t) Named.t -> string -> any t
  val to_string : 'j t -> string
  val check : any t -> valid t option
end = struct
  type +'j t = Valid of string | Invalid of string constraint 'j = [< any ]

  let make ~fs cn =
    let fs = Named.unpack fs in
    match Feat_sys.get_idx cn fs with Some _ -> Valid cn | None -> Invalid cn

  let to_string = function Valid s | Invalid s -> s
  let check = function Valid _ as v -> Some v | Invalid _ -> None
end

and Feat_sys : sig
  type +'j t constraint 'j = [< any ]

  val empty : any t
  val add : any Feat'.t -> ('fs, any t) Named.t -> any t
  val get_idx : string -> any t -> int option
  val size : any t -> int
  val check : any t -> valid t option
end = struct
  type 'j data = {
    idx_map : int String_map.t;
    cols : 'j Feat'.t list;
    size : int;
  }

  type +'j t =
    | Valid of valid data
    | Invalid of any data
    constraint 'j = [< any ]

  let empty = Valid { idx_map = String_map.empty; cols = []; size = 0 }

  let add c nfs =
    let fs = Named.unpack nfs in
    match (fs, Feat'.check c) with
    | Valid { idx_map; cols; size }, Some vc ->
        Valid
          {
            idx_map = String_map.add (Feat'.to_string vc) size idx_map;
            cols = vc :: cols;
            size = size + 1;
          }
    | Valid { idx_map; cols; size }, None ->
        Invalid
          { idx_map; cols = c :: (cols :> any Feat'.t list); size = size + 1 }
    | Invalid { idx_map; cols; size }, Some vc ->
        Invalid
          {
            idx_map = String_map.add (Feat'.to_string vc) size idx_map;
            cols = c :: cols;
            size = size + 1;
          }
    | Invalid { idx_map; cols; size }, None ->
        Invalid { idx_map; cols = c :: cols; size = size + 1 }

  let get_idx : string -> any t -> int option =
   fun s -> function
    | Valid { idx_map; _ } | Invalid { idx_map; _ } ->
        String_map.find_opt s idx_map

  let size = function Valid { size; _ } | Invalid { size; _ } -> size

  let check : any t -> valid t option = function
    | Valid _ as v -> Some v
    | Invalid _ -> None
end

include Feat_sys

module Feat = struct
  include Feat'

  type 'fs t = any Feat'.t
end
