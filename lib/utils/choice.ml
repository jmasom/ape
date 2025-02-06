type 'a t' = Empty | Leaf of float * 'a | Branch of float * 'a t' * 'a t'

type 'a t = 'a t' * 'a list

let weight_at = function
  | Empty -> 0.
  | Leaf (weight, _) | Branch (weight, _, _) -> weight

let empty = Empty, []

let join node1 node2 =
  (* Combine branches and ensure left weight is smaller *)
  let weight1 = weight_at node1 in
  let weight2 = weight_at node2 in
  let left_node, right_node =
    if weight1 < weight2 then (node1, node2) else (node2, node1)
  in
  Branch (weight1 +. weight2, left_node, right_node)

let rec add_node node = function
  | Empty -> node
  | Leaf (total_weight, _) as top_node ->
      Branch (total_weight +. weight_at node, top_node, node)
  | Branch (_, left_tree, right_tree) as top_node ->
      (* Minimize diff between branch weights *)
      let node1, node2 =
        if weight_at node >= weight_at left_tree +. weight_at right_tree then
          (top_node, node)
        else (add_node node left_tree, right_tree)
      in
      join node1 node2

let add value weight (tree, nodes) =
  add_node (Leaf (Float.max weight 0., value)) tree, value :: nodes

let of_weighted_list weighted_vals =
  List.fold_left (fun n (v, w) -> add v w n) empty weighted_vals

let to_list (_, nodes) = List.rev nodes

let rec value_at_target target tree =
  if target >= weight_at tree then None
  else
    match tree with
    | Empty -> None (* Covered by if-statement check *)
    | Leaf (_, value) -> Some value
    | Branch (_, left_tree, right_tree) -> (
        match value_at_target target left_tree with
        | Some _ as value -> value
        | None -> value_at_target (target -. weight_at left_tree) right_tree)

let choose (tree, _) = value_at_target (Random.float (weight_at tree)) tree
