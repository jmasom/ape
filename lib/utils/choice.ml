type 'a t' = Empty | Leaf of float * 'a | Branch of float * 'a t' * 'a t'
type 'a t = 'a t' * 'a list

let weight_at = function
  | Empty -> 0.
  | Leaf (weight, _) | Branch (weight, _, _) -> weight

let empty = (Empty, [])

let join n1 n2 =
  (* Combine branches and ensure left weight is smaller *)
  let w1 = weight_at n1 in
  let w2 = weight_at n2 in
  let left_node, right_node = if w1 < w2 then (n1, n2) else (n2, n1) in
  Branch (w1 +. w2, left_node, right_node)

let rec add_node n = function
  | Empty -> n
  | Leaf (total_weight, _) as top_node ->
      Branch (total_weight +. weight_at n, top_node, n)
  | Branch (_, left_tree, right_tree) as top_node ->
      (* Minimize diff between branch weights *)
      let n1, n2 =
        if weight_at n >= weight_at left_tree +. weight_at right_tree then
          (top_node, n)
        else (add_node n left_tree, right_tree)
      in
      join n1 n2

let add value weight (tree, nodes) =
  (add_node (Leaf (Float.max weight 0., value)) tree, value :: nodes)

let of_weighted_list weighted_vals =
  List.fold_left (fun n (v, w) -> add v w n) empty weighted_vals

let to_list (_, ns) = List.rev ns

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
