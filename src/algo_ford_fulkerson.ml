open Tools
open Graph

let init gr =
  let aux gr arc1 = add_arc gr arc1.tgt arc1.src 0 in
  e_fold gr aux gr


let find_path g source dest =
  let rec aux visited current_path node =
    if node = dest then
      List.rev (node :: current_path)
    else if List.mem node visited then
      []
    else
      let out_arcs = out_arcs g node in
      let next_nodes = List.map (fun arc -> arc.tgt) out_arcs in
      let valid_next_nodes = List.filter (fun n -> not (List.mem n current_path)) next_nodes in
      List.fold_left (fun acc next_node ->
        if acc <> [] then
          acc
        else
          aux (node :: visited) (node :: current_path) next_node
      ) [] valid_next_nodes
  in
  aux [] [] source

let update_arcs2 gr arc add =
  let gr' = add_arc gr arc.tgt arc.src add in
  let gr'' = add_arc gr' arc.src arc.tgt (-add) in
  gr''

let update_graph gr path =
  let rec update_arcs g p =
    match p with
    | [] | [_] -> g
    | src :: tgt :: rest ->
      let arc_opt = find_arc g src tgt in
      match arc_opt with
      | Some(arc) ->
        let add = arc.lbl in
        let g' = update_arcs2 g arc add in
        update_arcs g' rest
      | None ->
        (* Handle the case where the arc is not found, raise an exception or handle accordingly *)
        raise (Graph_error ("Arc from " ^ string_of_int src ^ " to " ^ string_of_int tgt ^ " not found."))
  in
  update_arcs gr path



