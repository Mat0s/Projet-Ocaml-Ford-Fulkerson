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
      let out_arcs = List.filter (fun n -> n.lbl>0) (out_arcs g node) in
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

  let find_capacity_min gr path = 
    let rec aux capacity_min = function
    | [] -> capacity_min
    | [_] -> capacity_min
    | src::tgt::rest ->
        match find_arc gr src tgt with
        | Some arc ->
        let current_capacity = arc.lbl in
        let capacity_min_aux = 
          if current_capacity < capacity_min then
            current_capacity
          else
            capacity_min
          in
        aux capacity_min_aux (tgt::rest)
        | None -> failwith "Arc not found"
  in
  aux max_int path

  

let update_arcs2 gr arc add =
  let gr' = add_arc gr arc.tgt arc.src add in
  let gr'' = add_arc gr' arc.src arc.tgt (-add) in
  gr''
  

let increase_flow graph path min_capacity =
  let rec aux gr = function
    | [] | [_] -> gr 
    | src :: tgt :: rest ->
      match find_arc gr src tgt with
      | Some arc ->
        let updated_arc = { arc with lbl = arc.lbl - min_capacity } in
        let updated_gr = update_arcs2 gr updated_arc min_capacity in
        aux updated_gr (tgt :: rest)
      | None -> assert false 
  in
  aux graph path

  let ford_fulkerson gr source dest =
    let rec loop graph =
      match find_path graph source dest with
      | [] -> graph  
      | path ->
        let min_capacity = find_capacity_min graph path in
        let updated_graph = increase_flow graph path min_capacity in
        loop updated_graph
    in
    loop gr
  
  

