open Tools
open Graph

(* find_path trouve un chemin dans g entre ids et idp et le retourne à l'envers dans une liste*)

(* créé des nouveaux arcs (retour avec label=0) *)
let init gr =
  let aux gr arc1 = add_arc gr arc1.tgt arc1.src 0 in
  e_fold gr aux gr 


let update_arcs gr arc add=
let gr = add_arc gr arc.tgt arc.src add in
let gr = add_arc gr arc.src arc.tgt (-add) in
gr

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


