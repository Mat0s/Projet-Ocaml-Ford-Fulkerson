open Tools
open Graph
open Printf


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
      let test_find_path = find_path graph 0 2 in

      let () = printf "path : [" in
      let () = List.iter (printf "%d ") test_find_path in
      let () = printf "]" in
    
      let find_capacity_min_test = find_capacity_min graph test_find_path in
      let () = printf "find_capacity_min_test : %d" find_capacity_min_test in
      match find_path graph source dest with
      | [] -> graph  
      | path ->
        let min_capacity = find_capacity_min graph path in
        let updated_graph = increase_flow graph path min_capacity in
        loop updated_graph
    in
    loop gr




let transform gr_init gr_ecart =
  e_fold gr_init 
  (fun gr_flot arc -> 
    let arc_ecart = find_arc gr_ecart arc.src arc.tgt in
    let lbl_ecart = match arc_ecart with 
    | Some a -> a.lbl
    | None -> 0 
    in

    create_add_arc2 gr_flot arc.src arc.tgt (max (arc.lbl-lbl_ecart) 0,arc.lbl)) 
    
    (clone_nodes gr_init)


(*Application : Chemin de Fret d'une ville A à une ville B*)
  
let tabVilles = ["Toulouse"; "Dubai"; "Marseille"; "Lyon"; "Bruxelles"; "Reykjavik"; "Washington"; "Pekin"; "Tokyo"; "Seoul"; "Sydney"; "Bordeaux"; "Casablanca"] 

let getVille id = List.nth tabVilles id

let getId ville = 
  let rec aux acu l = match l with    
                    |x::rest -> if x=ville then acu else aux (acu+1) rest
                    |[] -> raise Not_found
  in
  aux 0 tabVilles




  


let export_ville path gr=

    let op = open_out path in
  
    fprintf op "digraph finite_state_machine {rankdir=LR; node [shape = circle];";
  
    e_iter gr (fun arc -> fprintf op "%s -> %s [label = \"%s\"]\n" (getVille arc.src) (getVille arc.tgt) arc.lbl);
    fprintf op "}";
  
    close_out op;
    () 



let file = "graphs/graph_villes.txt"


let fichier nom_fichier =

  let string_to_list str =
    let len = String.length str in
    let rec split acc start =
      if start >= len then
        List.rev acc
      else
        match str.[start] with
        | '[' | ';' -> split acc (start + 1)
        | ']' -> List.rev acc
        | _ ->
          let end_pos =
            try String.index_from str start ';'
            with Not_found -> String.index_from str start ']'
          in
          let city = String.sub str start (end_pos - start) in
          split (city :: acc) (end_pos + 1)
    in
    split [] 1
  in
  
  try
    let canal = open_in nom_fichier in
    let oc = open_out file in
    try
      while true do
        let ligne = input_line canal in
        let list_ville = string_to_list ligne in
        let node = List.hd list_ville in
        let _arcs = List.tl list_ville in

        Printf.fprintf oc "n 1 50 %d" (getId node);
        Printf.fprintf oc "\n";
          (*print les arcs dans le fichier*)


        (*List.iter (fun elem -> Printf.fprintf oc "%s " elem) list_ville;*)
        

        
        


        (*Printf.fprintf oc "%s" node;*)
        
        (*print_endline ligne;*)
      done
    with
    | End_of_file -> close_in canal
  with
  | Sys_error msg -> print_endline ("Erreur d'ouverture du fichier : " ^ msg)


  