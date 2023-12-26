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
  let rec aux g source dest cheminl =  
    let out = out_arcs g source in
    let out2 = List.map (fun (x) -> x.src, x.tgt) out in
    let rec select_arc arclist =
      match arclist with
      | [] -> []
      | (s,t)::rest -> 
        if (List.exists (fun y-> y=s) cheminl) || (t=0) then 
          select_arc rest
        else match aux g s dest (source::cheminl) with
          | [] -> select_arc rest
          | li -> li

    in
    if source=dest then
      dest::cheminl
    else select_arc out2
  in 
  aux g source dest [] ;;
;;
