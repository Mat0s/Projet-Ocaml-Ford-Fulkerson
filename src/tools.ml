(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph


let gmap gr f = 
  e_fold gr (fun acc arc -> new_arc acc {arc with lbl = f arc.lbl}) (clone_nodes gr)


let add_arc g id1 id2 n = 
  let update_label lbl = match lbl with l -> (l+n) in
  match find_arc g id1 id2 with
  | Some arc -> new_arc g {arc with lbl = update_label arc.lbl}
  | None -> new_arc g {src = id1; tgt = id2; lbl = n}


let create_add_arc2 g id1 id2 (a,b) = 
  match find_arc g id1 id2 with
  | None -> new_arc g {src = id1; tgt = id2; lbl = (a,b)}
  | _ -> g
