open Gfile
open Tools
open Algo_ford_fulkerson

    
let () =


  

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;

  (*User graph for the application tranformed into graph type*)
  let (graph_ville,src_dest) = fichier "graphs/user_graph.txt" in

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in


  let int_string lbl = int_of_string lbl in
  let string_int lbl = string_of_int lbl in
  let string_tuple (a,b) = (string_of_int a)^"/"^(string_of_int b) in

  (* Exo *)
  let graph = from_file infile in
  let () = export "dot_format_graphs/Exo/graph" graph in

  let graph_int = gmap graph int_string in

  let init_graph = init graph_int in
  let init_graph_print = gmap init_graph string_int in
  let () = export "dot_format_graphs/Exo/init_graph" init_graph_print in

  let ecart_graph = ford_fulkerson init_graph source sink in
  let ecart_graph_print =  gmap ecart_graph string_int in
  let () = export "dot_format_graphs/Exo/ecart_graph" ecart_graph_print in
  
  let flow_graph = transform graph_int ecart_graph in
  let flow_graph_print =  gmap flow_graph string_tuple in
  let () = export "dot_format_graphs/Exo/flow_graph" flow_graph_print in
  

(*Application : Chemin de Fret d'une ville A à une ville B*)

  let graph_ville_print = gmap graph_ville string_int in
  let () = export "dot_format_graphs/Application/graph" graph_ville_print in

  let init_graph_ville = init graph_ville in
  let init_graph_ville_print = gmap init_graph_ville string_int in
  let () = export "dot_format_graphs/Application/init_graph" init_graph_ville_print in

  let ecart_graph_ville = ford_fulkerson init_graph_ville (List.nth src_dest 0) (List.nth src_dest 1) in
  let ecart_graph_ville_print =  gmap ecart_graph_ville string_int in
  let () = export "dot_format_graphs/Application/ecart_graph" ecart_graph_ville_print in

  let flow_graph_ville = transform graph_ville ecart_graph_ville in
  let flow_graph_ville_print =  gmap flow_graph_ville string_tuple in
  let () = export_ville "dot_format_graphs/Application/flow_graph_fret" flow_graph_ville_print in

  



  (*let () = write_file outfile flow_graph_print in*)
  let () = write_file outfile flow_graph_ville_print in
  ()

