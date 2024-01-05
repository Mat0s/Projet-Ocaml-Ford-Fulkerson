open Gfile
open Tools
(*open Printf*)
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


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (*let result = clone_nodes graph in*)

  let f lbl = int_of_string lbl in
  let f1 lbl = string_of_int lbl in
  let result2 = gmap graph f in
  (*let result3 = gmap result2 f1 in*)

  (*let result4 = add_arc result2 3 1 100 in*)
  
  let init_gr = init result2 in
  let init_gr2 = gmap init_gr f1 in

  let () = export "dot_format_graphs/init_gr" init_gr2 in

  (*let test_find_path = find_path init_gr 0 5 in

  let () = printf "path from 0 to 5 in graph 1 : [" in
  let () = List.iter (printf "%d ") test_find_path in
  let () = printf "]" in

  let find_capacity_min_test = find_capacity_min init_gr test_find_path in
  let () = printf "find_capacity_min_test : %d" find_capacity_min_test in

  

  let increase_flow_test = increase_flow init_gr test_find_path find_capacity_min_test in
  let increase_flow_graph =  gmap increase_flow_test f1 in
  let () = export "dot_format_graphs/increase_flow_graph" increase_flow_graph in*)


  let final_graph_test = ford_fulkerson init_gr 0 5 in
  let final_graph =  gmap final_graph_test f1 in
  let () = export "dot_format_graphs/final_graph" final_graph in


  (* Rewrite the graph that has been read. *)
  let () = write_file outfile final_graph in

  ()

