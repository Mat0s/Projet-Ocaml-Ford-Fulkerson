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


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  let () = export "dot_format_graphs/graph" graph in

  let int_string lbl = int_of_string lbl in
  let string_int lbl = string_of_int lbl in
  let string_tuple (a,b) = (string_of_int a)^"/"^(string_of_int b) in

  let graph_int = gmap graph int_string in

  
  let init_graph = init graph_int in
  let init_graph_print = gmap init_graph string_int in

  let () = export "dot_format_graphs/init_graph" init_graph_print in

  let ecart_graph = ford_fulkerson init_graph 0 5 in
  let ecart_graph_print =  gmap ecart_graph string_int in
  let () = export "dot_format_graphs/ecart_graph" ecart_graph_print in

  let flow_graph = transform graph_int ecart_graph in
  let flow_graph_print =  gmap flow_graph string_tuple in
  let () = export "dot_format_graphs/flow_graph" flow_graph_print in


  let () = write_file outfile flow_graph_print in

  ()

