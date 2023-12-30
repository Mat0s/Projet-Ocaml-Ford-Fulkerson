open Graph

val init : int graph -> int graph

val find_path : int graph -> id -> id -> id list

val update_arcs2 : int graph ->'a arc -> int -> int graph

val update_graph : int graph -> id list -> int graph