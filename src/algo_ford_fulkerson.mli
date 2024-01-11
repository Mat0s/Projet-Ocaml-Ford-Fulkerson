open Graph

val init : int graph -> int graph

val find_path : int graph -> id -> id -> id list


val find_capacity_min : int graph -> id list -> int

val increase_flow : int graph -> id list -> int -> int graph

val update_arcs2 : int graph ->'a arc -> int -> int graph

val ford_fulkerson :int graph -> id -> id -> int graph

val transform : int graph -> int graph -> (int * int) graph