open Graph

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val create_add_arc2: (int * int) graph -> id -> id -> (int * int) -> (int * int) graph
