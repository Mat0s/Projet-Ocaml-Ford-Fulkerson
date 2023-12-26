open Graph

val init: int graph -> int graph

(* On donne un graphe + les id des nodes source & puits -> la fonction renvoie un chemin & la valeur de flot minimale sur ce chemin *)
val find_path : int graph -> id -> id -> id list

val update_arcs: int graph -> 'a arc -> int -> int graph


