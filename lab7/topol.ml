(** Sortowanie topologiczne *)

(* Code author: Michał Radwański (290472)
 * Code review: Małgorzata Biegała (406094)
 * *)

exception Cykliczne


(* Code uses DFS-based algorithm described on Wikipedia:
 * https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
 * *)

(* each node is marked with one of three possibilities, which are described in
 * the Wikipedia algorithm *)
type mark = None | Temporary | Permanent


(* find_default returns the value of key in the map or default provided value,
 * if none is found *)
let find_default x map default =
    if PMap.mem x map then PMap.find x map else default


(* visitor function for DFS *)
let rec visit node adjacency_map mark_map topol_order_list =
    match find_default node mark_map None with
    (* node has been visited, but it's not included in result, the graph is not
     * a DAG *)
    | Temporary -> raise Cykliczne
    (* node has been visited and included in result, no further work is needed*)
    | Permanent -> (mark_map, topol_order_list) 
    (* node hasn't been visited *)
    | None      -> 
        let mark_map = PMap.add node Temporary mark_map in
        let neighbours = find_default node adjacency_map [] in
        let (mark_map, topol_order_list) = List.fold_left 
            (fun (mark_map_, topol_order_list_) node_ -> 
                visit node_ adjacency_map mark_map_ topol_order_list_)
            (mark_map, topol_order_list)
            neighbours
        in let mark_map = PMap.add node Permanent mark_map
        in let topol_order_list = node::topol_order_list in
        (mark_map, topol_order_list)


(* for maps of type 'a -> 'b list, key of type 'a and list_ of type 'b list 
 * returns a map for which (find key map = list_) if key wasn't present and
 * (find key map = list_ @ (find key previous_map)) otherwise *)
let add_list key list_ map =
    let old = find_default key map [] in
    (* runs in linear time w.r.t total length of lists that are added to given
     * key *)
    PMap.add key (list_@old) map


(* transform list described in input into a map, which for each node holds a 
 * list of its neighbours *)
let rec parse list_ adjacency_map =
    match list_ with
    | [] -> adjacency_map
    | (node, neighbours)::tail ->
        parse tail (add_list node neighbours adjacency_map)

let topol list_ =
    let adj_map = parse list_ PMap.empty
    (* consolidate results *)
    in let (_, topol_lst_) = PMap.foldi
        (fun key _ (mark_map, topol_lst) -> (visit key adj_map mark_map topol_lst))
        adj_map
        (PMap.empty, [])
    in topol_lst_
