
(* Node is of a left subtree, right subtree, value of the node and right height  *)
type 'a queue =
    | Leaf
    | Node of 'a queue * 'a queue * 'a * int


let empty : 'a queue =
    Leaf


let is_empty (q : 'a queue) : bool =
    q = empty


exception Empty


let value (q : 'a queue) : 'a =
    match q with
    | Leaf -> raise Empty
    | Node(_, _, v, _) -> v

    
let height (q : 'a queue) : int =
    match q with
    | Leaf -> 0
    | Node(_, _, _, h) -> h


let rec join (q1 : 'a queue) (q2 : 'a queue) : 'a queue =
    (* value in the root of q1 is not greater than in q2 *)
    let helper (q1 : 'a queue) (q2 : 'a queue) : 'a queue = 
        let Node(l1, r1, v1, h1) = q1
        in let joined = join r1 q2
        in if (height l1) > (height joined) then
            Node(l1, joined, v1, (height joined) + 1)
        else
            Node(joined, l1, v1, (height l1) + 1)

    in match (q1, q2) with 
    | (Leaf, q) -> q
    | (q, Leaf) -> q
    | _ -> if (value q1) < (value q2) then helper q1 q2 else helper q2 q1



let delete_min (q : 'a queue) : 'a * 'a queue =
    if is_empty q then raise Empty
    (* deleting node means deleting top element and joining subtrees *)
    else let Node(lq, rq, vq, _) = q
    in (vq, join lq rq)


let add (e : 'a) (q : 'a queue) : 'a queue =
    join (Node(Leaf, Leaf, e, 1)) q

let q = add 7 (add 2 (add 5 empty));;
let (a, b) = delete_min q;;
assert(a = 2);;
