(* Twórca: Michał Radwański (395415) *)

type t = 
    | Empty
    | Interval of t * (int * int) * t * int * int64
    (* letf_subtree, (a, b), right_subtree, height, size *)

let empty = Empty


let is_empty s = 
    match s with
    | Empty -> true
    | _     -> false


let height s = 
    match s with
    | Empty -> 0
    | Interval(_, _, _, h, _) -> h

let size s =
    match s with
    | Empty -> 0L
    | Interval(_, _, _, _, sz) -> sz

let interval_size a b =
    Int64.succ (Int64.sub (Int64.of_int b) (Int64.of_int a))

(* copied with minor modifications *)
let make l (a, b) r = 
    Interval(l, (a, b), r, max (height l) (height r) + 1, Int64.add (interval_size a b) (Int64.add (size l) (size r)))


(* copied with minor modifications *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Interval (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Interval (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Interval (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Interval (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Interval (l, k, r, max hl hr + 1, Int64.add (interval_size (fst k) (snd k)) (Int64.add (size l) (size r)))


(* s shouldn't contain any of numbers between x and y (including ends) *)
let rec add_disjoint (x, y) s =
    match s with
    | Empty -> Interval(Empty, (x, y), Empty, 1, interval_size x y) 
    | Interval(sl, (a, b), sr, h, sz) ->
        (*if a <= x && y <= b then Interval(sl, (a, b), sr, h, Int64.add (interval_size a b) sz)
        else*) if b+1 < x && b < max_int then
            let nowe = add_disjoint (x, y) sr in
            bal sl (a, b) nowe
        else if y+1 < a && y < max_int then
            let nowe = add_disjoint (x, y) sl in
            bal nowe (a, b) sr
        else assert false


(* works assuming all elements of l are less then all elements of r *)
(* and v is in between *)
let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_disjoint v r
  | (_, Empty) -> add_disjoint v l
  | (Interval(ll, lv, lr, lh, _), Interval(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* extracts balanced AVL tree containing elements of s which are    *)
(* strictly smaller than k *)
let rec set_of_smaller k s =
    match s with
    | Empty -> Empty
    | Interval(l, (a, b), r, _, _) ->
        if k < a then set_of_smaller k l
        else if k > b then bal l (a, b) (set_of_smaller k r)
        else if a = k then l
        else bal l (a, k-1) Empty


(* extracts balanced AVL tree containing elements of s which are    *)
(* strictly greater than k *)
let rec set_of_greater k s =
    match s with
    | Empty -> Empty
    | Interval(l, (a, b), r, _, _) ->
        if k > b then set_of_greater k r
        else if k < a then bal (set_of_greater k l) (a, b) r
        else if b = k then r
        else bal Empty (k+1, b) r


(* copied with minor modifications *)
let rec min_elt s =
    match s with
    | Interval (Empty, k, _, _, _) -> k
    | Interval (l, _, _, _, _) -> min_elt l
    | Empty -> assert false


(* copied with minor modifications *)
let rec max_elt s =
    match s with
    | Interval (_, k, Empty, _, _) -> k
    | Interval (_, _, r, _, _) -> max_elt r
    | Empty -> assert false


(* copied with minor modifications *)
let rec remove_min_elt s =
    match s with
    | Interval (Empty, _, r, _, _) -> r
    | Interval (l, k, r, _, _) -> bal (remove_min_elt l) k r
    | Empty -> invalid_arg "ISet.remove_min_elt"


(* copied with minor modifications *)
let rec remove_max_elt s =
    match s with
    | Interval (l, _, Empty, _, _) -> l
    | Interval (l, k, r, _, _) -> bal l k (remove_max_elt r)
    | Empty -> invalid_arg "ISet.remove_min_elt"


let remove (x, y) s =
    let greater_than_y = set_of_greater y s in
    match greater_than_y with
    | Empty -> (set_of_smaller x s)
    | _ -> join (set_of_smaller x s) (min_elt greater_than_y) (remove_min_elt greater_than_y)



let add (x, y) s =
    let smaller = set_of_smaller x s and
        greater = set_of_greater y s in
    match (smaller, greater) with
    | (Empty, Empty) -> add_disjoint (x, y) empty
    | (Empty, _)     -> 
        let (a, b) = min_elt greater in
        if y + 1 >= a then add_disjoint (x, b) (remove_min_elt greater)
        else add_disjoint (x, y) greater
    | (_, Empty)     ->
        let (a, b) = max_elt smaller in
        if b + 1 >= x then add_disjoint (a, y) (remove_max_elt smaller)
        else add_disjoint (x, y) smaller
    | _ ->
    let (left_maxa, left_maxb) = max_elt smaller and
        (right_mina, right_minb) = min_elt greater in
    let (smaller, x) = 
        if left_maxb + 1 = x then
            (remove_max_elt smaller, left_maxa)
        else
            (smaller, x) in
    let (greater, y) =
        if right_mina - 1 = y then
            (remove_min_elt greater, right_minb)
        else
            (greater, y) in
    join smaller (x, y) greater
    

let rec mem n s = 
    match s with
    | Empty -> false
    | Interval(sl, (a, b), sr, _, _) ->
        if a <= n && n <= b then true
        else if n < a then mem n sl
        else mem n sr


let rec iter f s =
    match s with
    | Empty -> ()
    | Interval(sl, v, sr, _, _) ->
        iter f sl; 
        f v; 
        iter f sr


let fold f s acc =
    let rec pom acc s = 
        match s with
        | Empty -> acc
        | Interval(l, v, r, _, _) ->
            pom (f v (pom acc l)) r in pom acc s


let elements s =
    List.rev (fold (fun v acc -> v::acc) s [])


let below x s =
    let max63 = Int64.of_int max_int in
    let suma = size (set_of_smaller x s) in
    let suma = if suma < 0L then max63 else suma in
    Int64.to_int (min max63 (Int64.add (min max63 suma) (if mem x s then 1L else 0L)))


let split x s =
    (set_of_smaller x s, mem x s, set_of_greater x s)
