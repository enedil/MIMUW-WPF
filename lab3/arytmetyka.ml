
type przedzial = { a : float; b : float }

(* wartość jest posortowaną listą rozłącznych przedziałów*)
type wartosc = przedzial list

let wartosc_od_do (x : float) (y : float) : wartosc =
    [{a = x ; b = y}]

let wartosc_dokladnosc (x : float) (p: float) : wartosc =
    let (fac1, fac2) = 
        if x < 0.0 then (1.0 +. p /. 100., 1.0 -. p /. 100.)
        else (1.0 -. p /. 100., 1.0 +. p /. 100.)
    in wartosc_od_do (x *. fac1) (x *. fac2)

let wartosc_dokladna (x : float) : wartosc =
    wartosc_od_do x x

let in_przedzial (p : przedzial) (f : float) : bool =
    f >= p.a && f <= p.b

let rec in_wartosc (w : wartosc) (f : float) : bool =
    match w with
    | []   -> false
    | h::t -> (in_przedzial h f) || (in_wartosc t f)

let min_wartosc (w : wartosc) : float =
    match w with
    | []   -> neg_infinity
    | h::t -> h.a

let rec max_wartosc (w : wartosc) : float =
    match w with
    | []    -> infinity
    | h::[] -> h.b
    | h::t  -> max_wartosc t

let sr_wartosc (w : wartosc) : float =
    if classify_float (min_wartosc w) = FP_infinite || classify_float (max_wartosc w) = FP_infinite then nan else
    let rec sr (lst : wartosc) (acc_suma : float) (acc_licznik : float) : float =
        match lst with
        | []   -> if acc_licznik = 0.0 then 0.0 else acc_suma /. acc_licznik /. 2.0
        | h::t -> sr t (acc_suma +. h.a +. h.b) (acc_licznik +. 1.0)
    in sr w 0.0 0.0

(* posortowane nierosnąco *)
let suma_przedzial (p : przedzial) (q : przedzial) : wartosc =
    if p.b < q.a then [q; p]
    else if q.b < p.a then [p; q]
    else wartosc_od_do (min p.a q.a) (max p.b q.b)


let uprosc_nachodzace (w : wartosc) : wartosc = 
    let rec pom (lst : wartosc) (acc : wartosc) : wartosc =
        match (lst, acc) with
        | ([], _)          -> acc
        | (hl::tl, ha::ta) -> pom tl ((suma_przedzial hl ha) @ ta)
        | (hl::tl, [])     -> pom tl [hl]
    in List.rev (pom w [])


let plus_razy (fn : przedzial -> przedzial -> przedzial) (w : wartosc) (v : wartosc) : wartosc =
    let rec petla1 (lst1 : wartosc) (acc : wartosc) : wartosc =
        match lst1 with
        | []   -> acc
        | x::t -> petla1 t (petla2 v x acc)
    and petla2 lst2 (x : przedzial) (acc : wartosc) : wartosc =
        match lst2 with
        | []   -> acc
        | y::t -> petla2 t x ((fn x y)::acc) 
    in let lst = petla1 w []
    in let lst = List.sort compare lst
    in uprosc_nachodzace lst

let plus_przedzial (p : przedzial) (q : przedzial) : przedzial =
    {a = p.a +. q.a; b = p.b +. q.b}

let plus = plus_razy plus_przedzial

let razy_minus1 (w : wartosc) : wartosc =
    let rec pom (lst : wartosc) (acc : wartosc) = 
        match lst with
        | []   -> acc
        | h::t -> pom t (({a = (-.h.b); b = (-.h.a)})::acc)
    in pom w []

let minus (w : wartosc) (v : wartosc) : wartosc =
    let minus_v = razy_minus1 v 
    in plus w minus_v

let rozdziel_zero (w : wartosc) : wartosc =
    let rec pom (lst : wartosc) (acc : wartosc) = 
        match lst with
        | []   -> acc
        | h::t -> let part =  
            if in_przedzial h 0.0 then 
                [{a = 0.0; b = h.b}; {a = h.a; b = (-0.0)}]
            else 
                [h]
        in pom t (part @ acc)
    in List.rev (pom w [])


let is_not_nan (f : float) =
    not (classify_float f = FP_nan)

(* przedziały nie powinny zawierać liczb różnych znaków *)
let razy_przedzial (p : przedzial) (q : przedzial) : przedzial =
    let lst = [p.a *. q.a; p.a *. q.b; p.b *. q.a; p.b *. q.b] 
    in let lst = List.filter is_not_nan lst 
    in
    {
        a = List.fold_left min nan lst;
        b = List.fold_left max nan lst;
    }

let razy (w : wartosc) (v : wartosc) : wartosc = 
    plus_razy razy_przedzial (rozdziel_zero w) (rozdziel_zero v)

let odwroc (w : wartosc) : wartosc =
    let lst = rozdziel_zero w in
    let lst = List.filter (fun p -> (not (p.a = 0.0) || not (p.b = 0.0))) lst in
    let lst = List.map (fun p -> {a = 1.0 /. p.b; b = 1.0 /. p.a}) lst in
    let lst = List.sort compare lst in
    let lst = uprosc_nachodzace lst in
    match lst with
    | [] -> wartosc_od_do nan nan
    | _  -> lst

let podzielic (w : wartosc) (v : wartosc) : wartosc =
    let odw = odwroc v
    in razy w odw
