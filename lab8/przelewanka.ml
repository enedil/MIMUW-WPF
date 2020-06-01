
(* największy wspólny dzielnik liczb a, b *)
let rec gcd a b = 
    if b = 0 then 
        a
    else
        gcd b (a mod b)

(* największy wspólny dzielnik l_0, l_1, l_2, ... dla listy [l_0;l_1;l_2;...] *)
let gcd_list = List.fold_left gcd 0


(* przelewa wodę z kubka o numerze i do kubka o numerze j *)
let przelej pojemnosci poziomy (i, j) =
    let poziomy = Array.copy poziomy in
    if i = j then poziomy else
    let level1 = poziomy.(i) and level2 = poziomy.(j) in
    let max2 = pojemnosci.(j) in
    let (nowy1, nowy2) = 
        if level1 + level2 <= max2 then
            (0, level1 + level2)
        else
            (level1 + level2 - max2, max2)
    in 
        poziomy.(i) <- nowy1;
        poziomy.(j) <- nowy2;
        poziomy


(* opróżnia kubek o numerze i *)
let oproznij poziomy i =
    let poziomy = Array.copy poziomy in
    poziomy.(i) <- 0;
    poziomy


(* napełnia kubek o numerze i *)
let napelnij pojemnosci poziomy i =
    let poziomy = Array.copy poziomy in
    poziomy.(i) <- pojemnosci.(i);
    poziomy



let odwiedz hashmap len node =
    if not (Hashtbl.mem hashmap node) then Hashtbl.add hashmap node len


let jeszcze_niepoznany m x = not (Hashtbl.mem m x)


(* funkcja filtruje wierzchołki, do których nie powinniśmy się wybierać *)
(* funkcja jest w łatwy sposób rozszerzalna, wystarczy dodać kolejne linijki *)
(* postaci "|> List.filter predykat" *)
let filtruj drogi mapa_odwiedzonych =
    drogi 
    |> List.filter (jeszcze_niepoznany mapa_odwiedzonych)


(* zwraca listę liczb całkowitychod początku do końca (przedział
 * domknięto-otwarty) *)
let zakres poczatek koniec =
    if poczatek >= koniec then [] else
    let rec loop i koniec acc =
        if i = koniec then acc
        else loop (i+1) koniec (i::acc)
    in
    List.rev (loop poczatek koniec [])


(* iloczyn kartezjański list, to lista par, których pierwszy element znajduje
 * się na pierwszej liście, a drugi element na drugiej *)
let iloczyn_kartezjanski l1 l2 =
    List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) l2) l1)


(* zwraca listę nieodwiedzonych sąsiadów wierzchołka *)
let krawedzie_z_wierzcholka pojemnosci wierzcholek mapa_odwiedzonych =
    let n = Array.length wierzcholek in
    let indeksy = zakres 0 n in
    let oproznione = List.map (oproznij wierzcholek) indeksy and
        napelnione = List.map (napelnij pojemnosci wierzcholek) indeksy and
        przelewane = List.map (przelej pojemnosci wierzcholek)
        (iloczyn_kartezjanski indeksy indeksy) in
    let drogi = oproznione @ napelnione @ przelewane in
    filtruj drogi mapa_odwiedzonych

let print_array a = 
    print_string "[|";
    Array.iter (fun x -> print_int x; print_char ';') a;
    print_string "|]";;

let print_map m =
    Hashtbl.iter (fun x y -> print_array x; 
                            print_string ": "; 
                               print_int y;
                               print_newline()) m;;

(* funkcja realizuje BFS ustawiając w mapa_odw odległości od wierzchołków,
 * które przed wywołaniem funkcji znajdują się w kolejce
 * dla pustej kolejki, funkcja zakańcza natychmiastowo swoje działanie *)
let rec wyszukaj_wszerz pojemnosci cel kolejka mapa_odw =
    if Queue.length kolejka <> 0 then 
        let top = Queue.pop kolejka in
        let dist = Hashtbl.find mapa_odw top in
        if top <> cel then
            let krawedzie = krawedzie_z_wierzcholka pojemnosci top mapa_odw in
            (* funkcja odwiedzająca ustawia wartość tylko dla niepoznanych
             * wierzchołków - jeśli bowiem były wcześniej odwiedzone, to są nie
             * dalej niż obecnie przetwarzany *)
            List.iter (odwiedz mapa_odw (dist + 1)) krawedzie;
            if not (jeszcze_niepoznany mapa_odw cel) then
                Queue.clear kolejka
            else
                List.iter (fun x -> Queue.add x kolejka) krawedzie;
                wyszukaj_wszerz pojemnosci cel kolejka mapa_odw


let przelewanka tablica =
    (* pozbywam się szklanek o pojemności 0, bo nie wpływają na wynik *)
    let tablica = Array.of_list 
                  (List.filter (fun (x,_) -> x>0) (Array.to_list tablica)) in
    let n = Array.length tablica in
    (* jeśli tablica jest pusta, to nie trzeba nic przelewać *)
    if n = 0 then 0 else
    let pojemnosci = Array.map fst tablica and
        cel        = Array.map snd tablica in
    (* po każdej operacji przynajmniej jedna ze szklanek jest pusta bądź pełna
     * jeśli końcowy stan tego nie uwzględnia, to znaczy że z pewnością nie da
     * się do niego dotrzeć *)
    if Array.for_all (fun x->x) 
        (Array.map2 (fun x y -> x != 0 && x != y) cel pojemnosci) 
    then (-1) else
        (* jeśli NWD wielkości szklanek nie dzieli któregoś z oczekiwanych
         * poziomów, to z pewnością nie da się dotrzeć do takiego stanu, gdyż
         * każda operacja zachowuje podzielność poziomów wody przez owe NWD *)
        let gcd = gcd_list (Array.to_list pojemnosci) in
        if not (Array.for_all (fun x -> x mod gcd = 0) cel) then (-1) else
            let kolejka    = Queue.create() in
            let mapa_odw   = Hashtbl.create 1000000 in
            Queue.add (Array.make n 0) kolejka;
            Hashtbl.add mapa_odw (Array.make n 0) 0;
            wyszukaj_wszerz pojemnosci cel kolejka mapa_odw;
            if jeszcze_niepoznany mapa_odw cel then (-1)
            else Hashtbl.find mapa_odw cel;;
