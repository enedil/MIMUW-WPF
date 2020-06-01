(* Autor:   Michał Radwański (395415) *)
(* Review:  Jacek Jakimiuk   (406165) *)


type point = float * float


type kartka = point -> int


let eps = 1e-10


let prostokat p1 p2 =
    let in_interval x1 x2 x =
        abs_float(x1 -. x) <= eps || abs_float(x2 -. x) <= eps || x1 <= x && x <= x2 
    in
    let k p =
        let (x1, y1) = p1 and
            (x2, y2) = p2 and
            (x , y ) = p in
        if in_interval x1 x2 x && in_interval y1 y2 y
        then 1
        else 0
    in k


let kolko o r =
    let sq x = 
        x *. x in
    let k p =
        let (xo, yo) = o and
            (xp, yp) = p in
        if sq (xo -. xp) +. sq (yo -. yp) -.  sq r < eps
        then 1
        else 0
    in k


(* which_direction a b c gives the side of c relative to line ab *)
type direction = Left | Right | Collinear

let which_direction (p1x, p1y) (p2x, p2y) (p3x, p3y) =
    let cross_prod = (p2x -. p1x)*.(p3y -. p1y) -. (p2y -. p1y)*.(p3x -. p1x) in
    if abs_float(cross_prod) < eps then Collinear
    else if cross_prod > 0.0 then Left
    else Right


let negate (x1, x2) =
    (-. x1, -. x2)


let translate (x1, y1) (x2, y2) =
    (x1 +. x2, y1 +. y2)


let rotate (x, y) phi =
    let sinphi = sin phi and
        cosphi = cos phi in
    (x *. cosphi -. y *. sinphi, x *. sinphi +. y *. cosphi)


let x_axis_reflect (x, y) = (x, -.y)


(* reflection is achieved by rotationg whole plane so that line p1p2 is the Ox axis,
 * conjugating the rotated vector and reversing the process *)
let reflect p1 p2 p =
    let (rx, ry)        = translate p2 (negate p1)                      in
    let phi             = atan2 ry rx                                   in
    let rotated         = rotate (translate p (negate p1)) (-.phi)      in
    let reflected       = x_axis_reflect rotated                        in
    let unrotated       = rotate reflected phi                          in
    let untranslated    = translate unrotated p1                        in
    untranslated 


let zloz p1 p2 kartka =
    let k r =
        match which_direction p1 p2 r with
        | Right ->      0
        | Collinear ->  kartka r
        | Left ->       kartka r + kartka (reflect p1 p2 r)
    in k


let skladaj l k =
    List.fold_left (fun k (p1, p2) -> zloz p1 p2 k) k l
