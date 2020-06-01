let factorial a =
    let rec helper a acc = 
        if a < 2
        then acc
        else helper (a - 1) (a * acc)
    in helper a 1

let fib n = 
    let rec helper a b i = 
        if i < 1
        then a
        else helper b (a + b) (i - 1)
    in helper 0 1 n

let mul (a, b, c, d) (e, f, g, h) =
    (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)

let pow mat n =
    let rec helper a n acc = 
        if n = 0 
        then acc
        else if n mod 2 = 1
            then helper a (n - 1) (mul a acc)
            else helper (mul a a) (n / 2) acc
    in helper mat n (1, 0, 0, 1)

let fast_fib n = 
    match (pow (1, 1, 1, 0) n) with (_, b, _, _) -> b

