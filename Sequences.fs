// 49.5.1
let even_seq = Seq.initInfinite (fun i -> 2 * i)

let factorial n =
    let rec f x a =
        if x <= 1 then a
        else f (x - 1) (a * x)
    f n 1
// 49.5.2
let fac_seq = Seq.initInfinite (fun i -> factorial i)

let rec f n = 
    match n with
    | 0 -> 0
    | _ -> (f (n - 1)) + int ((-1.0) ** float n) * n
// 49.5.3
let seq_seq = Seq.initInfinite f