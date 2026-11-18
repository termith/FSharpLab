let factorial n =
    let rec f x a =
        if x <= 1 then a
        else f (x - 1) (a * x)
    f n 1

// 50.2.1
let fac_seq = seq {
    yield! (Seq.initInfinite factorial)
}

let rec f n = 
    match n with
    | 0 -> 0
    | _ -> (f (n - 1)) + int ((-1.0) ** float n) * n
// 50.2.2
let seq_seq = seq {
    yield! (Seq.initInfinite f)
}