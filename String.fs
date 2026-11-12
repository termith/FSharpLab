// 17.1
let rec pow (s, n) = 
    match n with 
    | 0 -> ""
    | 1 -> s
    | _ -> s + pow(s, n - 1)

// 17.2
let isIthChar (s, n, c) = 
    let l = String.length s
    match l with 
    | l when l > n -> s.[n] = c
    | _ -> false

// 17.3
let rec occFromIth (s, n, c) = 
    let l = String.length s
    match n with
    | n when (n >= l) -> 0
    | n when (s.[n] = c) -> 1 + occFromIth(s, n + 1, c)
    | n when (s.[n] <> c) -> 0 + occFromIth(s, n + 1, c)
    | _ -> 0