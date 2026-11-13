// 34.1
let rec upto n = 
    let rec create acc n =
        match n with 
        | 0 -> acc
        | _ -> create (n :: acc) (n - 1)
    create [] n


// 34.2
let rec dnto n = 
    let rec create acc m = 
        match m with 
        | m when m = n -> n :: acc
        | _ -> create (m :: acc) (m + 1)
    create [] 1

// 34.3
let rec evenn n = 
    let rec create acc count =
        match count with 
        | _ when count = -1 -> acc
        | _ -> create (count * 2 :: acc) (count - 1)
    create [] (n - 1)