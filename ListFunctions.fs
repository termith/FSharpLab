// 41.4.1
let list_filter f xs = 
    let folder x acc = 
        match x with 
        | _ when f x -> x :: acc
        | _ -> acc
    List.foldBack folder xs []

// 41.4.2
let sum (p, xs) = 
    let folder acc x = 
        match x with 
        | _ when p x -> x + acc
        | _ -> acc
    List.fold folder 0 xs

// 41.4.3
let revrev = 
    let revert = fun acc x -> x :: acc
    function
    | lst -> List.fold (fun acc x -> (List.fold revert [] x) :: acc) [] lst