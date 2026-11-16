// 40.1
let rec sum (p: int -> bool, xs) = 
    match xs with
        | [] -> 0
        | head :: tail when p head -> head + sum (p, tail)
        | _ :: tail -> sum (p, tail)

// 40.2.1
let rec count (xs: int list, n: int) = 
    let rec counter cnt = function
        | [] -> cnt
        | head :: tail when head = n -> counter (cnt + 1) tail
        | head :: tail when head > n -> cnt
        | _ :: tail -> counter cnt tail
    counter 0 xs

// 40.2.2
let rec insert (xs: int list, n: int) = 
    match xs with 
    | [] -> [n]
    | head :: tail when head >= n -> [n] @ (head :: tail)
    | head :: tail -> [head] @ insert (tail, n)

// 40.2.3
let rec intersect (xs1: int list, xs2: int list) = 
    match xs1, xs2 with
    | [], [] -> []
    | [], _ -> [] 
    | _, [] -> []
    | head1::tail1, head2::tail2 when head1 = head2 -> [head1] @ intersect (tail1, tail2)
    | head1::tail1, head2::tail2 when head1 > head2 -> intersect ([head1] @ tail1, tail2)
    | head1::tail1, head2::tail2 when head1 < head2 -> intersect (tail1, [head2] @ tail2)

// 40.2.4
let rec plus (xs1: int list, xs2: int list) = 
    match xs1, xs2 with 
    | [], [] -> []
    | [], xs -> xs
    | xs, [] -> xs
    | head1::tail1, head2::tail2 when head1 > head2 -> [head2] @ plus ([head1] @ tail1, tail2)
    | head1::tail1, head2::tail2 when head1 <= head2 -> [head1] @ plus (tail1, [head2] @ tail2)

// 40.2.5
let rec minus (xs1: int list, xs2: int list) =
    match xs1, xs2 with 
    | [], [] -> []
    | [], xs -> []
    | xs, [] -> xs
    | head1::tail1, head2::tail2 when head1 = head2 -> minus (tail1, tail2)
    | head1::tail1, head2::tail2 when head1 > head2 -> minus ([head1] @ tail1, tail2)
    | head1::tail1, head2::tail2 when head1 < head2 -> [head1] @ minus (tail1, [head2] @ tail2)

// 40.3.1
let smallest =
    let rec finder current = function
        | [] -> Some current
        | head :: tail when head < current -> finder head tail
        | _ :: tail -> finder current tail

    function
    | [] -> None
    | first :: tail -> finder first tail

// 40.3.2
let rec delete (n: int, xs: int list) = 
    match xs with 
    | [] -> []
    | head :: tail when head = n -> tail
    | head :: tail -> [head] @ delete (n, tail)

// 40.3.3
let rec sort = function
    | [] -> []
    | xs -> 
        let n = Option.get (smallest xs)
        n :: sort(delete(n, xs))

// 40.4
let rec revrev = function
    | [] -> []
    | head :: tail -> revrev tail @ [List.rev head]