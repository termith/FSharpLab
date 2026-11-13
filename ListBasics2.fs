// 39.1
let rec rmodd = function
    | [] -> []
    | [n] -> [n]
    | _ :: n :: tail -> n :: rmodd tail

// 39.2
let rec del_even = function 
    | [] -> []
    | n :: tail when n % 2 <> 0 -> n :: del_even tail
    | _ :: tail -> del_even tail

// 39.3
let rec multiplicity x xs = 
    let rec count cnt  = function
        | [] -> cnt
        | head :: tail when head = x -> count (cnt + 1) tail
        | head :: tail -> count cnt tail
    count 0 xs
            

// 39.4
let rec split xs =
    let rec splitter acc_odd acc_even = function 
        | [] -> (acc_odd, acc_even)
        | [n] -> (n :: acc_odd, acc_even)
        | first :: second :: tail -> splitter (first :: acc_odd) (second :: acc_even) tail
    let odds, evens = splitter [] [] xs
    (List.rev odds, List.rev evens)

// 39.5
let rec zip (xs1,xs2) = 
    let rec zipper acc x1 x2 = 
        match x1, x2 with 
            | [], [] -> acc
            | head1 :: tail1, head2 :: tail2 -> zipper ((head1, head2) :: acc) tail1 tail2
            | _ -> failwith "Lengths are different"
    let res = zipper [] xs1 xs2
    List.rev res
        