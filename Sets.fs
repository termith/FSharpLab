// 42.3
let rec allSubsets n k = 
    match n, k with
    | _, 0 -> set [Set.empty]
    | 0, _ -> Set.empty
    | _ ->
        // Все подмножества, в которых есть элемент n
        let X = allSubsets (n - 1) (k - 1)
        // Все подмножества, в которых его нет
        let Y = allSubsets (n - 1) k
        Set.union (Set.map (Set.add n) X) Y
    
        



    