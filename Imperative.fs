// 47.4.1
let f n = 
    let mutable count = 1
    let mutable res = 1
    while count < n + 1 do
        res <- res * count
        count <- count + 1
    res 

// 47.4.2
let fibo n = 
    let mutable first = 0
    let mutable second = 1
    let mutable count = n
    while count <> 0 do
        let tmp = second
        second <- first + second
        first <- tmp
        count <- count - 1
    first