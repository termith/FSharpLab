// 20.3.1
let vat n x = (float n) / 100.0 * x + x

// 20.3.2
let unvat n x = x / ((float n) / 100.0 + 1.0)

// 20.3.3
let rec min f = 
    let rec find n = 
        match n with 
        | n when f n = 0 -> n
        | n -> find (n + 1)
    find 0

