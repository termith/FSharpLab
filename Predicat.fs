// 16.1
let notDivisible (n, m) = m % n = 0

let rec isPrime (n, limit, d) =
    match n with
    | x when d > limit -> true
    | x when x % d = 0 -> false
    | x -> isPrime (x, limit, d + 2)


// 16.2
let prime =
    function
    | n when n < 4 -> true
    | n when n % 2 = 0 -> false
    | n -> isPrime (n, int(System.Math.Sqrt (float(n))), 3)
