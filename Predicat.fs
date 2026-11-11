// 16.1
let notDivisible (n, m) = m % n = 0

let rec isPrime (n, limit, d) =
    function
    | d when d > limit -> true
    | n when n % d = 0 -> false
    | n -> isPrime (n, limit, d + 2)


// 16.2
let prime =
    function
    | n when n < 4 -> true
    | n when n % 2 = 0 -> false
    | n -> isPrime (n, int(System.Math.Sqrt (float(n))), 3)
