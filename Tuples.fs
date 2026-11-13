// 23.4.1
let (.+.) x y = 
    let gold_x, silver_x, cupper_x = x
    let gold_y, silver_y, cupper_y = y
    
    let raw_result_cupper = cupper_x + cupper_y
    let raw_result_silver = (silver_x + silver_y) + raw_result_cupper / 12
    let raw_result_gold = (gold_x + gold_y) + raw_result_silver / 20
    
    (raw_result_gold, raw_result_silver % 20, raw_result_cupper % 12)

let (.-.) x y = 
    let gold_x, silver_x, cupper_x = x
    let gold_y, silver_y, cupper_y = y
    let total_x = cupper_x + silver_x * 12 + gold_x * 20 * 12
    let total_y = cupper_y + silver_y * 12 + gold_y * 20 * 12
    let result = total_x - total_y
    if result <= 0 then
        (0, 0, 0)
    else
        let gold = result / (20 * 12)
        let silver = (result - (gold * 20 * 12)) / 12
        let cupper = result - (gold * 20 * 12) - silver * 12
        (gold, cupper, silver)

// 23.4.2
let (.+) (x: float * float) (y: float * float) = 
    let a, b = x
    let c, d = y
    (a + c, b + d)
    
let (.-) (x: float * float) (y: float * float) =
    let a, b = x
    let c, d = y
    (a - c, b - d)

let (.*) (x: float * float) (y: float * float) = 
    let a, b = x
    let c, d = y
    (a * c - b * d, b * c + a * d)

let inverse (x: float * float) = 
    let a, b = x
    (a / (a * a + b * b), -b / (a * a + b * b))

let (./) (x: float * float) (y: float * float) =
    x .* (inverse y)
