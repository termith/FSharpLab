let curry f =
    let g (x: int) =
        let h (y: int) : int = f (x, y)
        h
    g

let uncurry g  = 
    let f (x: int, y: int) : int = 
        let h = 
            g x
        h y
    f
