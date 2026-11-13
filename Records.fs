type TimeOfDay = { hours: int; minutes: int; f: string }

let minutes (t: TimeOfDay): int = 
    match t with
    | t when t.f = "AM" -> t.hours * 60 + t.minutes
    | t when t.f = "PM" -> (t.hours + 12) * 60 + t.minutes
    | _ -> failwith "Wrong value"

let (.>.) (x: TimeOfDay)  (y: TimeOfDay) = 
    (minutes x) > (minutes y)
    