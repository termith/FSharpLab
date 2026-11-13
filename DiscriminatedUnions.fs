type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let minutes (t: TimeOfDay): int = 
    match t.f with
    | AM -> t.hours * 60 + t.minutes
    | PM -> (t.hours + 12) * 60 + t.minutes
    | _ -> failwith "Wrong value"

let (.>.) (x: TimeOfDay)  (y: TimeOfDay) = 
    (minutes x) > (minutes y)
