let f x = x + 1

let (a, b, c) = Tuple.map f (1, 2, 3)

let _ = Printf.printf "(%d, %d, %d)\n" a b c
