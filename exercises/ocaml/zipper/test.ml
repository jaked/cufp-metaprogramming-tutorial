type ('a, 'b) t = Foo of bool | Bar of int * ('a, 'b) t * ('a, 'b) t | Baz of ('a, 'b) t * string

let rec plug : ('a, 'b) t -> ('a, 'b) t' -> ('a, 'b) t =
  fun t -> function
    | Top -> t
    | Bar0 (i, z0, t1) -> Bar (i, plug t z0, t1)
    | Bar1 (i, t0, z1) -> Bar (i, t0, plug t z1)
    | Baz0 (z, s) -> Baz (plug t z, s)
