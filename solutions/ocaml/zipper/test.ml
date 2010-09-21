type ('a, 'b) t = Foo of bool | Bar of int * ('a, 'b) t * ('a, 'b) t | Baz of ('a, 'b) t * string
