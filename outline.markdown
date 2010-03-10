# What is static metaprogramming?

 * compile-time code generation
 * transformation of syntax trees

# What's it good for?

 * for convenience (e.g. generate boilerplate or type-derived functions)
 * for speed (e.g. generate first-order functions from higher-order templates)
 * for DSLs (e.g. embedded regexps or SQL)
 * for language extensions (e.g. )

# Static metaprogramming in OCaml and Haskell

 * Camlp4:
  preprocessing front-end to ocamlc,
  AST transformations written as Camlp4 plugins

 * Template Haskell:
  compiler extension to ghc,
  AST transformations embedded in Haskell code

# A small example: first-order map

 * Eliminate use of higher-order function argument by generating
static expansion
 * so `List.map f list` is transformed to
  `let rec map = function [] -> [] | a::l -> let r = f a in r :: map l`
 * benchmark showing this is faster [hopefully]

# first-order map in OCaml

# first-order map in Haskell

# [alternative] A small example: map over a tuple

 * Avoid boilerplate of mapping over elements of a tuple
 * so `Tuple.map{N} f (a, b, c, ...)` is transformed to
  `(f a, f b, f c, ...)`

# tuple map in OCaml

# tuple map in Haskell [is this covered by deriving Functor?]

# ASTs in Camlp4

# ASTs in Template Haskell

# OCaml quotations in Camlp4

# Haskell quotations in Template Haskell

# Practice: [something small]

# A bigger example: [??]

# Practice: [something bigger]

# Camlp4-specific features, example

 * quotations
 * extending OCaml

# Template Haskell specific features, example

 * reification
 * [what else?]

# Practice: [something using the specific features]
