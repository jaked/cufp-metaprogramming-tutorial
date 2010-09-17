+----------------------------------------------+
|Deprecated, the slides now take place in      |
|slides/cufp-metaprogramming-tutorial-slides.hs|
+----------------------------------------------+

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
  preprocessing front-end to the OCaml compiler,
  AST transformations written as Camlp4 plugins

 * Template Haskell:
  compiler extensions to ghc,
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

# tuple map in Haskell

# Camlp4: mechanics

How to run Camlp4:

 * `camlp4of [module.cmo] [file.ml]`
 * show loaded modules: `-loaded-modules`
 * print original syntax: `-printer o`
 * show AST for debugging: `-filter Camlp4AstLifter`
 * take input from command line: `-str [input]`
 * Ex. `camlp4of -printer o -filter Camlp4AstLifter -str "type t = Foo"`

# ASTs in Camlp4

The Camlp4 AST:

 * `type expr = ... and patt = ... and ctyp = ... and str_item = ... and ...`
 * e.g. `ExInt` for an int expr, `TySum` for a sum type
 * see `Camlp4Ast.partial.ml` for full def
 * somewhat loose---easy to make invalid AST
 * converted to OCaml AST; see `Camlp4Ast2OCamlAst.ml` for errors
 * locations added by parser; see `module type Loc` in `Sig.ml` for API

# OCaml quotations

Work with the OCaml AST using OCaml concrete syntax:

 * you can always fall back to AST constructors!
 * `<:expr< 1, 2 >>` becomes
   `ExTup (_, (ExCom (_, (ExInt (_, "1")), (ExInt (_, "2")))))`
 * `<:ctyp< int * int >>` becomes
   `TyTup (_, (TySta (_, (TyId (_, (IdLid (_, "int")))), (TyId (_, (IdLid (_, "int")))))))`
 * antiquotations: `<:expr< 1, $x$ >>`, `<:expr< 1, $`int:x$ >>`
 * see wiki page of quotations / antiquotations

# Revised syntax

Alternative syntax for OCaml:

 * fixes some infelicities in OCaml syntax
 * makes antiquotation easier (gives more context)
 * avoid bugs in orginal syntax antiquotations
 * `list t` instead of `t list`
 * `match [ patt -> expr | ... ]` instead of `match patt -> expr | ...`
 * `True` instead of `true`
 * see doc page for full details

# Examples

 * ...

# ASTs in Template Haskell

# Haskell quotations in Template Haskell

# Practice: [something small]

# A bigger example: [??]

# Practice: [something bigger]

# Camlp4-specific features, example

 * extending OCaml syntax

# Template Haskell specific features, example

 * reification
 * safer name handling with Q module
 * type-checked quotations

# Practice: [something using the specific features]
