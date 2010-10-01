# Template Haskell and Camlp4 Tutorial

Hi and welcome to the tutorial! Here are some local resources:

## OCaml

 * [AST type definition](doc/ocaml/Camlp4Ast.partial.ml.html)
 * [AST quotations](doc/ocaml/ast.html)
 * [revised syntax](doc/ocaml/revised.html)
 * [library signatures](doc/ocaml/Sig.ml.html)
 * [Camlp4Ast2OCamlAst.ml](doc/ocaml/Camlp4Ast2OCamlAst.ml.html)
 * [Camlp4OCamlRevisedParser.ml](doc/ocaml/Camlp4OCamlRevisedParser.ml.html)
 * [Camlp4OCamlParser.ml](doc/ocaml/Camlp4OCamlParser.ml.html)

Exercises:

 * [Tuple map](exercises/ocaml/tuple_map/index.html)
 * [Zipper types](exercises/ocaml/zipper/index.html)
 * [JSON quotations](exercises/ocaml/json_quot/index.html)

## Haskell

 * [TH Manual](http://www.haskell.org/ghc/docs/latest/html/users_guide/template-haskell.html)
 * [QQ Manual](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/template-haskell.html#th-quasiquotation)
 * [TH API](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/template-haskell)
 * [TH Wiki](http://www.haskell.org/haskellwiki/Template_Haskell)
 * [TH Paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/meta-haskell.pdf)

Loading TH in GHCi:

`GHCI> :set -XTemplateHaskell`

Hint to show code:

`printQ :: Ppr a => Q a -> IO ()`
`printQ x = putStrLn . pprint =<< runQ x`

And then:

`printQ [e| [42,43] |]`

Exercises:

 * [Tuple map](exercises/haskell/tuple/tuple.html)
 * [Zipper types](exercises/haskell/zipper/zipper.html)
 * [JSON quotations](exercises/haskell/json-quot/json-quot.html)

## Other

 * [The Zipper (Huet)](doc/huet-zipper.pdf)
