{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}
{-# OPTIONS_GHC -F -pgmF frquotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- The writer monad is used to get the "do" notation
-- for writing environments like slides or itemizes.
import Control.Monad.Writer

--import Control.Applicative

-- In order to get overloaded string literals
import Data.String

-- Getting ⊕ (ø is defined below)
import Data.Monoid.Unicode ((⊕))

-- Most of these are imported qualified, then
-- most used combinators are locally defined
-- (near the bottom of the file)
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Internal as BI
import qualified Language.LaTeX.Builder.Beamer as BM
import qualified Language.LaTeX.Builder.Math as M
import qualified Language.LaTeX.Length as L
import Language.LaTeX.Slicer (slice)
import Language.LaTeX.Builder.QQ

doc = B.document dc preamb body where
  dc = BM.documentclass [BM.t,BM.red,BM.compress]

preamb = ø
       ⊕ B.title «Metaprogramming Tutorial:{B.newline}
                  OCaml and Template Haskell»
       ⊕ B.author «Jake Donham and Nicolas Pouillard»
       ⊕ B.institute (B.texttt "jake@donham.org nicolas.pouillard@gmail.com")
       ⊕ BI.rawPreamble "\\date{CUFP 2010, Baltimore}"
       ⊕ BM.beamertemplatenavigationsymbolsempty
       ⊕ BM.useoutertheme [] "default"
       ⊕ BM.useinnertheme [("shadow","true")] "rounded"
       ⊕ BM.usecolortheme [] "orchid"
       ⊕ footline
       ⊕ margins

body = slice . execWriter $ do
  put B.maketitle

  slide «What is static metaprogramming?» $ do
    itemize $ do
      item «compile-time code generation»
      item «transformation of syntax trees»

  slide «What's it good for?» $ do
    itemize $ do
      item «for convenience (e.g. generate boilerplate or type-derived functions)»
      item «for speed (e.g. generate first-order functions from higher-order templates)»
      item «for EDSLs (e.g. embedded regexps or SQL)»
      item «for language extensions» -- (e.g. ???)

  slide «Static metaprogramming in OCaml and Haskell» $ do
    description $ do
      itemD «Camlp4:»
        «preprocessing front-end to the OCaml compiler,
         AST transformations written as Camlp4 plugins»
      itemD «Template Haskell:»
        «compiler extensions to GHC,
         AST transformations embedded in Haskell code»
  {-
  # A small example: first-order map
  
   * Eliminate use of higher-order function argument by generating
  static expansion
   * so `List.map f list` is transformed to
    `let rec map = function [] -> [] | a::l -> let r = f a in r :: map l`
   * benchmark showing this is faster [hopefully]
  
  # first-order map in OCaml
  
  # first-order map in Haskell
  -}

  slide «A small example: map over a tuple» $ do
    itemize $ do
      item «Avoid boilerplate of mapping over elements of a tuple»
      let tup_map_N = ml"Tuple.map{N} f (a, b, c, ...)" -- lifted to avoid nested braces
      item «so {tup_map_N} is transformed to {ml"(f a, f b, f c, ...)"}»

  slide «tuple map in OCaml» $ do
    p «...»

  slide «tuple map in Haskell» $ do
    p $ hs"import qualified Data.Tuple.TH as T"
    p «{hs"$(T.map 4) f (a,b,c,d)"} is transformed to
       {hs"(f a, f b, f c, f d)"}»

  slideC «Camlp4: mechanics»

  slide «How to run Camlp4:» $ do
    itemize $ do
      item . sh $ "camlp4of [module.cmo]* [file.ml]"
      item «show loaded modules: {sh"-loaded-modules"}»
      item «print original syntax: {sh"-printer o"}»
      item «show AST for debugging: {sh"-filter Camlp4AstLifter"}»
      item «take input from command line: {sh"-str [input]"}»
      item «Ex. {sh"camlp4of -printer o -filter Camlp4AstLifter \\\n    -str 'type t = Foo'"}»

{-
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
-}

  slide «ASTs in Template Haskell» $ do
    p «...»

  slide «Haskell quotations in Template Haskell» $ do
    p «...»

  slide «Practice: Generic functions on Tuples» $ do
    p «...»

  slide «A bigger example: [??]» $ do
    p «...»

  slide «Practice: [something bigger]» $ do
    p «...»

  slide «Camlp4-specific features, example» $ do
    itemize $ do
      item «extending OCaml syntax»

  slide «Template Haskell specific features, example» $ do
    itemize $ do
      item «reification»
      item «safer name handling with Q monad»
      item «type-checked quotations»

  slide «Practice: [something using the specific features]» $ do
    p «...»

todo :: a -> a
todo = id
{-# DEPRECATED todo "You have something to do here" #-}

docName = "cufp-metaprogramming-tutorial-slides"

ø :: Monoid m => m
ø = mempty

main = quickView myViewOpts{basedir="out",showoutput=False,pdfviewer="echo"} docName doc

usepackages = mconcat . map (BI.usepackage [] . BI.pkgName)

footline :: PreambleItem
footline =
  BI.rawPreamble $
  [$istr|\defbeamertemplate*{footline}{}
        |{
        |  \leavevmode%
        |  \hbox{%
        |    \begin{beamercolorbox}[wd=\paperwidth,ht=2.25ex,dp=1.125ex,right]{}%
        |      \insertframenumber{}\hspace*{2ex}
        |    \end{beamercolorbox}}%
        |  \vskip0pt%
        |}
        |]

p = put . B.para

vcenter x = B.vfill ⊕ x ⊕ B.vfill

put :: ParItem -> ParItemW
put = tell

slide title = put . BM.slide title . mapNonEmpty vcenter . execWriter

margins = BM.setbeamersize (BM.TextMarginLeft (L.cm 0.3))
        ⊕ BM.setbeamersize (BM.TextMarginRight (L.cm 0.3))

slideCs title subtitle =
    slide ø . put . B.center . (⊕subt) . B.para $ title
  where subt = mapNonEmpty ((B.vfill ⊕) . B.para) subtitle

slideC x = slideCs x ø

box = BM.block

verb = B.texttt . B.protector (myXchar (M.mchar B.ttchar))

myXchar xchar x
  | x `elem` "·ƛ_" = B.makebox (L.ex 1.22) Centered (xchar x)
  | x `elem` "="   = B.ttchar x
  | x `elem` "{}"  = M.mchar B.hchar x
myXchar xchar x    = xchar x

code = verb . dropWhile (=='\n')

hs = code
ml = code
sh = code

paraC = put . B.center . B.para

alertC = paraC . BM.alert

example :: LatexItem -> ParItemW
example = put . box ø . B.para

exampleML :: String -> ParItemW
exampleML = example . ml

exampleHS :: String -> ParItemW
exampleHS = example . hs

itemize block = B.itemize !$? block
description block = B.description !$? block
item = tell . return . B.item . B.para
itemD x = tell . return . B.item' x . B.para

