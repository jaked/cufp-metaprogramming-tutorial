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
import Language.LaTeX.Slicer (slice,(^$))
import Language.LaTeX.Builder.QQ

doc = B.document dc preamb body where
  dc = BM.beamer Nothing [BM.t,BM.red,BM.compress] []

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

  slide «Static metaprogramming» $ do
    p «What is it?»
    itemize $ do
      item «compile-time code generation»
      item «transformation of syntax trees»
    p «What's it good for?»
    itemize $ do
      item «for convenience (generate boilerplate or type-derived functions)»
      item «for speed (generate first-order functions from higher-order templates)»
      item «for EDSLs (embedded regexps or SQL)»
      item «for language extensions» -- (e.g. ???)

  slide «Static metaprogramming in OCaml and Haskell» $ do
    description $ do
      itemD «Camlp4:»
        «preprocessing front-end to the OCaml compiler,
         AST transformations written as Camlp4 plugins»
      itemD «Template Haskell:»
        «compiler extensions to GHC,
         AST transformations embedded in Haskell code»

  slide «Small example: map over a tuple» $ do
    itemize $ do
      item «Avoid boilerplate of mapping over elements of a tuple»
      item «in OCaml, {B.newline}
            {ml"  Tuple.map f (a, b, c, d)"} {B.newline}
            is transformed to {B.newline}
            {ml"  (f a, f b, f c, f d)"}»
      item «in Haskell, {B.newline}
            {hs"  import qualified Data.Tuple.TH as T"} {B.newline}
            {hs"  $(T.map 4) f (a,b,c,d)"} {B.newline}
            is transformed to {B.newline}
            {hs"  (f a, f b, f c, f d)"}»

  slideCB «Camlp4»

  slide «ASTs in Camlp4:» $ do
    itemize $ do
      itemV . exampleML . unlines $
        ["type expr = ... and patt = ... and"
        ,"ctyp = ... and str_item = ... and ..."
        ]
      item «e.g. {ml"ExInt"} for an int expr, {ml"TySum"} for a sum type»
      item «see {sh"Camlp4Ast.partial.ml"} for full def»
      item «somewhat loose---easy to make invalid AST»
      item «converted to OCaml AST; see {sh"Camlp4Ast2OCamlAst.ml"} for errors»

  slide «OCaml quotations:» $ do
    itemize $ do
      item «a way to work with the AST using concrete syntax»
      item «you can always fall back to AST constructors!»
      item «e.g. {ml"<:expr< 1, 2 >>"} becomes {B.newline}
           {ml"ExTup (_, (ExCom (_, (ExInt (_, \"1\")), \n                     (ExInt (_, \"2\")))))"}»
      item «{ml"<:ctyp< int * int >>"} becomes {B.newline}
           {ml"TyTup (_, (TySta (_, (TyId (_, (IdLid (_, \"int\")))), \n                     (TyId (_, (IdLid (_, \"int\")))))))"}»
      item «antiquotations: {ml"<:expr< 1, $x$ >>"}, {ml"<:expr< 1, $`int:x$ >>"}»
      item «see doc page of quotations / antiquotations»

  slide «Working with the AST:» $ do
    p . ml $ "Ast.map"
    itemize $ do
      item «object that maps over AST»
      item «method for each syntactic class»
      item «override to operate on AST»
    p «locations, {ml"Ast.Loc.t"}»
    itemize $ do
      item «stores filename and position»
      item «must provide one to construct AST nodes»
      item «quotations use {ml"_loc"} by default»
      item . ml $ "Loc.ghost"
      item . ml $ "Ast.loc_of_expr e"
      item . ml $ "<:expr@_loc< >>"
      item . ml $ "<:expr@here< >>"

  slide «Revised syntax:» $ do
    itemize $ do
      item «alternative concrete syntax for OCaml»
      item «fixes some infelicities in OCaml syntax»
      item «makes antiquotation easier (gives more context, bugs with original syntax)»
      item «{ml"list t"} instead of {ml"t list"}»
      item «{ml"match [ patt -> expr | ... ]"} instead of {ml"match patt -> expr | ..."}»
      item «{ml"True"} instead of {ml"true"}»
      item «see doc page for full details»

  slide «Running Camlp4:» $ do
    itemize $ do
      item . sh $ "camlp4of [module.cmo]* [file.ml]"
      item «show loaded modules: {sh"-loaded-modules"}»
      item «print original syntax: {sh"-printer o"}»
      item «show AST for debugging: {sh"-filter Camlp4AstLifter"}»
      item «take input from command line: {sh"-str [input]"}»
      item «Ex. {sh"camlp4of -printer o -filter Camlp4AstLifter \\\n    -str 'type t = Foo'"}»

  slide «Debugging:» $ do
    itemize $ do
      item «don't know what AST to use?»
      {- itemize $ do -}
      item «run example through camlp4of to see what AST is parsed»
      item «quotations / antiquotations don't work?»
      {- itemize $ do -}
      item «read parsers {sh":("} to see why ({sh"Camlp4OCamlRevisedParser.ml"}, {sh"Camlp4OCamlParser.ml"})»
      item «fall back to AST constructors»
      item «errors converting Camlp4 to OCaml AST?»
      {- itemize $ do -}
      item «read converter to see why ({sh"Camlp4Ast2OCamlAst.ml"})»
      item «use {sh"-filter Camlp4AstLifter"} to see what you're generating»

  slideCB «Template Haskell»

  slide «ASTs in Template Haskell» $ do
    p «TH exposes data-types for expressions, patterns, declarations,
       types... (Exp, Pat, Dec, Type).»
    exampleHS . unlines $
      ["exE :: Exp"
      ,"exE = ListE [_42, VarE 'succ `AppE` _42]"
      ,"  where _42 = LitE (IntegerL 42)"
      ]

  slide «Smart constructors, new names, and the Q monad» $ do
    p «TH also exposes smart constructors for all constructors, to
       build programs in the Q monad.»
    exampleHS . unlines $
      ["apE :: ExpQ"
      ,"apE = do x <- qNewName \"x\""
      ,"         y <- qNewName \"y\""
      ,"         lamE [varP x, varP y] (varE x `appE` varE y)"
      ]

  slide «Generic quotations in Template Haskell» $ do
    p «TH has a general mechanism for quotations.»
    exampleHS "[$sql| SELECT * FROM `users` |]"
    exampleHS "[$regex| (a|b)*b*(a|b)* |]"
    exampleHS "[$xml| <person><name>Foo</name><age>42</age></person> |]"

  slide «Haskell quotations in TH» $ do
    p «TH has a general mechanism for quotations.»
    exampleHS "[e| \\f g x -> f (g x) |]"
    exampleHS "[t| Int -> (Bool, Char) |]"
    exampleHS "[d| data Foo = A | B | C |]"

  slide «... and antiquotions for those» $ do
    p «Using {hs"$(...)"} one can splice expressions, types... into one other.»
    exampleHS "[e| case $(a) of { [] -> $(b) ; x:xs -> $(c) x xs } |]"
    exampleHS "[t| Int -> ($(t), Char) |]"
    exampleHS "[d| data Foo = A | B $(t) | C |]"

  slideCB «Exercises»

  slide «Tuple map» $ do
    p «Implement the tuple map syntax from the example.»

  slide «Zipper types» $ do

    p «The "zipper" representation of a value of type {ml"t"} is a
       subtree of type {ml"t"}, and a context of type {ml"t'"}, where
       {ml"t'"} is derived systematically from {ml"t"}. (see Huet)»

    put B.bigskip

    p «A zipper type has:»
    itemize $ do
      item «a {ml"Top"} arm»
      item «for each arm containing {ml"t"}, an arm for each occurrence of {ml"t"}
            with that occurrence replaced with {ml"t'"}»

    put B.bigskip

    p «For example: {B.newline}
      {ml"  type t = Leaf | Tree of t * t"} {B.newline}
      has zipper type {B.newline}
      {ml"  type t' = Top | Tree0 of t' * t | Tree1 of t * t'"}»

    put B.bigskip

    p «Implement a generator for zipper types.»



  slide «Implementing quotations/antiquotations in Camlp4» $ do
    p «Quotations are implemented in several phases:»
    itemize $ do
      item «quotation is lexed to a {ml"QUOTATION"} token containing tag and body as strings»
      item «expander for tag is looked up according to parse context (e.g. {ml"expr"} vs. {ml"patt"})»
      item «expander parses string to quotation AST with {ml"FooAnt"} nodes for antiquotations, containing tag and body»
      item «expander lifts quotation AST to Camlp4 AST according to parse context»
      item «expander parses antiquotation nodes as OCaml and applies conversions according to tag»

  slide «JSON quotations in OCaml» $ do
    p «We can define quotations for JSON:»
    exampleHS . unlines $
      ["<:json< [ 1, 2, 3 ] >>"
      ,"<:json< { \"foo\" : true, \"bar\" : 17 } >>"
      ]

    put B.bigskip

    p «And antiquotations:»
    exampleHS . unlines $
      ["<:json< [ 1, $int:x$, 3 ] >>"
      ,"<:json< { \"foo\" : $bool:b$, \"bar\" : 17  } >>"
      ,"<:json< [ 1, $list:y$, 3 ] >>"
      ]

  slide «JSON quotations in Haskell» $ do
    p «We can define quotations for JSON:»
    exampleHS . unlines $
      ["[$json| [ 1, 2, 3 ] |]"
      ,"[$json| { \"foo\" : true, \"bar\" : 17 } |]"]

    put B.bigskip

    p «And antiquotations:»
    exampleHS . unlines $
      ["[$json| [ 1, $(js x), 3 ] |]"
      ,"[$json| { \"foo\" : $(js b), \"bar\" : 17  } |]"
      ,"[$json| [ 1, $(js y), 3 ] |]"
      ]

    put B.bigskip

    p «Implement JSON quotations and antiquotations.»

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

slideCB x = slideCs (B.decl B._Huge x) ø

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

paraCHuge = paraC . B.decl B._Huge

alertC = paraC . BM.alert

example :: LatexItem -> ParItemW
example = put . box ø . B.para

exampleML :: String -> ParItemW
exampleML = example . ml

exampleHS :: String -> ParItemW
exampleHS = example . hs . reverse . dropWhile (=='\n') . reverse

itemize block = B.itemize !$? block
description block = B.description !$? block
item = tell . return . B.item . B.para
itemV bk = tell . return . B.item $? bk
itemD x = tell . return . B.item' x . B.para

