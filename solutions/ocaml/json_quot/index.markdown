# JSON quotations

This exercise implements Camlp4 quotations for JSON. You can write e.g.

    <:json< [ 1, 2, 3 ] >>

    <:json< { "foo" : [ true, false ], "bar" : [ false, true ] } >>

Antiquotations are also supported, e.g.:

    let x = 2 in
    << [ 1, $int: x$, 3 ] >>

    let lst = [ << 1 >>, << 2 >>, << 3 >> ] in
    << [ $list: lst$ ] >>

(The default quotation is set to `json` so you can just use `<< >>`.)

## JSON AST

The JSON AST is defined in `Jq_ast`. It is not exactly what you might
expect; in addition to the constructors for the JSON base types (null,
boolean, number, string) and for arrays and objects, there are
constructors `Jq_colon`, `Jq_comma`, and `Jq_nil`. The array and
object constructors just take a `Jq_ast.t` as argument rather than a
list. Finally there is a `Jq_Ant` constructor to represent
antiquotations; an antiquotation consists of a string containing a tag
and the actual antiquotation, separated by a colon.

The idea here is that lists are represented internally to `Jq_ast.t`,
with `Jq_comma` roughly playing the role of `::` and `Jq_nil` of
`[]`. Any tree built of `Jq_comma` and `Jq_nil` nodes corresponds to a
list where the elements are the non-`Jq_comma`, non-`Jq_nil` nodes
taken in-order. (In particular a tree that contains no `Jq_comma`
nodes is a one-element list.) This is to make possible antiquotations
like

    <:json< [ 1, $list: lst$, 3 ] >>

where a list is interpolated within an array. The quotation parses to

    Jq_array
      (Jq_comma (Jq_number 1.,
        (Jq_comma (Jq_Ant (_loc, "list:lst"),
           (Jq_number 3.)))))

and when the antiquotation is expanded, the `Jq_Ant` node is replaced
with a `Jq_comma` list. For objects, the elements of the list must be
`Jq_colon` nodes, so e.g.

    <:json< { "foo" : 5 } >>

parses to

    Jq_object (Jq_colon (Jq_string "foo", Jq_number 5.))

A downside to this setup is that it's easy to form values which don't
correspond to a JSON value. The functions `t_of_list` and `list_of_t`
in `Jq_ast` convert between ordinary lists and `Jq_comma` lists.

## Quotations

The quotation implementation is in `Jq_quotations`. All it really does
is parse quotations to `Jq_ast.t` values, lift them to `Ast.expr` or
`Ast.patt` values, and expand the antiquotations inside. But there are
a lot of niggly details.

The idea with lifting is that we must ultimately produce an `Ast.expr`
or `Ast.patt` from a quotation; when we write e.g. `<:json< 1 >>` we
want that to turn into the OCaml AST representing `Jq_number 1.`,
which of course is `<:expr< Jq_number 1. >>`. So we have two functions
in `Jq_ast` (`MetaExpr.meta_t` and `MetaPatt.meta_t`) which lift the
JSON AST to the equivalent OCaml AST.

When we reach a `Jq_Ant` constructor, we lift it to an `Ast.ExAnt` or
`Ast.PaAnt`, rather than an `<:expr< Jq_Ant >>`. There is no point in
actually lifting `Jq_Ant` since we're going to expand it anyway, and
it's easier to find the antiquotations as specific constructors.

To expand an antiquotation node we have to take the string it
contains, parse it as OCaml, then (possibly) do some additional
conversion of the OCaml AST based on the tag. For instance, for the
quotation

    <:json< $int:x$ >>

we parse it to `Jq_Ant "int:x"`, lift it to `Ast.ExAnt "int:x"`, parse
`"x"` as OCaml, then wrap it in a conversion to get

    <:expr< Jq_number (float_of_int x) >>

## Other stuff

The parser is defined in `Jq_parser`. It uses Camlp4's parsing
machinery, which is convenient (in particular because Camlp4's default
lexer supports antiquotations) but not necessary; the interface takes
a `string` (along with a Camlp4 location) to a `Jq_ast.t`, so we could
use ocamlyacc or whatever instead.

A pretty-printer is defined in `Jq_printer`, and hooked up to the
toplevel so `Jq_ast.t` values are pretty-printed as JSON.

Once you have built and installed the code, you can try it by starting
a toplevel then running

    #use "topfind"
    #camlp4o
    #require "json_quot"
    <:json< [1, 2, 3 ] >>

## Tasks

  1. In `Jq_ast`, the `meta_t` functions are stubbed out; implement
     them. For the base types you'll need to convert OCaml base types
     to lifted versions; see the AST and quotation documentation for
     convenient antiquotation tags. See the antiquotations section
     above for the treatment of `Jq_Ant`.

  2. In `Jq_quotations`, we define an object extending `Ast.map` to
     expand antiquotations. There is already code to parse the
     antiquotations as OCaml, but the conversions have not been
     implemented; implement them. For `list` and `alist`, see the AST
     section above for how JSON arrays and objects should be
     represented.

  3. Implement a quotation expander for a small language of your
     choice. You will need to design an AST that can accomodate
     antiquotations, parse strings to this AST (using whatever parsing
     technology you like), lift your AST to the OCaml AST, and expand
     any special antiquotations you care to define. You should be able
     to copy the niggly details which we haven't explained from the
     JSON code.
