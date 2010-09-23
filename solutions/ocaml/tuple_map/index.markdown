# Tuple map

This exercise implements a "function" to map over a tuple:

    let (a, b, c) = Tuple.map g (d, e, f)

Such a function can't be implemented in plain OCaml of course; we use
Camlp4 to rewrite this to

    let (a, b, c) = (g d, g e, g f)

If the tuple is not a syntactic tuple, we have to specify its
arity. (Camlp4 works with syntax trees, so has no idea of the type of
an expression.) In that case let's use the syntax

    let x = Tuple.map 3 g y

to indicate that y is a 3-element tuple.

## Filters

This expansion is implemented using a Camlp4 filter. To implement a
filter we just provide a function of type `Ast.str_item ->
Ast.str_item`; we match the AST fragment for the syntax above and
replace it with the rewritten code.

To walk the tree we extend the `Ast.map` object, which has a method
for each syntactic class (`expr`, `patt`, etc.) that just maps over
the subtrees and returns the result unchanged.

## Other stuff

Unfortunately, Camlp4 filters don't work in the toplevel. So once you
have built and installed the code, you can try it by running e.g.

    camlp4of -I `ocamlfind query tuple_map` tuple_map.cmo \
      -str "Tuple.map f (1, 2, 3)"

or use the `syntax_camlp4o` and `pkg_tuple_map` tags in your
ocamlbuild `_tags` file.

## Tasks

 1. We have provided the boilerplate for registering a filter and
    walking the AST, but the actual rewrite is not implemented. You
    need to match `Tuple.map` expressions and replace them with the
    appropriate substitution. Do just the case where the
    tuple is a syntactic tuple.

    See the `Ast` functions `list_of_expr`
    and `exCom_of_list` (see the library signatures documentation) for
    converting between `Ast.ExCom` lists (that is, comma separated
    expressions) and ordinary lists. Also see the quotations page and
    the `tup` antiquotation in particular.

 2. Do the case where the arity of the tuple is given as an
    argument. Here the `int` antiquotation is useful (check the
    expansion if you're confused by a type error).

 3. Lots of other functions from the `List` module might be useful
    over tuples instead. Pick a few and implement them.

 4. Some but not all of the functions in `List` still make sense if
    the elements of the tuple or tuples involved have different
    types. Implement one of these, if you have not already, and make
    sure that it actually works on such tuples.
