# JSON quotations

This exercise implements TH quotations for JSON. You can write e.g.

    [$json| [ 1, 2, 3 ] |]

    [$json| { "foo" : [ true, false ], "bar" : [ false, true ] } |]

Antiquotations are also supported, e.g.:

    let x = [$json| 2 |] in
    [$json| [ 1, $(x), 3 ] |]

## JSON AST

The JSON AST is defined in `Data.Object` in the `data-object` package,
and the JSON scalars are in `Data.Object.Json` in the `data-object-json`.

To handle the antiquotation one will tweak the type of scalars (and the
dictionary key type) in order to leave some room to the antiquotations.
Actually it is more modular to wrap the actual JSON scalars with this
type:

    data MayAnti a = V a
                   | A String

Then the parsers will go to `Object (MayAnti JsonScalar)`, example:

    [:json|{ "foo" : 5 } |]

parses to

    Mapping [(V "foo", Scalar (V (JsonNumber 5)))]

## Tasks

  1. Combine some parser combinators to build a parser supporting
     antiquotations.

  2. Fill the `lift` instances for the JSON types.

  3. Right the JSON quotation expander for expressions using the parser,
     the lifting function. By testing you may discover some things to
     tweak.
