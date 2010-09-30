# Tuple map

This exercise implements a "function" to map over a tuple:

    import qualified Data.Tuple.TH as T

    let (a, b, c) = $(T.map 3) g (d, e, f)

Such a function can't be implemented in plain Haskell of course; we use
TH to rewrite this to:

    let (a, b, c) = (\f (x1,x2,x3) -> (f x1, f x2, f x3)) g (d, e, f)

Which gets optimized by GHC into (at least):

    let (a, b, c) = (g d, g e, g f)

## TH code generators

The `T.map` function has the following type:

    map :: Int -> ExpQ

And given the size of the tuple as an integer and returns the code
that does map a tuple of this size.

## TH Splices

Once we have generated code and we want to insert it into the original
program, and this what TH splices do. The `$(e)` constructs insert (splice)
the result of `e` of type `ExpQ` as if the contents where typed by hand.

## Tasks

1. There is plenty of functions that you can write on tuples. We propose
   a lot of functions inspired from the `Data.List` module. Start with
   the functions you find the simpler. The test file will helps you to
   check your code.

2. Make sure to write the `map` function.

3. As you write more functions try to factorize them since many of them
   can get really short definitions.
