# Generating zipper types, or "deriving the derivative"

Gerard Huet's well-known functional pearl The Zipper shows a way to
represent a tree that provides constant-time navigation and
modification operations. The idea is to represent a pointer into the
tree as a pair of the pointed-to subtree and the context in which it
appears (that is, its parent node).

This exercise defines a Camlp4 filter which generates the associated
zipper type (the type of the context) of a type.

The context of a subtree consists of the fields in the parent node's
constructor (other than the pointed-to subtree) and the parent's
parent (also represented as a context, until we get to the top of the
tree). We also need to know which constructor (of the original type)
it is, and, if the original type occurs more than once in the
constructor, in which occurrence the subtree appears.

Here is a concrete example:

    type t = Leaf | Tree of t * t
    type t' = Top | Tree0 of t' * t | Tree1 of t * t'

The zipper type `t'` always has a `Top` constructor, and it has a
`TreeN` for each occurrence of `t` in `Tree` (and similarly for other
constructors), to indicate the position of the subtree. Constructors
with no occurrence of the original type have no arm in the zipper
type, since the original type can't appear with the constructor as its
context.

A pointer consists of a `t * t'`. So if we have a tree

    Tree (Tree (Leaf, Leaf), Leaf)

a pointer to the left child of the root is

    (Tree (Leaf, Leaf), Tree0 (Top, Leaf))

We can then define functions to move the pointer in the tree and make
modifications to it. See Huet's paper for details.

## Other stuff

Once you have built and installed the code, you can try it by running
e.g.

    camlp4of -I `ocamlfind query zipper` zipper.cmo \
      -str "type t = Leaf | Tree of t * t"

or use the `syntax_camlp4o` and `pkg_zipper` tags in your ocamlbuild
`_tags` file.

## Tasks

 1. We have provided the filter boilerplate; fill in the code that
    generates the zipper type of a type. At first don't worry about
    type parameters or occurrences of the type which are not directly
    under a constructor (e.g. in `Foo of bool * t` the type `t` occurs
    directly under a constructor, but in `Bar of int * (int * t)` it
    does not). Handle ordinary variants only.

 2. Handle type parameters. Find out how they are represented in the
    AST, and when generating the zipper type check for occurrences of
    the type applied to its parameters. Don't worry about non-regular
    type definitions; assume that the type is always applied to the
    same parameters with which it was declared. When comparing
    fragments of AST, you'll want to ignore the locations somehow.

 3. Generate navigation and modification functions for the zipper,
    following Huet's paper.

 4. Handle occurrences which are not directly under a constructor, but
    appear in a tuple as above, to any level of nesting.

 5. What other types could we reasonably handle? Mutually recursive
    types? Polymorphic variants? Records? Functions? Objects? Pick
    some and handle them.
