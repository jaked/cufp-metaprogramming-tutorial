{-# LANGUAGE TemplateHaskell #-}
module ZipperTest where

import Zipper

data T a b
  = Foo Bool
  | Bar Int (T a b) (T a b)
  | Baz (T a b) String

$(mkZipper ''T "Z" "ZTop")

plug :: T a b -> Z a b -> T a b
plug t ZTop            = t
plug t (Bar1 i z1 t2)  = Bar i (plug t z1) t2
plug t (Bar2 i t1 z2)  = Bar i t1 (plug t z2)
plug t (Baz1 z s)      = Baz (plug t z) s
