{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Data.Tuple.TH
  (map
  ,map'
  ,sequence
  ,sequence_
  ,mapM
  ,mapM'
  ,mapM_
  ,mapM_'
  ,length
  ,(++)
  ,(!)
  ,reverse
  ,head
  ,tail
  ,init
  ,take
  ,drop
  ,splitAt
  ,replicate
  ,zip
  ,unTupleN
  ,unTupleT
  ,unTupleI) where

import Prelude ((&&),otherwise,Bool(..),error,Ord(..),Eq(..),Num(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qNewName,lift)

-- n ! i = [e| λ(x₁, …, xn) → xᵢ |]
(!) :: Int -> Int -> ExpQ
(!) n i = -- TODO

-- map' n f = [e| λ(x₁, …, xn) → ($(varE f) x₁, …, $(varE f) xn) |]
map' :: Int -> Name -> ExpQ
map' n f = --TODO

-- map n = [e| λf (x₁, …, xn) → (f x₁, …, f xn) |]
map :: Int -> ExpQ
map = -- TODO

-- sequence_ n = [e| λ(mx₁, …, mxn) → do { mx₁; …; mxn } |]
sequence_ :: Int -> ExpQ
sequence_ n = -- TODO

-- sequence n = [e| λ(mx₁, …, mxn) →
--   do { x₁ <- mx₁; …; xn <- mxn; return (x₁, …, xn) } |]
sequence :: Int -> ExpQ
sequence n = -- TODO

-- mapM' n f = [e| $(sequence n) · $(map' n f) |]
mapM' :: Int -> Name -> ExpQ
mapM' n f = -- TODO

-- mapM n = [e| λf (x₁, …, xn) →
--   do { y₁ <- f x₁; …; yn <- f xn; return (y₁, …, yn) } |]
mapM :: Int -> ExpQ
mapM = -- TODO

-- mapM_' n f = [e| λ(x₁, …, xn) → do { $(varE f) x₁; …; $(varE f) xn } |]
mapM_' :: Int -> Name -> ExpQ
mapM_' n f = -- TODO

-- mapM_ n = [e| λf (x₁, …, xn) → do { f x₁; …; f xn } |]
mapM_ :: Int -> ExpQ
mapM_ = -- TODO

-- (++) m n = [e| λ(x₁, …, xm)(y₁, …, yn) → (x₁, …, xm, y₁, …, yn) |]
(++) :: Int -> Int -> ExpQ
(++) m n = -- TODO

-- reverse n = [e| λ(x₁, …, xn) → (xn, …, x₁) |]
reverse :: Int -> ExpQ
reverse = -- TODO

-- tail n = [e| λ(x₁, …, xn) → (x₂, …, xn) |]
tail :: Int -> ExpQ
tail = -- TODO

-- init n = [e| λ(x₁, …, xn) → (x₁, …, xn-₁) |]
init :: Int -> ExpQ
init = -- TODO

-- take m n = [e| λ(x₁, …, xn) → (x₁, …, xm) |]
take :: Int -> Int -> ExpQ
take n = -- TODO

-- drop m n = [e| λ(x₁, …, xn) → (xm+₁, …, xn) |]
drop :: Int -> Int -> ExpQ
drop n = -- TODO

-- head n = [e| λ(x₁, …, xn) → x₁ |]
head :: Int -> ExpQ
head n = -- TODO

-- head n = [e| λ(x₁, …, xn) → x₁ |]
last :: Int -> ExpQ
last n = -- TODO

-- replicate n = [e| λx → (x, …, x) |]
replicate :: Int -> ExpQ
replicate n = -- TODO

-- splitAt i n = [e| λ(x₁, …, xn) → ((x₁, …, xᵢ), (xᵢ+₁, …, xn)) |]
splitAt :: Int -> Int -> ExpQ
splitAt i n = -- TODO

-- zip 2 n = [e| λ(x₁, …, xn) (y₁, …, yn) → ((x₁,y₁), …, (xn,yn)) |]
-- zip 3 n = [e| λ(x₁,…,xn) (y₁,…,yn) (z₁,…,zn) → ((x₁,y₁,z₁), …, (xn,yn,zn)) |]
zip :: Int -> Int -> ExpQ
zip k0 n = -- TODO

-- let p = (x₁, …, xn) in length 'p == n
length :: Name -> ExpQ
length n = do info <- reify n
              -- TODO
  where err = error "Data.Tuple.TH.length: A variable of type tuple was expected"
