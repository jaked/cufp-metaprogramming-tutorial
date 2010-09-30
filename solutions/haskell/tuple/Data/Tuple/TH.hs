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
import Data.Function
import Data.Int
import Data.Maybe
import qualified Data.List as L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qNewName,lift)
import Control.Monad (replicateM,return)

tupPV :: [Name] -> PatQ
tupPV = tupP . L.map varP

tupEV :: [Name] -> ExpQ
tupEV = tupE . L.map varE

pairE :: ExpQ -> ExpQ -> ExpQ
pairE x y = tupE [x,y]

retS :: ExpQ -> StmtQ
retS x = noBindS [e| return $(x) |]

lamTup :: Int -> ([ExpQ] -> ExpQ) -> ExpQ
lamTup n body = do xs <- replicateM n (qNewName "x")
                   lam1E (tupPV xs) (body (L.map varE xs))

tupFromList :: (forall a. [a] -> [a]) -> Int -> ExpQ
tupFromList f n = lamTup n $ tupE . f

-- lamF body = [e| \f -> $(body 'f) |]
lamF :: (Name -> ExpQ) -> ExpQ
lamF body = do f <- qNewName "f"
               lam1E (varP f) (body f)

-- n ! i = [e| λ(x₁, …, xn) → xᵢ |]
(!) :: Int -> Int -> ExpQ
(!) n i
  | i >= 0 && i < n = lamTup n $ \xs -> xs L.!! i
  | otherwise       = error "Data.Tuple.(!): index out of bounds"

-- map' n f = [e| λ(x₁, …, xn) → ($(varE f) x₁, …, $(varE f) xn) |]
map' :: Int -> Name -> ExpQ
map' n f = lamTup n $ tupE . L.map (appE (varE f))
{-
map' n f
  = do xs <- replicateM n (qNewName "x")
       let f_ x = varE f `appE` varE x
       lam1E (tupPV xs) (tupE (L.map f_ xs))
-}

-- map n = [e| λf (x₁, …, xn) → (f x₁, …, f xn) |]
map :: Int -> ExpQ
map = lamF . map'

-- sequence_ n = [e| λ(mx₁, …, mxn) → do { mx₁; …; mxn } |]
sequence_ :: Int -> ExpQ
sequence_ n = lamTup n $ doE . L.map noBindS

-- sequence n = [e| λ(mx₁, …, mxn) →
--   do { x₁ <- mx₁; …; xn <- mxn; return (x₁, …, xn) } |]
sequence :: Int -> ExpQ
sequence n
  = lamTup n $ \mxs-> do
      xs <- replicateM n (qNewName "x")
      let stmt mx x = bindS (varP x) mx
      doE $ L.zipWith stmt mxs xs L.++ [retS (tupEV xs)]

-- mapM' n f = [e| $(sequence n) · $(map' n f) |]
mapM' :: Int -> Name -> ExpQ
mapM' n f = [e| $(sequence n) . $(map' n f) |]

-- mapM n = [e| λf (x₁, …, xn) →
--   do { y₁ <- f x₁; …; yn <- f xn; return (y₁, …, yn) } |]
mapM :: Int -> ExpQ
mapM = lamF . mapM'

-- mapM_' n f = [e| λ(x₁, …, xn) → do { $(varE f) x₁; …; $(varE f) xn } |]
mapM_' :: Int -> Name -> ExpQ
mapM_' n f = [e| $(sequence_ n) . $(map' n f) |]

-- mapM_ n = [e| λf (x₁, …, xn) → do { f x₁; …; f xn } |]
mapM_ :: Int -> ExpQ
mapM_ = lamF . mapM_'

-- (++) m n = [e| λ(x₁, …, xm)(y₁, …, yn) → (x₁, …, xm, y₁, …, yn) |]
(++) :: Int -> Int -> ExpQ
(++) m n = lamTup m $ \xs->
             lamTup n $ \ys->
               tupE (xs L.++ ys)

-- reverse n = [e| λ(x₁, …, xn) → (xn, …, x₁) |]
reverse :: Int -> ExpQ
reverse = tupFromList L.reverse

-- tail n = [e| λ(x₁, …, xn) → (x₂, …, xn) |]
tail :: Int -> ExpQ
tail = tupFromList L.tail

-- init n = [e| λ(x₁, …, xn) → (x₁, …, xn-₁) |]
init :: Int -> ExpQ
init = tupFromList L.init

-- take m n = [e| λ(x₁, …, xn) → (x₁, …, xm) |]
take :: Int -> Int -> ExpQ
take n = tupFromList (L.take n)

-- drop m n = [e| λ(x₁, …, xn) → (xm+₁, …, xn) |]
drop :: Int -> Int -> ExpQ
drop n = tupFromList (L.drop n)

-- head n = [e| λ(x₁, …, xn) → x₁ |]
head :: Int -> ExpQ
head n
  | n > 0 = lamTup n L.head
  | otherwise  = error "Data.Tuple.TH.head: expect a strictly positive tuple length"

-- head n = [e| λ(x₁, …, xn) → x₁ |]
last :: Int -> ExpQ
last n
  | n > 0 = lamTup n L.last
  | otherwise  = error "Data.Tuple.TH.last: expect a strictly positive tuple length"

-- replicate n = [e| λx → (x, …, x) |]
replicate :: Int -> ExpQ
replicate n = lamF $ tupE . L.replicate n . varE

-- splitAt i n = [e| λ(x₁, …, xn) → ((x₁, …, xᵢ), (xᵢ+₁, …, xn)) |]
splitAt :: Int -> Int -> ExpQ
splitAt i n = lamTup n $ \xs-> let (ys,zs) = L.splitAt i xs in
                               tupE [tupE ys, tupE zs]

-- zip n = [e| λ(x₁, …, xn) (y₁, …, yn) → ((x₁,y₁), …, (xn,yn)) |]
{-
zip :: Int -> ExpQ
zip n = lamTup n $ \xs->
          lamTup n $ \ys->
            tupE $ zipWith pairE xs ys
-}

-- zip 2 n = [e| λ(x₁, …, xn) (y₁, …, yn) → ((x₁,y₁), …, (xn,yn)) |]
-- zip 3 n = [e| λ(x₁,…,xn) (y₁,…,yn) (z₁,…,zn) → ((x₁,y₁,z₁), …, (xn,yn,zn)) |]
zip :: Int -> Int -> ExpQ
zip k0 n = go [] k0 where
  go xss k
    | k <= 0     = tupE . L.map tupE . L.transpose . L.reverse $ xss
    | otherwise  = lamTup n $ \xs-> go (xs:xss) (k-1)

-- TODO
-- unzips
-- tails,inits,iterate,mapAccumL,mapAccumR
-- folds and derivatives, scans

-- should not be exported
data OneTuple a = OneTuple a

tupleN :: Int -> Name
tupleN 1 = ''OneTuple
tupleN k = mkName ("(" L.++ L.replicate (k - 1) ',' L.++ ")")

unTupleN :: Name -> Maybe Int
unTupleN n = L.findIndex (==n) ns
  where ns = L.map tupleN [0..]

unTupleT :: Type -> Maybe Int
unTupleT (TupleT i)  = Just i
unTupleT (ConT c)    = unTupleN c
unTupleT (AppT t _)  = unTupleT t
unTupleT _           = Nothing

unTupleI :: Info -> Maybe Int
unTupleI (VarI _ t _ _)  = unTupleT t
unTupleI _               = Nothing

-- let p = (x₁, …, xn) in length 'p == n
length :: Name -> ExpQ
length n = do info <- reify n
              lift $ fromMaybe err (unTupleI info)
  where err = error "Data.Tuple.TH.length: A variable of type tuple was expected"

-- other suggestions: curry, uncurry, toList, fromList
