module Zipper where

import Language.Haskell.TH
import Data.List

-- | Generate a the zippper data-type of a given data-type.
-- More precisely it takes first the name of the input
-- type, then the name of the zipper type, and finally the
-- name of the top constructor for the generated zipper.
mkZipper :: Name -> String -> String -> Q [Dec]
mkZipper tcon tconZs topZdcon =
  do info <- reify tcon
     case info of
      TyConI (DataD cxt _ tvars ds _) ->
        return [DataD cxt tconZ tvars dsZ []]
        where
          tconZ = mkName tconZs
          tvars' = map (VarT . tyVarBndrName) tvars
          tZ = ConT tconZ `appsT` tvars'
          t  = ConT tcon  `appsT` tvars'
          dsZ = NormalC (mkName topZdcon) []
              : zipperDataConstructors t tZ ds

zipperDataConstructors :: Type -> Type -> [Con] -> [Con]
zipperDataConstructors t tZ = concatMap (go . unCon)
  where
  go (dcon, []) = []
  go (dcon, ts) =
       [ NormalC dconZi tZs
       | (i, (before,after)) <-
            zip [1..]
                [ (before,after)
                | (before,(_,t'),after) <- decomp ts
                , t == t'
                ]
       , let dconZi = mkName $ nameBase dcon ++ show i
             tZs = before ++ (NotStrict, tZ) : after
       ]

  unCon (NormalC dcon ts) = (dcon, ts)
  unCon _ = error "Unsupported type"

appsT :: Type -> [Type] -> Type
appsT = foldl AppT

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n _) = n

decomp :: [a] -> [([a],a,[a])]
decomp xs = zip3 (init . inits $ xs) xs (tail . tails $ xs)
