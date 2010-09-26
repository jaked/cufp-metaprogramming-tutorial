module Zipper where

import Language.Haskell.TH

mkZipper :: Name -> String -> Q[Dec]
mkZipper tcon tconZs =
  do info <- reify tcon
     case info of
      TyConI (DataD cxt _ tvars ds _) ->
        return [{- TODO -}]

  unCon (NormalC dcon ts) = (dcon, ts)
  unCon _ = error "Unsupported type"

-- utility functions

appsT :: Type -> [Type] -> Type
appsT = foldl AppT

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n _) = n
