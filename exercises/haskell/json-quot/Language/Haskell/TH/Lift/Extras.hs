{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.TH.Lift.Extras where

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Lift

liftAbstract :: (concrete -> ExpQ) -> Name -> (abstract -> concrete) -> abstract -> ExpQ
liftAbstract liftConcrete fromConcrete fromAbstract x
  = varE fromConcrete `appE` liftConcrete (fromAbstract x)

deriveLiftAbstract :: Name -> Name -> Name -> Q [Dec]
deriveLiftAbstract tyName fromConcrete fromAbstract 
  = pure <$> instanceD (cxt []) (conT ''Lift `appT` conT tyName)
      [valD (varP 'lift) (normalB body) []]
  where
    body = [e|liftAbstract lift $(lift fromConcrete) $(varE fromAbstract) |]
