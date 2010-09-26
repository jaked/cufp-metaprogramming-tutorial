{-# LANGUAGE QuasiQuotes #-}
import Data.Object.Json.QQ (json,MayAnti(..),JsonQ)
import Data.ByteString
import GHC.Real (Ratio((:%)))
import Data.Object
import Data.Object.Json

main = mapM_ print [x1,x2,x3,x4]
  where x1 = [$json|42|]
        x2 = [$json|["foo",true]|]
        x3 = [$json|{"name": "foo", "age": 42, "admin": false}|]
        x4 = [$json|[$(x1),$(x2)]|]
