{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Data.Object.Json.QQ where

import Data.Data
import Data.Object
import Data.Object.Json
import Data.Object.Json.DecodeParsec (dataObject, stringLiteral, jsonScalar)
import Data.Generics (extQ)

import Language.Haskell.TH (Q,Loc(..))
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Lift.Extras
import Text.Parsec hiding ((<|>))
import Text.Parsec.ByteString
import Text.Parsec.Pos
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.UTF8 as UTF8

import Data.Word
$(deriveLiftAbstract ''Word8 'fromInteger 'toInteger)
$(deriveLiftAbstract ''ByteString 'S.pack 'S.unpack)

parseWithPos :: Parser a -> SourceName -> (Line,Column) -> String -> Either ParseError a
parseWithPos p sn (l,c) inp = parse p' sn (UTF8.fromString inp)
  where p' = setPosition (newPos sn l c) >> p

data MayAnti a = V a
               | A String
  deriving (Eq,Ord,Typeable,Data,Show,Read)

mayAnti :: (a -> b) -> (String -> b) -> MayAnti a -> b
mayAnti f _ (V v) = f v
mayAnti _ f (A a) = f a

fromMayAnti :: a -> MayAnti a -> a
fromMayAnti = mayAnti id . const

-- this antiquotation parser is a bit rough
antiquotation :: Parser String
antiquotation = string "$(" >> manyTill (noneOf "()") (char ')')

orAnt :: Parser a -> Parser (MayAnti a)
orAnt p = (A <$> antiquotation) <|> (V <$> p)

data Generator p e =
  Generator { gPat :: p, gList :: e }
data Comprehension p e =
  Comprehension { cExp :: e, cGens :: [Generator p e] }

generator :: Parser p -> Parser e -> Parser p e
generator parsePat parseExp = liftM2 Generator parsePat (string "<-" >> parseExp)

{-

[ { "name": n
  , "age":  i }
| n <- ["foo", "bar"]
, i <- [42, 64]]

-}
compr :: Parser p -> Parser e -> Parser p e
compr parsePat parseExp
  = do char '['
       e <- jsonQ
       char '|'
       qs <- generator parsePat parseExp `sep1By` char ','
       char ']'
       return $ Comprehension e qs

type JsonQ = Object (MayAnti ByteString) (MayAnti JsonScalar)
type Json = Object ByteString JsonScalar

jsonQ :: Parser JsonQ
jsonQ = dataObject (orAnt stringLiteral) (orAnt jsonScalar)

parseQ :: Parser a -> String -> Q a
parseQ p inp = do loc <- TH.location
                  let sn = loc_filename loc
                      pos = loc_start loc
                  either (fail . show) return $
                    parseWithPos p sn pos inp

antiVarE :: MayAnti a -> Maybe TH.ExpQ
antiVarE (V _) = Nothing
antiVarE (A a) = Just . TH.varE . TH.mkName $ a

antiVarP :: MayAnti a -> Maybe TH.PatQ
antiVarP (V _) = Nothing
antiVarP (A a) = Just . TH.varP . TH.mkName $ a

antiScalarE :: MayAnti JsonScalar -> Maybe TH.ExpQ
antiScalarE = antiVarE
antiScalarP :: MayAnti JsonScalar -> Maybe TH.PatQ
antiScalarP = antiVarP
antiKeyE :: MayAnti ByteString -> Maybe TH.ExpQ
antiKeyE = antiVarE
antiKeyP :: MayAnti ByteString -> Maybe TH.PatQ
antiKeyP = antiVarP

class LiftScalar a where
  liftScalar :: a -> TH.ExpQ

instance Lift a => LiftScalar (MayAnti a) where
  liftScalar (V v) = [e| Scalar $(lift v) |]
  liftScalar (A a) = TH.varE . TH.mkName $ a

instance LiftScalar ByteString where
  liftScalar = lift

instance (Lift key, LiftScalar scalar) => Lift (Object key scalar) where
  lift (Scalar    x) = liftScalar x
  lift (Sequence  x) = [e| Sequence  $(lift x) |]
  lift (Mapping   x) = [e| Mapping   $(lift x) |]

instance Lift JsonScalar where
  lift (JsonString   x) = [e| JsonString   $(lift x) |]
  lift (JsonNumber   x) = [e| JsonNumber   $(lift x) |]
  lift (JsonBoolean  x) = [e| JsonBoolean  $(lift x) |]
  lift JsonNull         = [e| JsonNull               |]

instance Lift a => Lift (MayAnti a) where
  lift (V v) = lift v
  lift (A a) = TH.varE . TH.mkName $ a

removeAntiquots :: JsonQ -> JsonObject
removeAntiquots = mapKeysValues (fromMayAnti err) (fromMayAnti err)
  where err = error "removeAntiquots: unexpected antiquotation"

coe :: TH.Name -> TH.ExpQ -> TH.ExpQ
coe t e = [e|($(e) :: $(TH.conT t))|]

json :: QuasiQuoter
json = QuasiQuoter jsonE jsonP
  where jsonP s = parseJsonQ s >>= dataToPatQ (const Nothing `extQ` antiScalarP `extQ` antiKeyP)
        -- jsonE s = parseJsonQ s >>= dataToExpQ (const Nothing `extQ` antiScalarE `extQ` antiKeyE)
        jsonE s = coe ''Json . lift =<< parseQ jsonQ s
