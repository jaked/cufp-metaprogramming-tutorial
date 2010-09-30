{-# LANGUAGE TemplateHaskell #-}

module Data.Object.Json.QQ where

import Data.Object
import Data.Object.Json
import Data.Object.Json.DecodeParsec (dataObject, stringLiteral, jsonScalar)

import Language.Haskell.TH (Q,Loc(..))
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Lift.Extras
import Text.Parsec hiding ((<|>))
import Text.Parsec.ByteString
import Text.Parsec.Pos
import Control.Applicative hiding (many)
import Data.ByteString (ByteString)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.ByteString as S
import qualified Data.ByteString.UTF8 as UTF8

import Data.Word
$(deriveLiftAbstract ''Word8 'fromInteger 'toInteger)
$(deriveLiftAbstract ''ByteString 'S.pack 'S.unpack)

parseWithPos :: Parser a -> SourceName -> (Line,Column) -> String -> Either ParseError a
parseWithPos p sn (l,c) inp = parse p' sn (UTF8.fromString inp)
  where p' = setPosition (newPos sn l c) >> p

newtype Antiquotation = Ant { ant :: [String] }
  deriving (Eq,Ord,Show,Read)

data MayAnti a = V a
               | A Antiquotation
  deriving (Eq,Ord,Show,Read)

-- A rough antiquotation parser
antiquotation :: Parser Antiquotation
antiquotation = Ant <$> (string "$(" *> ident `sepBy1` spaces1 <* char ')')
  where spaces1 = many1 space
        ident = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

orAnt :: Parser a -> Parser (MayAnti a)
orAnt p = (A <$> antiquotation) <|> (V <$> p)

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

class LiftScalar a where
  liftScalar :: a -> TH.ExpQ

instance Lift a => LiftScalar (MayAnti a) where
  liftScalar (V v) = [e| Scalar $(lift v) |]
  liftScalar (A a) = lift a

instance LiftScalar ByteString where
  liftScalar = lift

instance Lift Antiquotation where
  -- lift = TH.varE . TH.mkName . ant
  lift = TH.appsE . map (TH.varE . TH.mkName) . ant

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
  lift (A a) = lift a

coe :: TH.Name -> TH.ExpQ -> TH.ExpQ
coe t e = [e|($(e) :: $(TH.conT t))|]

json :: QuasiQuoter
json = QuasiQuoter jsonE jsonP
  where jsonP _ = error "Patterns are not supported by the json quasiquoter"
        jsonE s = coe ''Json . lift =<< parseQ jsonQ s
