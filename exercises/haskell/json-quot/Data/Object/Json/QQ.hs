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

-- A rough antiquotation parser
antiquotation :: Parser String
antiquotation = string "$(" >> manyTill (noneOf "()") (char ')')

orAnt :: Parser a -> Parser (MayAnti a)
orAnt p = (A <$> antiquotation) <|> (V <$> p)

type JsonQ = Object (MayAnti ByteString) (MayAnti JsonScalar)
type Json = Object ByteString JsonScalar

jsonQ :: Parser JsonQ
jsonQ = dataObject {- TODO -} {- TODO -}

parseQ :: Parser a -> String -> Q a
parseQ p inp = do loc <- TH.location
                  let sn = loc_filename loc
                      pos = loc_start loc
                  either (fail . show) return $
                    parseWithPos p sn pos inp

class LiftScalar a where
  liftScalar :: a -> TH.ExpQ

instance (Lift key, LiftScalar scalar) => Lift (Object key scalar) where
  lift _ = -- TODO

instance Lift JsonScalar where
  lift _ = -- TODO

instance Lift a => Lift (MayAnti a) where
  lift _ = -- TODO

json :: QuasiQuoter
json = QuasiQuoter jsonE jsonP
  where jsonP _ = error "Patterns are not supported by the json quasiquoter"
        jsonE s = {- TODO -} =<< parseQ jsonQ s
