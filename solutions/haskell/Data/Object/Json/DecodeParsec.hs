{-# LANGUAGE ScopedTypeVariables
           , ParallelListComp
  #-}

{- Mainly imported from the JSONb package. -}

{-| Parse UTF-8 JSON into native Haskell types.
 -}

module Data.Object.Json.DecodeParsec where


import Data.Char
import Data.Ratio ((%))
import Prelude hiding (null)
import Data.List (foldl')
import Data.ByteString (append, empty, ByteString)
import Data.ByteString.Char8 (pack)
import Control.Applicative hiding (empty)

import qualified Data.ByteString.UTF8 as UTF8
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.ByteString (Parser)

import Data.Object
import Data.Object.Json




{-| Interpret a 'ByteString' as any JSON literal.
 -}
decode                      ::  ByteString -> Either String JsonObject
decode                       =  either (Left . show) Right . parse json "<string>" 


{-| Tries to parse any JSON literal.
 -}
json                        ::  Parser JsonObject
json                         =  dataObject stringLiteral jsonScalar


{-| Given how to parse keys and scalars, tries to parse any
    Data.Object with a JSON syntax for objects (mappings) and
    arrays (sequences).
 -}
dataObject                  ::  Parser key -> Parser scalar ->
                                Parser (Object key scalar)
dataObject key scalar        =  self where
  self = do
    whitespace
    choice  [  Mapping   <$> object key self
            ,  Sequence  <$> array self
            ,  Scalar    <$> scalar
            ]

{-| Tries to parse any JSON literal.
 -}
jsonScalar                  ::  Parser JsonScalar
jsonScalar                   =  choice [JsonString <$> stringLiteral, number, boolean, null]

{-| Parse a JSON object (dictionary).
 -}
object                      ::  Parser k -> Parser v -> Parser [(k, v)]
object parseKey parseValue   =  do
  char_ '{'
  whitespace
  choice
    [ whitespace >> char '}' >> return []
    , properties []
    ]
 where
  properties acc             =  do
    key                     <-  parseKey
    whitespace
    char_ ':'
    something               <-  parseValue
    whitespace
    let
      acc'                   =  (key, something) : acc
    choice
      [ char ',' >> whitespace >> choice
          [ char '}' >> return acc'
          , properties acc'
          ]
      , char '}' >> return acc'
      ]


{-| Parse a JSON array.
 -}
array                       ::  Parser a -> Parser [a]
array element                =  do
  char_ '['
  choice
    [ whitespace >> char ']' >> return []
    , elements []
    ]
 where
  elements acc               =  do
    something               <-  element
    whitespace
    let
      acc'                   =  something : acc
      finish                 =  char ']' >> return (reverse acc')
    choice
      [ char ',' >> whitespace >> choice [finish, elements acc']
      , finish
      ]


{-| Parses a numeric literal to a @Rational@ and make it a JsonScalar.
 -}
number                      ::  Parser JsonScalar
number                       =  JsonNumber <$> rational

{-| Parses a numeric literal to a @Rational@.
 -}
rational                    ::  Parser Rational
rational                     = do
  sign :: Rational          <-  (char '-' *> pure (-1)) <|> pure 1
  i                         <-  just_zero <|> positive_number
  f                         <-  option 0 fractional
  e :: Integer              <-  option 0 (exponentialE *> integer)
  return (sign * (i + f) * (10^e))
 where
  exponentialE               =  char 'e' <|> char 'E'
  fractional                 =  do
    char_ '.'
    digits                  <-  many1 digit
    return (int digits % (10^ length digits))
  just_zero                  =  char '0' *> pure 0
  positive_number = pure ((int .) . (:)) <*> satisfy hi <*> many digit
   where
    hi d                     =  d > '0' && d <= '9'


{-| Parse a JSON Boolean literal.
 -}
boolean                     ::  Parser JsonScalar
boolean                      =  JsonBoolean <$> choice
  [ string "true"  >> pure True
  , string "false" >> pure False
  ]


{-| Parse a JSON null literal.
 -}
null                        ::  Parser JsonScalar
null                         =  string "null" >> return JsonNull


{-| Per RFC 4627, section 2 "JSON Grammar", only a limited set of whitespace
    characters actually count as insignificant whitespace. 
 -}
whitespace                  ::  Parser ()
whitespace                   =  skipMany (satisfy w)
 where
  w ' '                      =  True          --  ASCII space.
  w '\n'                     =  True          --  Newline.
  w '\r'                     =  True          --  Carriage return.
  w '\t'                     =  True          --  Horizontal tab.
  w _                        =  False         --  Not a JSON space.


{-| Parse a JSON string literal and unescape it but don't wrap it in a string
    constructor (we might wrap it as a dict key instead).
 -}
stringLiteral               ::  Parser ByteString
stringLiteral                =  char '"' >> recurse empty
 where
  recurse acc                =  do
    text                    <-  pack <$> many (noneOf "\\\"")
    choice
      [ char '"' >> return (acc `append` text)
      , do
          char_ '\\'
          c                 <-  escape_sequence
          recurse (acc `append` text `append` UTF8.fromString [c])
      ]
   where
    escape_sequence          =
      choice      [  c >> r  |  c <- fmap char "n/\"rfbt\\u"
                             |  r <- fmap return "\n/\"\r\f\b\t\\" ++ [u]  ]
     where
      u                      =  do
        (a,b,c,d)           <-  (,,,) <$> hex <*> hex <*> hex <*> hex
        return . toEnum      $  a * 0x1000
                             +   b * 0x100
                             +    c * 0x10
                             +     d * 0x1
       where  
        hex                  =  choice digits
         where
          prep (n, chars)    =  fmap (fmap ((+n) . ord) . char) chars
          digits             =  concatMap prep [  (-48, ['0'..'9'])
                                               ,  (-55, ['A'..'F'])
                                               ,  (-87, ['a'..'f'])  ]


char_                       :: Char -> Parser ()
char_ c                      = char c >> pure ()

integer                     :: Parser Integer
integer                      =  (negate <$> (char '-' *> natural))
                            <|> natural

-- | A natural (i.e. non-negative integer) number, in decimal notation.
natural                     :: (Integral a) => Parser a
natural                      = int <$> many1 digit
                            <?> "nonnegative decimal integer"

int                         :: Num a => String -> a
int                          = foldl' (\ a b -> a * 10 + b) 0 . map (\ c -> fromIntegral (fromEnum c - fromEnum '0'))
