

{-# LANGUAGE ScopedTypeVariables
           , ParallelListComp
  #-}


{-| Parse UTF-8 JSON into native Haskell types.
 -}


module Text.JSONb.Decode where


import Data.Char
import Data.Ratio ((%))
import Prelude hiding (length, null, last, takeWhile)
import Data.ByteString (length, append, empty, ByteString)
import Data.ByteString.Char8 (snoc, cons, pack)
import Control.Applicative hiding (empty)

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Trie.Convenience as Trie
import Data.Attoparsec (eitherResult)
import Data.Attoparsec.Char8 hiding (string, isDigit)
import qualified Data.Attoparsec.Char8 as Attoparsec
import Data.ByteString.Nums.Careless

import Text.JSONb.Simple




{-| Interpret a 'ByteString' as any JSON literal.
 -}
decode                      ::  ByteString -> Either String JSON
decode bytes                 =  (eitherResult . Attoparsec.parse json) bytes


{-| Split out the first parseable JSON literal from the input, returning
    the result of the attempt along with the remainder of the input or the
    whole input if no parseable item was discovered.
 -}
break                       ::  ByteString -> (Either String JSON, ByteString)
break bytes                  =  case Attoparsec.parse json bytes of
  Done remainder result     ->  (Right result, remainder)
  Fail _ _ s                ->  (Left s, bytes)
  Partial _                 ->  (Left "Partial", bytes)


{-| Tries to parse any JSON literal.
 -}
json                        ::  Parser JSON
json                         =  do
  whitespace
  choice [object, array, string, number, boolean, null]


{-| Parse a JSON object (dictionary).
 -}
object                      ::  Parser JSON
object                       =  do
  char '{'
  whitespace
  Object . Trie.fromListS <$> choice
    [ whitespace >> char '}' >> return []
    , properties []
    ]
 where
  properties acc             =  do
    key                     <-  string_literal
    whitespace
    char ':'
    something               <-  json
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
array                       ::  Parser JSON
array                        =  do
  char '['
  Array <$> choice
    [ whitespace >> char ']' >> return []
    , elements []
    ]
 where
  elements acc               =  do
    something               <-  json
    whitespace
    let
      acc'                   =  something : acc
      finish                 =  char ']' >> return (reverse acc')
    choice
      [ char ',' >> whitespace >> choice [finish, elements acc']
      , finish
      ]


{-| Parses a string literal, unescaping as it goes.
 -}
string                      ::  Parser JSON
string                       =  String <$> string_literal


{-| Parses a numeric literal to a @Rational@.
 -}
number                      ::  Parser JSON
number                       =  Number <$> do
  (sign :: Rational)        <-  (char '-' *> pure (-1)) <|> pure 1
  i                         <-  just_zero <|> positive_number
  f                         <-  option 0 fractional
  e                         <-  option 0 (exponentialE *> signed decimal)
  return (sign * (i + f) * (10^^e))
 where
  exponentialE               =  char 'e' <|> char 'E'
  fractional                 =  do
    c                       <-  char '.'
    digits                  <-  takeWhile1 isDigit
    return (int digits % (10^(length digits)))
  just_zero                  =  char '0' *> pure 0
  positive_number = pure ((int .) . cons) <*> satisfy hi <*> takeWhile isDigit
   where
    hi d                     =  d > '0' && d <= '9'


{-| Parse a JSON Boolean literal.
 -}
boolean                     ::  Parser JSON
boolean                      =  Boolean <$> choice
  [ s_as_b "true" >> pure True
  , s_as_b "false" >> pure False
  ]


{-| Parse a JSON null literal.
 -}
null                        ::  Parser JSON
null                         =  s_as_b "null" >> return Null




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
string_literal              ::  Parser ByteString
string_literal               =  char '"' >> recurse empty
 where
  recurse acc                =  do
    text                    <-  takeWhile (not . (`elem` "\\\""))
    choice
      [ char '"' >> return (acc `append` text)
      , do
          char '\\'
          c                 <-  escape_sequence
          recurse (acc `append` text `append` UTF8.fromString [c])
      ]
   where
    escape_sequence          =  do
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


s_as_b s                     =  Attoparsec.string (pack s)

