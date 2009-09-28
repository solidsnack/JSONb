

{-# LANGUAGE ScopedTypeVariables
           , ParallelListComp
  #-}


{-| Parse UTF-8 JSON into native Haskell types.
 -}

module Data.ByteString.JSON.Decode where


import Data.Char
import Prelude hiding (null, last, takeWhile)
import qualified Data.ByteString as ByteString.Strict
import Data.ByteString.Lazy.Char8
  hiding (reverse, null, takeWhile, elem, concatMap)
import Control.Applicative hiding (empty)

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Trie.Convenience as Trie
import Data.ParserCombinators.Attoparsec.Char8 hiding (string)
import qualified Data.ParserCombinators.Attoparsec.Char8 as Attoparsec
import Data.ByteString.Nums.Careless

import Data.JSON.Simple




{-| Interpret a 'ByteString' as any JSON literal.
 -}
decode :: ByteString -> Either (ParseError, ByteString) JSON
decode bytes                 =  case Attoparsec.parse json bytes of
  (remainder, Left e)       ->  Left (e, remainder)
  (r, Right j)              ->  Right j


{-| Split out the first parseable JSON literal from the input, returning
    the result of the attempt along with the remainder of the input or the
    whole input if not parseable item was discovered.
 -}
break :: ByteString -> (Either ParseError JSON, ByteString)
break bytes                  =  case Attoparsec.parse json bytes of
  (_, Left e)               ->  (Left e, bytes)
  (remainder, result)       ->  (result, remainder)


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
  Object . Trie.fromListS <$> properties []
 where
  properties acc             =  do
    key                     <-  string_literal
    whitespace
    char ':'
    something               <-  json
    whitespace
    let
      acc'                   =  (strictify key, something) : acc
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
  Array <$> elements []
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
string                       =  String . strictify <$> string_literal


{-| Parses a numeric literal to a @Rational@.
 -}
number                      ::  Parser JSON
number                       =  Number <$> do
  (sign :: Rational)        <-  '-' ?> (-1) <|> pure 1
  n                         <-  choice
    [ do
        integer_part        <-  digits
        char '.'
        fractional_part     <-  digits
        return $ float (integer_part `snoc` '.' `append` fractional_part)
    , do
        char '.'
        fractional_part     <-  digits
        return $ float ('.' `cons` fractional_part)
    , do
        integer_part        <-  digits
        return $ int integer_part
    ]
  (exponent <*> pure (sign * n)) <|> (ended >> return (sign * n))
 where
  c ?> r                     =  char c >> pure r
  digits                     =  takeWhile1 isDigit
  exponent                  ::  Parser (Rational -> Rational)
  exponent                   =  do
    char 'e' <|> char 'E'
    op                      <-  '-' ?> (/) <|> '+' ?> (*) <|> pure (*)
    (e :: Int)              <-  int <$> digits
    pure (`op` (10^e)) 
  ended                      =  notFollowedBy $ satisfy oops
   where
    oops c                   =  isAlphaNum c || elem c ".+-"


{-| Parse a JSON Boolean literal.
 -}
boolean                     ::  Parser JSON
boolean                      =  Boolean <$> choice
  [ Attoparsec.string "true" >> return True
  , Attoparsec.string "false" >> return False
  ]


{-| Parse a JSON null literal.
 -}
null                        ::  Parser JSON
null                         =  Attoparsec.string "null" >> return Null




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


{-| Turn a lazy 'ByteString' in to a strict 'ByteString.Strict.ByteString'.
 -}
strictify                   ::  ByteString -> ByteString.Strict.ByteString
strictify                    =  ByteString.Strict.concat . toChunks


