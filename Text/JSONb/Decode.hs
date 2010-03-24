

{-# LANGUAGE ScopedTypeVariables
           , ParallelListComp
  #-}


{-| Parse UTF-8 JSON into native Haskell types.
 -}


module Text.JSONb.Decode where


import Data.Char
import Prelude hiding (null, last, takeWhile)
import Data.ByteString (append, empty, ByteString)
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
decode bytes                 =  (eitherResult . Attoparsec.parse) json bytes


{-| Split out the first parseable JSON literal from the input, returning
    the result of the attempt along with the remainder of the input or the
    whole input if no parseable item was discovered.
 -}
break                       ::  ByteString -> (Either String JSON, ByteString)
break bytes                  =  case Attoparsec.parse json bytes of
  Done remainder result     ->  (result, remainder)
  Fail _ _ s _              ->  (Left s, bytes)
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
  ended                      =  {- notFollowedBy $ -} satisfy oops >> return ()
   where
    oops c                   =  isAlphaNum c || elem c ".+-"


{-| Parse a JSON Boolean literal.
 -}
boolean                     ::  Parser JSON
boolean                      =  Boolean <$> choice
  [ s_as_b "true" >> return True
  , s_as_b "false" >> return False
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

