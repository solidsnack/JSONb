

module Data.ByteString.JSON.Encode where


import Data.Ratio
import Data.ByteString.Char8

import Data.Trie hiding (singleton)

import Data.JSON.Simple
import Text.JSON.Escape




{-| Encode 'JSON' as a 'ByteString'. All strings are treated as UTF-8; ASCII
    control characters are escaped and other byte formations are left as is.
 -}
encode                      ::  Style -> JSON -> ByteString
encode style@Compact json    =  case json of
  Object trie               ->  '{' `cons` pairs trie `snoc` '}'
  Array elems               ->  '[' `cons` elements elems `snoc` ']'
  String s                  ->  stringify s
  Number r
    | denominator r == 1    ->  (pack . show . numerator) r
    | otherwise             ->  (pack . show) (fromRational r :: Double)
  Boolean True              ->  pack "true"
  Boolean False             ->  pack "false"
  Null                      ->  pack "null"
 where
  comcat                     =  intercalate (singleton ',')
  elements                   =  comcat . fmap (encode style)
  pairs                      =  comcat . toListBy pair
   where
    pair k v                 =  stringify k `snoc` ':' `append` encode style v


{-| Style of serialization. Compact is the only one that is implemented at
    present.
 -}
data Style                   =  Compact -- | LightSpaces | Indented
deriving instance Show Style
deriving instance Eq Style


{-| Escape a 'ByteString' representing a JSON string and wrap it in quote
    marks.
 -}
stringify                   ::  ByteString -> ByteString
stringify s                  =  '"' `cons` escape s `snoc` '"'



