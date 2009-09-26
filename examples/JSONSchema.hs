#!/usr/bin/env runhaskell



import Prelude hiding
  ( interact
  , lines
  , unlines
  , tail
  , null
  , unwords
  , length
  , repeat
  , elem
  , take
  )
import Data.ByteString.Lazy.Char8 hiding (any, reverse, foldr)
import Data.Word
import Data.Monoid
import qualified Data.Set as Set

import qualified Data.Trie as Trie

import qualified JSONb
import qualified Data.JSON.Schema as Schema
import qualified Data.JSON.Schema.Display as Schema




main                         =  interact (display . schemas . progressive)


display                      =  unlines . fmap Schema.counted_bytes


schemas :: [JSONb.JSON] -> [(Schema.OneMany, Schema.Schema Schema.OneMany)]
schemas                      =  Schema.schemas


progressive                  =  progressive_parse' []
 where
  progressive_parse' acc bytes
    | null bytes             =  reverse acc
    | otherwise              =  case JSONb.break bytes of
      (Left _, _)           ->  progressive_parse' acc (tail bytes)
      (Right piece, rem)    ->  progressive_parse' (piece:acc) rem

