#!/usr/bin/env runhaskell


import Prelude hiding (interact, unlines, tail, null)
import Data.ByteString.Lazy.Char8 hiding (reverse)

import qualified JSONb




main                         =  interact schema


schema                       =  unlines . fmap name . progressive_parse
 where
  name json                  =  pack $ case json of
    JSONb.Object _          ->  "Object"
    JSONb.Array _           ->  "Array"
    JSONb.String _          ->  "String"
    JSONb.Number _          ->  "Number"
    JSONb.Boolean _         ->  "Boolean"
    JSONb.Null              ->  "Null"


progressive_parse            =  progressive_parse' []
 where
  progressive_parse' acc bytes
    | null bytes             =  reverse acc
    | otherwise              =  case JSONb.break bytes of
      (Left _, _)           ->  progressive_parse' acc (tail bytes)
      (Right piece, rem)    ->  progressive_parse' (piece:acc) rem



