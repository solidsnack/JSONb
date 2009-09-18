#!/usr/bin/env runhaskell




import Prelude hiding (interact, unlines, tail, null, unwords)
import Data.ByteString.Lazy.Char8 hiding (reverse)
import qualified Data.Set as Set

import qualified Data.Trie as Trie

import qualified JSONb




main                         =  interact (unlines . schemas . progressive)


--schema                       =  unlines . fmap name . progressive_parse
-- where
--  name json                  =  pack $ case json of
--    JSONb.Object _          ->  "Object"
--    JSONb.Array _           ->  "Array"
--    JSONb.String _          ->  "String"
--    JSONb.Number _          ->  "Number"
--    JSONb.Boolean _         ->  "Boolean"
--    JSONb.Null              ->  "Null"


schemas                      =  Set.elems . Set.fromList . fmap schema


schema json                  =  case json of
  JSONb.Object trie         ->  (unwords . curly . schemas . values) trie
  JSONb.Array list          ->  (unwords . square . schemas) list
  JSONb.String _            ->  pack "string"
  JSONb.Number _            ->  pack "num"
  JSONb.Boolean _           ->  pack "bool"
  JSONb.Null                ->  pack "null"
 where
  values                     =  fmap snd . Trie.toList
  curly                      =  ([pack "{"] ++) . (++ [pack "}"])
  square                     =  ([pack "["] ++) . (++ [pack "]"])


progressive                  =  progressive_parse' []
 where
  progressive_parse' acc bytes
    | null bytes             =  reverse acc
    | otherwise              =  case JSONb.break bytes of
      (Left _, _)           ->  progressive_parse' acc (tail bytes)
      (Right piece, rem)    ->  progressive_parse' (piece:acc) rem



