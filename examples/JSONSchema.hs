#!/usr/bin/env runhaskell


{-# LANGUAGE NoMonomorphismRestriction
  #-}


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
import System.Exit
import System.IO (stderr, stdout)
import System.Environment
import Data.ByteString.Lazy (hPutStr)
import Data.ByteString.Lazy.Char8 hiding (any, reverse, foldr)
import Data.Word
import qualified Data.Set as Set

import qualified Data.Trie as Trie

import qualified JSONb
import qualified Data.JSON.Schema as Schema
import qualified Data.JSON.Schema.Display as Schema




help                         =  (unlines . fmap pack)
  [ "USAGE: json-schema (--any|--count|--one-many)? < json_file.json"
  , ""
  , "Derives a schema for JSON input, counting according to the option"
  , "spec or using one-many counting by default. An example is probably best."
  , "JSON that looks this:"
  , ""
  , "  { id: 0, name: \"molly\", windage: 1.2 }"
  , "  { id: 0, name: \"edmund\", windage: null }"
  , ""
  , "is assigned this schema under one-many counting:"
  , ""
  , "  { id      : num"
  , "    name    : str"
  , "    windage : num | null"
  , "  }+"
  , ""
  , "but is assigned this schema under plain old integral counting:"
  , ""
  , "  { id      : num"
  , "    name    : str"
  , "    windage : num | null"
  , "  } 2"
  , ""
  , "and is assigned this schema under any counting:"
  , ""
  , "  { id      : num"
  , "    name    : str"
  , "    windage : num | null"
  , "  }"
  , ""
  , "Notice that properties sum types; the schema inferencer assumes JSON"
  , "objects with like property names are the same object type."
  , ""
  ]


main                         =  do
  args                      <-  getArgs
  case args of
    []                      ->  op (Schema.schemas :: Counting Schema.OneMany)
    ["--any"]               ->  op (Schema.schemas :: Counting ())
    ["--one-many"]          ->  op (Schema.schemas :: Counting Schema.OneMany)
    ["--count"]             ->  op (Schema.schemas :: Counting Word)
    ["-h"]                  ->  hPutStr stdout help
    ["-?"]                  ->  hPutStr stdout help
    ["--help"]              ->  hPutStr stdout help
    _                       ->  do
      hPutStr stderr $ pack "!!  Invalid option or options.\n"
      hPutStr stderr help
      exitFailure
 where
  --op                        ::  Counting c -> IO ()
  op schemas                 =  interact (display . schemas . progressive)


display                      =  unlines . fmap Schema.bytes


progressive                  =  progressive_parse' []
 where
  progressive_parse' acc bytes
    | null bytes             =  reverse acc
    | otherwise              =  case JSONb.break bytes of
      (Left _, _)           ->  progressive_parse' acc (tail bytes)
      (Right piece, rem)    ->  progressive_parse' (piece:acc) rem


type Counting c              =  [JSONb.JSON] -> [(c, Schema.Schema c)]


