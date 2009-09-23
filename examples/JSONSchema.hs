#!/usr/bin/env runhaskell


{-

  JSON Schemas:

    document               ::=  array

    element                ::=  num | str | null | bool | object | array

    object                 ::=  "{" Set(element) "}"

    array                  ::=  "[" List(element) "]"

    num                    ::=  "num"
    str                    ::=  "str"
    null                   ::=  "null"
    bool                   ::=  "bool"

 -}



import Prelude hiding (interact, unlines, tail, null, unwords)
import Data.ByteString.Lazy.Char8 hiding (reverse)
import Data.Word
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
  JSONb.String _            ->  pack "str"
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




class Collate t where
  c                         ::  [t] -> t -> [t]

class Recognize t where
  r                         ::  JSONb.JSON -> Schema t

class Display t where
  d                         ::  [t] -> t -> [t]

data Schema t
  = Num t
  | Str t
  | Bool t
  | Null t
  | Obj t (Set.Set (Schema t))
  | Arr t [Schema t]
deriving instance (Eq t) => Eq (Schema t)

data OneMany                 =  One | Many
deriving instance Eq OneMany

instance Collate (Schema ()) where
  c (Num _ : t) (Num _)      =  Num () : t
  c (Str _ : t) (Str _)      =  Str () : t
  c (Bool _ : t) (Bool _)    =  Bool () : t
  c (Null _ : t) (Null _)    =  Null () : t
  c (Obj _ a : t) (Obj _ b)  |  a == b         =  Obj () b : t
                             |  otherwise      =  Obj () b : Obj () a : t
  c (Arr _ a : t) (Arr _ b)  |  a == b         =  Arr () b : t
                             |  otherwise      =  Arr () b : Arr () a : t
  c list b                   =  b : list

instance Collate (Schema OneMany) where
  c (Num _ : t) (Num _)      =  Num Many : t
  c (Str _ : t) (Str _)      =  Str Many : t
  c (Bool _ : t) (Bool _)    =  Bool Many : t
  c (Null _ : t) (Null _)    =  Null Many : t
  c (Obj x a : t) (Obj y b)  |  a == b         =  Obj Many b : t
                             |  otherwise      =  Obj x b : Obj y a : t
  c (Arr x a : t) (Arr y b)  |  a == b         =  Arr Many b : t
                             |  otherwise      =  Arr x b : Arr y a : t
  c list b                   =  b : list

instance Collate (Schema Word) where
  c (Num x : t) (Num y)      =  Num (x+y) : t
  c (Str x : t) (Str y)      =  Str (x+y) : t
  c (Bool x : t) (Bool y)    =  Bool (x+y) : t
  c (Null x : t) (Null y)    =  Null (x+y) : t
  c (Obj x a : t) (Obj y b)  |  a == b         =  Obj (x+y) b : t
                             |  otherwise      =  Obj x b : Obj y a : t
  c (Arr x a : t) (Arr y b)  |  a == b         =  Arr (x+y) b : t
                             |  otherwise      =  Arr x b : Arr y a : t
  c list b                   =  b : list




