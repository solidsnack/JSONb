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




main                         =  interact show_it
 where
  show_it                    =  unlines . fmap display . schemas . progressive
   where
    schemas                 ::  [JSONb.JSON] -> [Schema OneMany]
    schemas                  =  schemas


progressive                  =  progressive_parse' []
 where
  progressive_parse' acc bytes
    | null bytes             =  reverse acc
    | otherwise              =  case JSONb.break bytes of
      (Left _, _)           ->  progressive_parse' acc (tail bytes)
      (Right piece, rem)    ->  progressive_parse' (piece:acc) rem


schemas                     ::  (Plus t) => [JSONb.JSON] -> [Schema t]
schemas                      =  foldr collate [] . fmap schema

schema                      ::  (Plus t) => JSONb.JSON -> Schema t
schema json                  =  case json of
  JSONb.Object trie         ->  Obj one $ fmap schema trie
  JSONb.Array list          ->  Arr one $ schemas list
  JSONb.String _            ->  Str one
  JSONb.Number _            ->  Num one
  JSONb.Boolean _           ->  Bool one
  JSONb.Null                ->  Null one
 where
  values                     =  fmap snd . Trie.toList


data Schema t
  = Num t
  | Str t
  | Bool t
  | Null t
  | Obj t (Trie.Trie (Schema t))
  | Arr t [Schema t]
deriving instance (Eq t) => Eq (Schema t)

data OneMany                 =  One | Many
deriving instance Eq OneMany


collate :: (Plus t) => Schema t -> [Schema t] -> [Schema t]
collate                      =  flip c
 where
  c (Num x : t) (Num y)      =  Num (x `plus` y) : t
  c (Str x : t) (Str y)      =  Str (x `plus` y) : t
  c (Bool x : t) (Bool y)    =  Bool (x `plus` y) : t
  c (Null x : t) (Null y)    =  Null (x `plus` y) : t
  c (Obj x a : t) (Obj y b)  |  a == b         =  Obj (x `plus` y) b : t
                             |  otherwise      =  Obj x b : Obj y a : t
  c (Arr x a : t) (Arr y b)  |  a == b         =  Arr (x `plus` y) b : t
                             |  otherwise      =  Arr x b : Arr y a : t
  c list b                   =  b : list


class (Eq t) => Plus t where
  plus                      ::  t -> t -> t
  one                       ::  t

instance Plus OneMany where
  one                        =  One
  plus _ _                   =  Many

instance Plus Word where
  one                        =  1
  plus                       =  (+)

instance Plus () where
  one                        =  ()
  plus _ _                   =  ()


class Display t where
  display                   ::  t -> ByteString

instance Display OneMany where
  display One                =  empty
  display Many               =  singleton '+'

instance Display Word where
  display                    =  pack . show

instance Display () where
  display _                  =  empty

instance (Display t) => Display (Schema t) where
  display (Num t)            =  unwords [pack "num", display t]
  display (Str t)            =  unwords [pack "str", display t]
  display (Bool t)           =  unwords [pack "bool", display t]
  display (Null t)           =  unwords [pack "null", display t]
  display (Obj t trie)       =  unwords [(c . b . props) trie, display t]
  display (Arr t list)       =  unwords [s . b $ fmap display list, display t]

c                            =  (pack "{ " `append`) . (`append` pack " }")

s                            =  (pack "[ " `append`) . (`append` pack " ]")

props :: (Display t) => Trie.Trie (Schema t) -> [ByteString]
props                        =  Trie.toListBy prop
 where
  prop k v                   =  k' `append` pack ": " `append` dent v
   where
    dent                     =  intercalate spaces . lines . display
    spaces                   =  take (length k' + 3) ('\n' `cons` repeat ' ')
    k'                       =  fromChunks [k]

b things                     =  if broken things || overlong things
                                  then  intercalate (pack "\n, ") (fmap dent things)
                                  else  intercalate (pack ", ") things
                                 where
                                  broken       =  any ('\n' `elem`)
                                  overlong     =  (> len) . sum . fmap length
                                  len          =  64
                                  dent         =  intercalate spaces . lines
                                   where
                                    spaces     =  pack "\n  "

