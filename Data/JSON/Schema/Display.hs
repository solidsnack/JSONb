

module Data.JSON.Schema.Display where


import Prelude hiding
  ( lines
  , unlines
  , tail
  , null
  , unwords
  , length
  , repeat
  , elem
  , take
  , concat
  )
import Data.Ord
import Data.Word
import Data.ByteString.Lazy.Char8 hiding (any)
import qualified Data.Set as Set

import qualified Data.Trie as Trie

import Data.JSON.Schema




class Display t where
  {-| Provide a formatted 'ByteString' for the displayable.
   -}
  bytes                     ::  t -> ByteString


instance (Display counter) => Display (Schema counter) where
  bytes schema               =  case schema of
    Num                     ->  pack "num"
    Str                     ->  pack "str"
    Bool                    ->  pack "bool"
    Null                    ->  pack "null"
    Obj (Props trie)        ->  pack "{ " `append` f trie `append` pack " }"
     where
      f                      =  dent 2 . unlines . Trie.toListBy prop_bytes
       where
        prop_bytes k set     =  k' `append` pack ": " `append` join set'
         where
          k'                 =  fromChunks [k]
          k''                =  length k'
          bar                =  '\n' `cons` take k'' space `append` pack "| "
          set'               =  (fmap bytes . Set.toList) set
          join               =  if must_be_multiline 3 set'
                                  then  intercalate bar . fmap (dent (k'' + 2))
                                  else  intercalate (pack " | ")
    Arr (Elements list)     ->  pack "[ " `append` inner `append` pack " ]"
     where
      inner                  =  if must_be_multiline 1 list'
                                  then  intercalate (pack "\n  ") list'
                                  else  intercalate (pack " ") list'
                                 where
                                  list'        =  fmap bytes list

len                          =  64

must_be_multiline s items    =  broken items || too_long s items

broken                       =  any (elem '\n')

too_long s                   =  (> len + s) . sum . fmap ((+s) . length)

dent n                       =  intercalate ('\n' `cons` take n space) . lines

space                        =  repeat ' '


instance Display () where
  bytes _                    =  empty


instance Display OneMany where
  bytes One                  =  empty
  bytes Many                 =  singleton '+'


instance Display Word where
  bytes                      =  (' ' `cons`) . pack . show


instance (Display counter) => Display (counter, Schema counter) where
  bytes (count, schema)      =  bytes schema `append` bytes count




instance (Display counter) => Show (Schema counter) where
  show                       =  unpack . bytes




