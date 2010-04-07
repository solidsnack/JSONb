

module Text.JSONb.Schema.Display where


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
import Data.List (foldl1')
import Data.Word
import Data.ByteString.Lazy.Char8 hiding (any, foldl1')
import qualified Data.Set as Set

import qualified Data.Trie as Trie

import Text.JSONb.Schema




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
    Obj (Props trie)        ->  pack "{ " `append` f trie `append` pack "\n}"
     where
      m                      =  longest_key_len trie
      f                      =  dent 2 . unlines . Trie.toListBy prop_bytes
       where
        prop_bytes k set     =  k' `append` colon `append` join set'
         where
          colon              =  take (m - length k') space `append` pack " : "
          k'                 =  fromChunks [k]
          bar                =  '\n' `cons` take m space `append` pack " | "
          set'               =  (fmap bytes . Set.toList) set
          join               =  if must_be_multiline 3 set'
                                  then  intercalate bar . fmap (dent (m + 3))
                                  else  intercalate (pack " | ")
    Arr (Elements list)     ->  if must_be_multiline 1 list'
      then  pack "[ " `append` intercalate nl list' `append` pack "\n]"
      else  pack "[ " `append` intercalate (pack " ") list' `append` pack " ]"
     where
      nl                     =  pack "\n  "
      list'                  =  fmap bytes list

len                          =  64

must_be_multiline s items    =  broken items || too_long s items

broken                       =  any (elem '\n')

too_long s                   =  (> len + s) . sum . fmap ((+s) . length)

dent n                       =  intercalate ('\n' `cons` take n space) . lines

space                        =  repeat ' '

{-| Warning -- does not work on empty tries. 
 -}
longest_key_len              =  length . foldl1' longest . lazify . Trie.keys
 where
  lazify                     =  fmap $ fromChunks . (:[])
  longest x h
    | length h > length x    =  h
    | otherwise              =  x


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




