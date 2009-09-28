

{-# LANGUAGE StandaloneDeriving
           , RelaxedPolyRec
  #-}


module Data.JSON.Schema where


import Data.Ord
import Data.Word
import Data.List (permutations)
import Data.Set as Set

import Data.Trie as Trie

import qualified Data.JSON.Simple as Simple




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




{-| The type of JSON schemas. We treat the atomic types simply whereas objects
    and arrays are treated specially.

    Objects are treated as maps of keys to sets of schema types. Say a certain
    type of object sometimes has a string at a certain key and sometimes has a
    null at that key; we should merge them and say the schema of that key is a
    union of string and null.

    Arrays admit measure in the sense of how many elements there are of a
    certain kind. We support three measures at present: any, one or more and
    individual counts. We expect the "any" measure to prevail practice. Arrays
    are also ordered; so one can distinguish an array that interleaves strings
    and ints from one that is all strings and then all ints.
 -}
data Schema counter
  = Num
  | Str
  | Bool
  | Null
  | Obj (Props counter)
  | Arr (Elements counter)
deriving instance (Eq counter) => Eq (Schema counter)
deriving instance (Ord counter) => Ord (Schema counter)


{-| Determine a schema for one JSON data item.
 -}
schema :: (Counter counter) => Simple.JSON -> Schema counter
schema json                  =  case json of
  Simple.Object trie        ->  Obj $ props trie
  Simple.Array list         ->  Arr . Elements $ schemas list
  Simple.String _           ->  Str
  Simple.Number _           ->  Num
  Simple.Boolean _          ->  Bool
  Simple.Null               ->  Null


props :: (Counter counter) => Trie.Trie Simple.JSON -> Props counter
props                        =  Props . fmap (Set.singleton . schema)


{-| Develop a schema for a list of JSON data, collating schemas according to
    the measure, a well-ordered semigroup. 
 -}
schemas :: (Counter counter) => [Simple.JSON] -> [(counter, Schema counter)] 
schemas json                 =  foldr collate []
                                  [ (bottom, schema e) | e <- json ]


{-| Collate a list of counted schemas. Alike counted schemas that are adjacent
    are replaced by a counted schema with an incremented counter. This
    operation is mutually recursive with 'merge', in order to merge comaptible
    object definitions before collating.
 -}
collate
 :: (Counter counter, Counter counter')
 => (counter, Schema counter')
 -> [(counter, Schema counter')]
 -> [(counter, Schema counter')]
collate s []                 =  [s]
collate (c0, Obj p0) ((c1, Obj p1):t)
  | match p0 p1              =  (c0 `plus` c1, Obj $ merge p0 p1):t
  | otherwise                =  (c0, Obj p0):(c1, Obj p1):t
collate (c0, schema0) ((c1, schema1):t)
  | schema0 == schema1       =  (c0 `plus` c1, schema0):t
  | otherwise                =  (c0, schema0):(c1, schema1):t




data Props counter           =  Props (Trie.Trie (Set.Set (Schema counter)))
deriving instance (Eq counter) => Eq (Props counter)
instance (Ord counter) => Ord (Props counter) where
  compare (Props trie0) (Props trie1) = comparing Trie.toList trie0 trie1

{-| Merge two property sets. This operation is mutually recursive with our
    'collate' and relies on polymorphic recusion in 'collate'.
 -}
merge
 :: (Counter counter)
 => Props counter
 -> Props counter
 -> Props counter
merge (Props a) (Props b)    =  Props $ Trie.mergeBy ((Just .) . merge') a b
 where
  merge'                     =  ((count_in . merge'' . count_out) .) . Set.union
   where
    --  We use the unary (existence) counter so that it collates set-like. 
    count_out                =  fmap ((,) ()) . Set.toList
    count_in                 =  Set.fromList . fmap snd
  merge'' [   ]              =  []
  merge'' (h:t)              =  foldr collate' t (h:t)
   where
    --  We expect only very small sets of schemas.
    collate' schema          =  shortest . fmap (collate schema) . permutations
  shortest [   ]             =  []
  shortest (h:t)             =  foldr shortest' h t
   where
    shortest' x h
      | length h < length x  =  h
      | otherwise            =  x

match
 :: (Counter counter)
 => Props counter
 -> Props counter
 -> Bool
match (Props a) (Props b)    =  Trie.keys a == Trie.keys b


data Elements counter        =  Elements [(counter, Schema counter)]
deriving instance (Eq counter) => Eq (Elements counter)
deriving instance (Ord counter) => Ord (Elements counter)


data OneMany                 =  One | Many
deriving instance Eq OneMany
deriving instance Ord OneMany
deriving instance Show OneMany




{-| A well-ordered semigroup has a minimal element and an associative
    operation. These are used to provide measures for schema. At present, we
    allow three measures: whether there is one or more of a schema (measured
    with '()'), whether there is one or more than one of an item (measured with
    'OneMany') and positive counts of items (measured with 'Word').
 -}
class (Eq t, Show t, Ord t) => Counter t where
  bottom                    ::  t
  plus                      ::  t -> t -> t

instance Counter OneMany where
  bottom                     =  One
  plus _ _                   =  Many

instance Counter Word where
  bottom                     =  1
  plus                       =  (+)

instance Counter () where
  bottom                     =  ()
  plus _ _                   =  ()





