

module Data.JSON.Schema where


import Data.Trie as Trie
import Data.Set as Set




data Schema counter
  = Num
  | Str
  | Bool
  | Null
  | Obj (Props counter)
  | Arr (Elements counter)
deriving instance (Ord counter) => Ord (Schema counter)

data Props counter           =  Props (Trie.Trie (Set.Set (Schema counter)))
deriving instance (Show counter) => Show (Props counter)

data Elements counter        =  Elements [(counter, Schema counter)]
deriving instance (Show counter) => Show (Elements counter)


data OneMany                 =  One | Many
deriving instance Ord OneMany


class (Ord t) => WellOrderedSemigroup t where
  bottom                    ::  t
  plus                      ::  t -> t -> t

instance WellOrderedSemigroup OneMany where
  bottom                     =  One
  plus _ _                   =  Many

instance WellOrderedSemigroup Word where
  bottom                     =  1
  plus                       =  (+)

instance WellOrderedSemigroup () where
  bottom                     =  ()
  plus _ _                   =  ()





