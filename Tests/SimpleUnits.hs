

{-# LANGUAGE StandaloneDeriving
  #-}


import Data.Ratio
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as Strict
import Test.QuickCheck

import Data.Trie

import qualified JSONb




object_test                  =  map succeed object_tests_that_should_succeed
 where
  succeed (parse, should)    =  (parse, parse == (Right . JSONb.Object) should)
  object_tests_that_should_succeed =
    [ (rt "{ \"ixion\":6 }", fromList [(Strict.pack "ixion", JSONb.Number 6)])
    ]


array_test                   =  map succeed array_tests_that_should_succeed
 where
  succeed (parse, should)    =  (parse, parse == (Right . JSONb.Array) should)
  array_tests_that_should_succeed =
    [ ( rt "[ 7, 6 ]", [JSONb.Number 7, JSONb.Number 6] )
    , ( rt "[7,6]", [JSONb.Number 7, JSONb.Number 6] )
    , ( rt "[7.6, 21]", [JSONb.Number 7.6, JSONb.Number 21.0] )
    , ( rt "[22.0 ,7.6,]", [JSONb.Number 22, JSONb.Number 7.6] )
    ]


data NumArray                =  NumArray [Rational]
deriving instance Show NumArray




prop_integer_round_trip     ::  Integer -> Property
prop_integer_round_trip n    =  collect bin $ case (rt . show) n of
  Right (JSONb.Number r)    ->  r == fromIntegral n
  _                         ->  False
 where
  bin
    | n == 0                 =  Bounds (Open (-1)) (Open 1)
    | n >= 1 && n < 100      =  Bounds (Closed 1) (Open 100)
    | n > -100 && n <= -1    =  Bounds (Open (-100)) (Closed (-1))
    | n <= -100              =  Bounds Infinite (Closed (-100))
    | n >= -100              =  Bounds (Closed (100)) Infinite


prop_double_round_trip      ::  Double -> Property
prop_double_round_trip n     =  collect bin $ case (rt . show) n of
  Right (JSONb.Number r)    ->  fromRational r == n
  _                         ->  False
 where
  bin
    | n < 1 && n > -1        =  Bounds (Open (-1)) (Open 1)
    | n >= 1 && n < 100      =  Bounds (Closed 1) (Open 100)
    | n > -100 && n <= -1    =  Bounds (Open (-100)) (Closed (-1))
    | n <= -100              =  Bounds Infinite (Closed (-100))
    | n >= -100              =  Bounds (Closed (100)) Infinite


data Bounds n where
  Bounds :: (Show n, Num n) => Bound n -> Bound n -> Bounds n
instance (Show n) => Show (Bounds n) where
  show (Bounds l r)          =  case (l, r) of
    (Open l, Open r)        ->  "(" ++ show l ++ ".." ++ show r ++ ")"
    (Closed l, Closed r)    ->  "[" ++ show l ++ ".." ++ show r ++ "]"
    (Closed l, Open r)      ->  "[" ++ show l ++ ".." ++ show r ++ ")"
    (Open l, Closed r)      ->  "(" ++ show l ++ ".." ++ show r ++ "]"
    (Closed l, Infinite)    ->  "[" ++ show l ++ ".."
    (Infinite, Closed r)    ->  ".." ++ show r ++ "]"
    (Open l, Infinite)      ->  "(" ++ show l ++ ".."
    (Infinite, Open r)      ->  ".." ++ show r ++ ")"
    (Infinite, Infinite)    ->  ".."


data Bound n where
  Open                      ::  (Show n, Num n) => n -> Bound n
  Closed                    ::  (Show n, Num n) => n -> Bound n
  Infinite                  ::  (Show n, Num n) => Bound n


rt                           =  JSONb.decode . pack


deepCheck = check (defaultConfig { configMaxTest = 10000})




