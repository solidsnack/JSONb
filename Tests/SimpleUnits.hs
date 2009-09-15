

{-# LANGUAGE StandaloneDeriving
  #-}


import Data.List (concatMap, nub)
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


prop_arrays_parse            =  forAll (elements tests) with_classifiers
 where
  with_classifiers          ::  (String, [JSONb.JSON], [String]) -> Property
  with_classifiers           =  compound (property . array_parse) classifiers
   where
    compound                 =  foldl (flip ($))
  array_parse (s, a, info)   =  rt s == (Right . JSONb.Array) a
  classifiers                =  (fmap classifier . nub . concatMap third) tests 
   where
    third (_,_,t)            =  t
    classifier string p x    =  classify (string `elem` third x) string $ p x
  tests =
    [ ( "[ 7, 6 ]", JSONb.Number `fmap` [7, 6]
      , ["excessive spacing", "integers"] )
    , ( "[7,6]", JSONb.Number `fmap` [7, 6]
      , ["compact spacing", "integers"] )
    , ( "[7.6, 21]", JSONb.Number `fmap` [7.6, 21.0]
      , ["normal spacing", "floating point"] )
    , ( "[22.0 ,7.6,]", JSONb.Number `fmap` [22, 7.6]
      , ["weird comma spacing", "extra comma", "floating point"] )
    , ( "[\"22.0\" ,7.6,]"
      , [(JSONb.String . Strict.pack) "22.0", JSONb.Number 7.6]
      , ["weird comma spacing", "extra comma", "floating point", "string"] )
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




