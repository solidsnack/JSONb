

{-# LANGUAGE StandaloneDeriving
  #-}


import Data.List (concatMap, nub)
import Data.Ratio
import Data.ByteString.Char8 (pack)
import Test.QuickCheck

import Data.Trie
import qualified Data.ByteString.UTF8 as UTF8

import qualified Text.JSONb as JSONb




prop_structures_parse        =  samples structure_tests


samples tests                =  forAll (elements tests) with_classifiers
 where
  with_classifiers          ::  (String, JSONb.JSON, [String]) -> Property
  with_classifiers           =  compound (property . array_parse) classifiers
   where
    compound                 =  foldl (flip ($))
  array_parse (s, j, info)   =  rt s == Right j
  classifiers                =  (fmap classifier . nub . concatMap third) tests 
   where
    third (_,_,t)            =  t
    classifier string p x    =  classify (string `elem` third x) string $ p x


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


prop_string_round_trip s     =  high . escapes $ case round_trip bytes of
  Right (JSONb.String b)    ->  bytes == b
  _                         ->  False
 where
  bytes                      =  UTF8.fromString s
  round_trip = JSONb.decode . JSONb.encode JSONb.Compact . JSONb.String
  high                       =  classify (any (> '\x7f') s) "above ASCII"
  escapes                    =  classify (any JSONb.escaped s) "escaped chars"


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


rt                           =  JSONb.decode . pack . (++ " ")
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  We have to add a space so that the number parser terminates.  
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}


structure_tests =
  [ ( "[ 7, 6 ]", (JSONb.Array . fmap JSONb.Number) [7, 6]
    , ["array", "excessive spacing", "integers"] )
  , ( "[]", JSONb.Array []
    , ["array", "compact spacing", "empty"] )
  , ( "[ ]", JSONb.Array []
    , ["array", "normal spacing", "empty"] )
  , ( "[7,6]", (JSONb.Array . fmap JSONb.Number) [7, 6]
    , ["array", "compact spacing", "integers"] )
  , ( "[7.6, 21]", (JSONb.Array . fmap JSONb.Number) [7.6, 21.0]
    , ["array", "normal spacing", "floats"] )
  , ( "[22.0 ,7.6,]", (JSONb.Array . fmap JSONb.Number) [22, 7.6]
    , ["array", "weird comma spacing", "extra comma", "floats"] )
  , ( "[\"22.0\" ,7.6,]"
    , JSONb.Array [(JSONb.String . pack) "22.0", JSONb.Number 7.6]
    , ["array", "weird comma spacing", "extra comma", "floats", "strings"] )
  , ( "{ \"ixion\":6 }"
    , (JSONb.Object . fromList) [(pack "ixion", JSONb.Number 6)]
    , ["object", "no commas", "integers"] )
  , ( "{ \"Ack\":\"Success\" ,\"Build\" :\"e605_core_Bundled_8000231_R1\"}"
    , (JSONb.Object . fromList)
        [ (pack "Ack", JSONb.String (pack "Success"))
        , ( pack "Build"
          , JSONb.String (pack "e605_core_Bundled_8000231_R1") ) ]
    , ["object", "random spacing", "strings"] )
  , ( "{\n\"Ack\"\n:\n\"Success\" , \"Build\":\"e605_core_Bundled_8000231_R1\"}"
    , (JSONb.Object . fromList)
        [ (pack "Ack", JSONb.String (pack "Success"))
        , ( pack "Build"
          , JSONb.String (pack "e605_core_Bundled_8000231_R1") ) ]
    , ["object", "newlines", "strings"] )
  , ( "{}", JSONb.Object empty
    , ["object", "compact spacing", "empty"] )
  , ( "{ }", JSONb.Object empty
    , ["object", "normal spacing", "empty"] )
  , ( "{\"Ack\":\"Success\",\"Build\":\"e605_core_Bundled_8000231_R1\"}"
    , (JSONb.Object . fromList)
        [ (pack "Ack", JSONb.String (pack "Success"))
        , ( pack "Build"
          , JSONb.String (pack "e605_core_Bundled_8000231_R1") ) ]
    , ["object", "compact spacing", "strings"] )
  ]


