#!/usr/bin/env runhaskell


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
    [ ( rt "{ \"ixion\":6 }", fromList [(Strict.pack "ixion", JSONb.Number 6)] )
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


prop_integers               ::  Integer -> Bool
prop_integers n              =  case (rt . show) n of
  Right (JSONb.Number r)    ->  r == fromIntegral n
  _                         ->  False


prop_doubles                ::  Double -> Bool
prop_doubles n               =  case (rt . show) n of
  Right (JSONb.Number r)    ->  fromRational r == n
  _                         ->  False


rt                           =  JSONb.decode . pack






