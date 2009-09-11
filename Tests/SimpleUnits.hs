#!/usr/bin/env runhaskell


import Data.Ratio
import Data.ByteString.Lazy.Char8 (pack)
import Test.QuickCheck

import qualified JSONb




array_test                   =  map succeed array_tests_that_should_succeed
 where
  succeed (parse, should)    =  (parse, parse == Right should)


array_tests_that_should_succeed =
  [ ( rt "[ 7, 6 ]"
    , JSONb.Array [JSONb.Number 7, JSONb.Number 6] )
  , ( rt "[7,6]"
    , JSONb.Array [JSONb.Number 7, JSONb.Number 6] )
  , ( rt "[7.6, 21]"
    , JSONb.Array [JSONb.Number 7.6, JSONb.Number 21.0] )
  ]


prop_integers               ::  Int -> Bool
prop_integers i              =  case (JSONb.decode . pack . show) i of
  Right (JSONb.Number r)    ->  r == toRational i
  _                         ->  False


rt                           =  JSONb.decode . pack






