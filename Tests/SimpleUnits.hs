

{-# LANGUAGE StandaloneDeriving
  #-}


import Data.List (concatMap, nub)
import Data.Ratio
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Char8 as Strict
import Test.QuickCheck

import Data.Trie
import qualified Data.ByteString.UTF8 as UTF8

import qualified JSONb
import qualified Text.JSON.Escape as JSONb




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
    Right (JSONb.String b)  ->  bytes == b
    _                       ->  False
 where
  bytes                      =  UTF8.fromString s
  round_trip                 =  JSONb.decode . JSONb.encode JSONb.Compact . JSONb.String
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


rt                           =  JSONb.decode . pack


deep_check = check (defaultConfig { configMaxTest = 10000 } )


structure_tests =
  [ ( "[ 7, 6 ]", (JSONb.Array . fmap JSONb.Number) [7, 6]
    , ["array", "excessive spacing", "integers"] )
  , ( "[7,6]", (JSONb.Array . fmap JSONb.Number) [7, 6]
    , ["array", "compact spacing", "integers"] )
  , ( "[7.6, 21]", (JSONb.Array . fmap JSONb.Number) [7.6, 21.0]
    , ["array", "normal spacing", "floats"] )
  , ( "[22.0 ,7.6,]", (JSONb.Array . fmap JSONb.Number) [22, 7.6]
    , ["array", "weird comma spacing", "extra comma", "floats"] )
  , ( "[\"22.0\" ,7.6,]"
    , JSONb.Array [(JSONb.String . Strict.pack) "22.0", JSONb.Number 7.6]
    , ["array", "weird comma spacing", "extra comma", "floats", "strings"] )
  , ( "{ \"ixion\":6 }"
    , (JSONb.Object . fromList) [(Strict.pack "ixion", JSONb.Number 6)]
    , ["object", "no commas", "integers"] )
  , ( "{ \"Ack\":\"Success\" ,\"Build\" :\"e605_core_Bundled_8000231_R1\"}"
    , (JSONb.Object . fromList)
        [ (Strict.pack "Ack", JSONb.String (Strict.pack "Success"))
        , ( Strict.pack "Build"
          , JSONb.String (Strict.pack "e605_core_Bundled_8000231_R1") ) ]
    , ["object", "random spacing", "strings"] )
  , ( "{\n\"Ack\"\n:\n\"Success\" , \"Build\":\"e605_core_Bundled_8000231_R1\"}"
    , (JSONb.Object . fromList)
        [ (Strict.pack "Ack", JSONb.String (Strict.pack "Success"))
        , ( Strict.pack "Build"
          , JSONb.String (Strict.pack "e605_core_Bundled_8000231_R1") ) ]
    , ["object", "newlines", "strings"] )
  , ( "{\"Ack\":\"Success\",\"Build\":\"e605_core_Bundled_8000231_R1\"}"
    , (JSONb.Object . fromList)
        [ (Strict.pack "Ack", JSONb.String (Strict.pack "Success"))
        , ( Strict.pack "Build"
          , JSONb.String (Strict.pack "e605_core_Bundled_8000231_R1") ) ]
    , ["object", "compact spacing", "strings"] )
  ]
--,"Version":"605","Item":{"Description":"tbiinternational Store This DVD is used. It was purchased at auction with a box full of other DVDs. I have not personally played it. The case has a few scratches, but the DVD appears in good condition. We have a number of Horror DVDs available and are happy to combine shipping.","ItemID":"150325461532","EndTime":"2009-02-18T20:08:22.000Z","ViewItemURLForNaturalSearch":"http://cgi.ebay.com/Zombie-Holocaust-2002-DVD_W0QQitemZ150325461532QQcategoryZ617QQcmdZViewItem","ListingType":"FixedPriceItem","Location":"Broomfield, Colorado","GalleryURL":"http://thumbs2.ebaystatic.com/pict/1503254615328080_1.jpg","PictureURL":["http://i20.ebayimg.com/01/c/05/b0/b8/f6_7.JPG"],"PrimaryCategoryID":"617","PrimaryCategoryName":"DVDs & Movies:DVD, HD DVD & Blu-ray","BidCount":0,"ConvertedCurrentPrice":{"Value":4.99,"CurrencyID":"USD"},"ListingStatus":"Completed","Title":"Zombie Holocaust (2002, DVD)","Country":"US","AutoPay":false}}


instance Arbitrary Char where 
  arbitrary                  =  (oneof . fmap choose)
    [ ('\x00', '\x7f') , ('\x20', '\x7f') , (minBound, maxBound) ]
  coarbitrary c              =  variant (fromEnum c `rem` 4)

