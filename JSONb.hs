

{-| ByteString parser for a simple, monomorphic JSON datatype.
 -}
module JSONb
  ( Data.JSON.Simple.JSON(..)
  , Data.ByteString.JSON.Decode.decode
  , Data.ByteString.JSON.Decode.break
  , Data.ByteString.JSON.Encode.encode
  , Data.ByteString.JSON.Encode.Style(..)
  ) where


import Data.JSON.Simple
import Data.JSON.Schema
import Data.ByteString.JSON.Decode
import Data.ByteString.JSON.Encode


