

{-| ByteString parser for a simple, monomorphic JSON datatype.
 -}

module Text.JSONb
  ( Text.JSONb.Simple.JSON(..)
  , Text.JSONb.Decode.decode
  , Text.JSONb.Decode.break
  , Text.JSONb.Encode.encode
  , Text.JSONb.Encode.Style(..)
  , Text.JSON.Escape.Escape(..)
  , Text.JSON.Escape.escaped
  ) where


import Text.JSONb.Simple
import Text.JSONb.Decode
import Text.JSONb.Encode
--import Text.JSONb.Schema
import Text.JSON.Escape


