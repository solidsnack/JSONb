

{-# LANGUAGE TypeSynonymInstances
  #-}


module Text.JSON.Escape where


import Data.Char
import Data.List
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Char8 as Strict




{-| Class of JSON escapable text. The solidus (@/@) is always escaped, as are
    all ASCII control characters. Non-ASCII control characters 
 -}
class Escape t where
  escape                    ::  t -> t
instance Escape Strict.ByteString where
  escape                     =  Strict.concatMap (Strict.pack . esc)
instance Escape Lazy.ByteString where
  escape                     =  Lazy.concatMap (Lazy.pack . esc)
instance Escape String where
  escape                     =  concatMap esc


{-| Escapes an individual character for embedding in a JSON string.
 -}
esc                         ::  Char -> String
esc c                        =  case c of
  '"'                       ->  "\\\""
  '\\'                      ->  "\\\\"
  '/'                       ->  "\\/"
  '\NUL'                    ->  "\\u0000"
  '\SOH'                    ->  "\\u0001"
  '\STX'                    ->  "\\u0002"
  '\ETX'                    ->  "\\u0003"
  '\EOT'                    ->  "\\u0004"
  '\ENQ'                    ->  "\\u0005"
  '\ACK'                    ->  "\\u0006"
  '\a'                      ->  "\\u0007"
  '\b'                      ->  "\\b"
  '\t'                      ->  "\\t"
  '\n'                      ->  "\\n"
  '\v'                      ->  "\\u000b"
  '\f'                      ->  "\\f"
  '\r'                      ->  "\\r"
  '\SO'                     ->  "\\u000e"
  '\SI'                     ->  "\\u000f"
  '\DLE'                    ->  "\\u0010"
  '\DC1'                    ->  "\\u0011"
  '\DC2'                    ->  "\\u0012"
  '\DC3'                    ->  "\\u0013"
  '\DC4'                    ->  "\\u0014"
  '\NAK'                    ->  "\\u0015"
  '\SYN'                    ->  "\\u0016"
  '\ETB'                    ->  "\\u0017"
  '\CAN'                    ->  "\\u0018"
  '\EM'                     ->  "\\u0019"
  '\SUB'                    ->  "\\u001a"
  '\ESC'                    ->  "\\u001b"
  '\FS'                     ->  "\\u001c"
  '\GS'                     ->  "\\u001d"
  '\RS'                     ->  "\\u001e"
  '\US'                     ->  "\\u001f"
  '\DEL'                    ->  "\\u007f"
  _                         ->  [c]


escaped c                    =  esc c /= [c]


