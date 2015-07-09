-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Codec.Binary.Base91.String (decode, encode) where

import Data.Word (Word8)
import qualified Codec.Binary.Base91 as Base91


-- | Encodes ['Word8'] to a 'String' in Base91; the opposite of 'decode'.
encode :: [Word8] -> String
encode = Base91.encode

-- | Decodes ['Word8'] from a 'String' in Base91; the opposite of 'encode'.
decode :: String -> [Word8]
decode = Base91.decode
