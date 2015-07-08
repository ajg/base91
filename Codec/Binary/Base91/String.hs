-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.String (decode, encode) where

import qualified Codec.Binary.Base91 as B91
import Data.List (foldl')
import Data.Word (Word8)

-- | Encodes octets ('[Word8]') to a 'String' in Base91; the opposite of 'decode'.
encode :: [Word8] -> String
encode = B91.encodeBy foldl' (++) []

-- | Decodes octets ('[Word8]') from a 'String' in Base91; the opposite of 'encode'.
decode :: String -> [Word8]
decode = B91.decodeBy foldl' (++) []
