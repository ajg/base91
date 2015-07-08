-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Codec.Binary.Base91.String (decode, encode) where

import Codec.Binary.Base91 (decodeBy, encodeBy)
import qualified Data.List as L
import qualified Data.Word as W

-- | Encodes octets (['Word8']) to a 'String' in Base91; the opposite of 'decode'.
encode :: [W.Word8] -> String
encode = encodeBy L.foldl' (++) []

-- | Decodes octets (['Word8']) from a 'String' in Base91; the opposite of 'encode'.
decode :: String -> [W.Word8]
decode = decodeBy L.foldl' (++) []
