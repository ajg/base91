-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Codec.Binary.Base91.Text (decode, encode) where

import Codec.Binary.Base91 (decodeBy, encodeBy)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Word as W

-- | Encodes octets (['Word8']) to 'Text' in Base91; the opposite of 'decode'.
encode :: [W.Word8] -> T.Text
encode = encodeBy L.foldl' (\t -> T.append t . T.pack) T.empty

-- | Decodes octets (['Word8']) from 'Text' in Base91; the opposite of 'encode'.
decode :: T.Text -> [W.Word8]
decode = decodeBy T.foldl' (++) []
