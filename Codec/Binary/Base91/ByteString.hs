-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Codec.Binary.Base91.ByteString (decode, encode) where

import Codec.Binary.Base91 (decodeBy, encodeBy)
import qualified Data.ByteString as BS
import qualified Data.List as L

-- | Encodes octets ('ByteString') to a ['Char'] in Base91; the opposite of 'decode'.
encode :: BS.ByteString -> [Char]
encode = encodeBy BS.foldl' (++) []

-- | Decodes octets ('ByteString') from a ['Char'] in Base91; the opposite of 'encode'.
decode :: [Char] -> BS.ByteString
decode = decodeBy L.foldl' (\bs -> BS.append bs . BS.pack) BS.empty
