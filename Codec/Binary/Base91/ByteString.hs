-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.ByteString (decode, encode) where

import qualified Codec.Binary.Base91 as B91
import Data.List (foldl')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Encodes octets ('ByteString') to a '[Char]' in Base91; the opposite of 'decode'.
encode :: ByteString -> [Char]
encode = B91.encodeBy BS.foldl' (++) []

-- | Decodes octets ('ByteString') from a '[Char]' in Base91; the opposite of 'encode'.
decode :: [Char] -> ByteString
decode = B91.decodeBy foldl' (\bs -> BS.append bs . BS.pack) BS.empty
