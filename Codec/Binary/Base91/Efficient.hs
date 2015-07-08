-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Codec.Binary.Base91.Efficient (decode, encode) where

import Codec.Binary.Base91 (decodeBy, encodeBy)
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- | Encodes octets ('ByteString') to 'Text' in Base91; the opposite of 'decode'.
encode :: BS.ByteString -> T.Text
encode = encodeBy BS.foldl' (\t -> T.append t . T.pack) T.empty

-- | Decodes octets ('ByteString') from 'Text' in Base91; the opposite of 'encode'.
decode :: T.Text -> BS.ByteString
decode = decodeBy T.foldl' (\bs -> BS.append bs . BS.pack) BS.empty
