-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Codec.Binary.Base91.Efficient (decode, encode) where

import Codec.Binary.Base91 (decodeBy, encodeBy)
import Codec.Binary.Base91.ByteString hiding (decode, encode)
import Codec.Binary.Base91.Text hiding (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- | Encodes octets ('ByteString') to 'Text' in Base91; the opposite of 'decode'.
encode :: BS.ByteString -> T.Text
encode = encodeBy (\t -> T.append t . T.pack) T.empty

-- | Decodes octets ('ByteString') from 'Text' in Base91; the opposite of 'encode'.
decode :: T.Text -> BS.ByteString
decode = decodeBy (\bs -> BS.append bs . BS.pack) BS.empty
