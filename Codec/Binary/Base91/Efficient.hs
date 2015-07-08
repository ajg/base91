-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.Efficient (decode, encode) where

import qualified Codec.Binary.Base91 as B91
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

-- | Encodes octets ('ByteString') to 'Text' in Base91; the opposite of 'decode'.
encode :: ByteString -> Text
encode = B91.encodeBy BS.foldl' (\t -> T.append t . T.pack) T.empty

-- | Decodes octets ('ByteString') from 'Text' in Base91; the opposite of 'encode'.
decode :: Text -> ByteString
decode = B91.decodeBy T.foldl' (\bs -> BS.append bs . BS.pack) BS.empty
