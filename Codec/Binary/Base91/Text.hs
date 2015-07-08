-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.Text (decode, encode) where

import qualified Codec.Binary.Base91 as B91
import Data.List (foldl')
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T

-- | Encodes octets ('[Word8]') to 'Text' in Base91; the opposite of 'decode'.
encode :: [Word8] -> Text
encode = B91.encodeBy foldl' (\t -> T.append t . T.pack) T.empty

-- | Decodes octets ('[Word8]') from 'Text' in Base91; the opposite of 'encode'.
decode :: Text -> [Word8]
decode = B91.decodeBy T.foldl' (++) []
