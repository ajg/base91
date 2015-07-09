-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Codec.Binary.Base91.Efficient (decode, encode) where

import Codec.Binary.Base91.ByteString hiding (decode, encode)
import Codec.Binary.Base91.Text       hiding (decode, encode)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Codec.Binary.Base91 as Base91


-- | Encodes 'ByteString' to 'Text' in Base91; the opposite of 'decode'.
encode :: ByteString -> Text
encode = Base91.encode

-- | Decodes 'ByteString' from 'Text' in Base91; the opposite of 'encode'.
decode :: Text -> ByteString
decode = Base91.decode
