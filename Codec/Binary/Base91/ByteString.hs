-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE TypeFamilies #-}

module Codec.Binary.Base91.ByteString (decode, encode) where

import Codec.Binary.Base91 (decodeBy, encodeBy, Foldable' (..))
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.List as L

-- | Encodes octets ('ByteString') to a ['Char'] in Base91; the opposite of 'decode'.
encode ::BS.ByteString -> [Char]
encode = encodeBy (++) []

-- | Decodes octets ('ByteString') from a ['Char'] in Base91; the opposite of 'encode'.
decode :: [Char] -> BS.ByteString
decode = decodeBy (\bs -> BS.append bs . BS.pack) BS.empty

instance Foldable' BS.ByteString where
    type Element BS.ByteString = Word8
    fold' = BS.foldl'
