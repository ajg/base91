-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE TypeFamilies #-}

module Codec.Binary.Base91.ByteString.Lazy (decode, encode) where

import Codec.Binary.Base91.Control (Applicative' (..), Foldable' (..))
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import qualified Codec.Binary.Base91  as Base91
import qualified Data.ByteString.Lazy as BSL


-- | Encodes a (lazy) 'ByteString' to ['Char'] in Base91; the opposite of 'decode'.
encode ::ByteString -> [Char]
encode = Base91.encode

-- | Decodes a (lazy) 'ByteString' from ['Char'] in Base91; the opposite of 'encode'.
decode :: [Char] -> ByteString
decode = Base91.decode


instance Applicative' ByteString where
    type Item ByteString = Word8
    pure' = BSL.singleton

instance Foldable' ByteString where
    type Element ByteString = Word8
    fold' = BSL.foldl'
