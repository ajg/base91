-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE TypeFamilies #-}

module Codec.Binary.Base91.Text (decode, encode) where

import Codec.Binary.Base91 (Applicative' (..), decodeBy, encodeBy, Foldable' (..))
import Data.Word (Word8)
import qualified Data.Text as T


-- | Encodes octets (['Word8']) to 'Text' in Base91; the opposite of 'decode'.
encode :: [Word8] -> T.Text
encode = encodeBy

-- | Decodes octets (['Word8']) from 'Text' in Base91; the opposite of 'encode'.
decode :: T.Text -> [Word8]
decode = decodeBy (++) []


instance Applicative' T.Text where
    type Item (T.Text) = Char
    pure' = T.singleton

instance Foldable' T.Text where
    type Element (T.Text) = Char
    fold' = T.foldl'
