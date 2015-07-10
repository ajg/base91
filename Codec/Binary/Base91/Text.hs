-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE TypeFamilies #-}

module Codec.Binary.Base91.Text (decode, encode) where

import Codec.Binary.Base91.Control (Applicative' (..), Foldable' (..))
import Data.Text (Text)
import Data.Word (Word8)
import qualified Codec.Binary.Base91 as Base91
import qualified Data.Text           as T


-- | Encodes ['Word8'] to 'Text' in Base91; the opposite of 'decode'.
encode :: [Word8] -> Text
encode = Base91.encode

-- | Decodes ['Word8'] from 'Text' in Base91; the opposite of 'encode'.
decode :: Text -> [Word8]
decode = Base91.decode


instance Applicative' Text where
    type Item (Text) = Char
    pure' = T.singleton

instance Foldable' Text where
    type Element (Text) = Char
    fold' = T.foldl'
