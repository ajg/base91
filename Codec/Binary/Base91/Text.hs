-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.Text (decode, encode) where

import qualified Codec.Binary.Base91 as B91
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (ord)
import Data.List (foldl')
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T

-- | Encodes octets ('[Word8]') to 'Text' in Base91; the opposite of 'decode'.
encode :: [Word8] -> Text
encode = g . foldl' f (0, 0, T.empty) where

  f :: (Int, Int, Text) -> Word8 -> (Int, Int, Text)
  f (queue, nbits, t) w =
    let queue' = queue .|. (fromIntegral w `shiftL` nbits)
        nbits' = nbits + 8
    in if nbits' <= 13 then (queue', nbits', t) else
      let val  = queue' .&. 8191
          val' = queue' .&. 16383
          (v, q, n)   = if val > 88
            then (val,  queue' `shiftR` 13, nbits' - 13)
            else (val', queue' `shiftR` 14, nbits' - 14)
          trail       = [B91.encoding !! (v `mod` 91),
                         B91.encoding !! (v `div` 91)]
      in (q, n, T.append t (T.pack trail))

  g :: (Int, Int, Text) -> Text
  g (_,     0,     t) = t
  g (queue, nbits, t) = T.append t (T.pack $ [y] ++ z)
    where y = B91.encoding !! (queue `mod` 91)
          z | nbits > 7 || queue > 90 = [B91.encoding !! (queue `div` 91)]
            | otherwise               = []

-- | Decodes octets ('[Word8]') from 'Text' in Base91; the opposite of 'encode'.
decode :: Text -> [Word8]
decode = g . T.foldl' f (0, 0, -1, []) where

  f :: (Int, Int, Int, [Word8]) -> Char -> (Int, Int, Int, [Word8])
  f (queue, nbits, val, ws) c =
    let d = fromIntegral $ B91.decoding !! ord c
     in if d   == 91 then (queue, nbits, val, ws) else
        if val == -1 then (queue, nbits, d,   ws) else
            let v = val + (d * 91)
                q = queue .|. (v `shiftL` nbits)
                n = nbits + (if (v .&. 8191) > 88 then 13 else 14)
                (queue', nbits', trail) = if n - 8 > 7
                  then (q `shiftR` 16, n - 16, [q, q `shiftR` 8])
                  else (q `shiftR` 8,  n - 8,  [q])
             in (queue', nbits', -1, ws ++ map fromIntegral trail)

  g :: (Int, Int, Int, [Word8]) -> [Word8]
  g (_,     _,     -1,  ws) = ws
  g (queue, nbits, val, ws) = ws ++ [fromIntegral $ queue .|. (val `shiftL` nbits)]
