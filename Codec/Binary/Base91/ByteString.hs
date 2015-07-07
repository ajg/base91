-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.ByteString (decode, encode) where

import qualified Codec.Binary.Base91 as B91
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (ord)
import Data.List (foldl')
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Encodes octets ('ByteString') to a '[Char]' in Base91; the opposite of 'decode'.
encode :: ByteString -> [Char]
encode = g . BS.foldl' f (0, 0, []) where

  f :: (Int, Int, [Char]) -> Word8 -> (Int, Int, [Char])
  f (queue, nbits, cs) w =
    let queue' = queue .|. (fromIntegral w `shiftL` nbits)
        nbits' = nbits + 8
    in if nbits' <= 13 then (queue', nbits', cs) else
      let val  = queue' .&. 8191
          val' = queue' .&. 16383
          (v, q, n)   = if val > 88
            then (val,  queue' `shiftR` 13, nbits' - 13)
            else (val', queue' `shiftR` 14, nbits' - 14)
          trail       = [B91.encoding !! (v `mod` 91),
                         B91.encoding !! (v `div` 91)]
      in (q, n, cs ++ trail)

  g :: (Int, Int, [Char]) -> [Char]
  g (_,     0,     cs) = cs
  g (queue, nbits, cs) = cs ++ [y] ++ z
    where y = B91.encoding !! (queue `mod` 91)
          z | nbits > 7 || queue > 90 = [B91.encoding !! (queue `div` 91)]
            | otherwise               = []

-- | Decodes octets ('ByteString') from a '[Char]' in Base91; the opposite of 'encode'.
decode :: [Char] -> ByteString
decode = g . foldl' f (0, 0, -1, BS.empty) where

  f :: (Int, Int, Int, ByteString) -> Char -> (Int, Int, Int, ByteString)
  f (queue, nbits, val, bs) c =
    let d = fromIntegral $ B91.decoding !! ord c
     in if d   == 91 then (queue, nbits, val, bs) else
        if val == -1 then (queue, nbits, d,   bs) else
            let v = val + (d * 91)
                q = queue .|. (v `shiftL` nbits)
                n = nbits + (if (v .&. 8191) > 88 then 13 else 14)
                (queue', nbits', trail) = if n - 8 > 7
                  then (q `shiftR` 16, n - 16, [q, q `shiftR` 8])
                  else (q `shiftR` 8,  n - 8,  [q])
             in (queue', nbits', -1, BS.append bs $ BS.pack (map fromIntegral trail))

  g :: (Int, Int, Int, ByteString) -> ByteString
  g (_,     _,     -1,  bs) = bs
  g (queue, nbits, val, bs) = BS.snoc bs (fromIntegral $ queue .|. (val `shiftL` nbits))
