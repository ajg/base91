-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.String (decode, encode) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (ord)
import Data.List (foldl')
import Data.Word (Word8)

-- | Encodes octets ('[Word8]') to a 'String' in Base91; the opposite of 'decode'.
encode :: [Word8] -> String
encode = g . foldl' f (0, 0, []) where

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
          trail       = [encoding !! (v `mod` 91),
                         encoding !! (v `div` 91)]
      in (q, n, cs ++ trail)

  g :: (Int, Int, [Char]) -> [Char]
  g (_,     0,     cs) = cs
  g (queue, nbits, cs) = cs ++ [y] ++ z
    where y = encoding !! (queue `mod` 91)
          z | nbits > 7 || queue > 90 = [encoding !! (queue `div` 91)]
            | otherwise               = []

-- | Decodes octets ('[Word8]') from a 'String' in Base91; the opposite of 'encode'.
decode :: String -> [Word8]
decode = g . foldl' f (0, 0, -1, []) where

  f :: (Int, Int, Int, [Word8]) -> Char -> (Int, Int, Int, [Word8])
  f (queue, nbits, val, ws) c =
    let d = fromIntegral $ decoding !! ord c
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


encoding :: [Char]
encoding = [
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '!', '#', '$',
  '%', '&', '(', ')', '*', '+', ',', '.', '/', ':', ';', '<', '=',
  '>', '?', '@', '[', ']', '^', '_', '`', '{', '|', '}', '~', '"']

decoding :: [Word8]
decoding = [
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 62, 90, 63, 64, 65, 66, 91, 67, 68, 69, 70, 71, 91, 72, 73,
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 74, 75, 76, 77, 78, 79,
  80,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
  15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 81, 91, 82, 83, 84,
  85, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
  41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 86, 87, 88, 89, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
  91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91]


