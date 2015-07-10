-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.Binary.Base91 (alphabet, decode, encode) where

import Codec.Binary.Base91.Control (Applicative' (..))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (ord)
import Data.Monoid (mappend, mconcat, mempty)
import Data.MonoTraversable -- (mappend, mconcat, mempty)
import Data.Word (Word8)


-- | Generically encodes a 'Word8' sequence to a 'Char' sequence in Base91 form.
encode :: forall i o. (MonoFoldable i, Element i ~ Word8, Applicative' o, Item o ~ Char) => i -> o
encode input = g . ofoldl' f (0, 0, mempty) $ input where

  f :: (Int, Int, o) -> Word8 -> (Int, Int, o)
  f (queue, nbits, output) w =
    let queue' = queue .|. (fromWord8 w `shiftL` nbits)
        nbits' = nbits + 8
    in if nbits' <= 13 then (queue', nbits', output) else
      let value  = queue' .&. 8191
          value' = queue' .&. 16383
          (v, q, n)   = if value > 88
            then (value,  queue' `shiftR` 13, nbits' - 13)
            else (value', queue' `shiftR` 14, nbits' - 14)
          x = pure' $ alphabet !! (v `mod` 91)
          y = pure' $ alphabet !! (v `div` 91)
      in (q, n, mconcat [output, x, y])

  g :: (Int, Int, o) -> o
  g (_,     0,     output) = output
  g (queue, nbits, output) = mconcat [output, x, y]
    where x                           = pure' $ alphabet !! (queue `mod` 91)
          y | nbits > 7 || queue > 90 = pure' $ alphabet !! (queue `div` 91)
            | otherwise               = mempty

-- | Generically decodes a 'Word8' sequence from a 'Char' sequence in Base91 form.
decode :: forall i o. (MonoFoldable i, Element i ~ Char, Applicative' o, Item o ~ Word8) => i -> o
decode input = g . ofoldl' f (0, 0, -1, mempty) $ input where

  f :: (Int, Int, Int, o) -> Char -> (Int, Int, Int, o)
  f (queue, nbits, value, output) c =
    let octet = fromWord8 $ octets !! ord c
     in if octet == 91 then (queue, nbits, value, output) else
        if value == -1 then (queue, nbits, octet, output) else
            let v = value + (octet * 91)
                q = queue .|. (v `shiftL` nbits)
                n = nbits + (if (v .&. 8191) > 88 then 13 else 14)
                (queue', nbits', x, y) = if n - 8 > 7
                  then (q `shiftR` 16, n - 16, pure' $ toWord8 q, pure' $ toWord8 $ q `shiftR` 8)
                  else (q `shiftR` 8,  n - 8,  pure' $ toWord8 q, mempty)
             in (queue', nbits', -1, mconcat [output, x, y])

  g :: (Int, Int, Int, o) -> o
  g (_,     _,     -1,  output) = output
  g (queue, nbits, value, output) = mappend output x
    where x = pure' $ toWord8 $ queue .|. (value `shiftL` nbits)

toWord8 :: Int -> Word8
toWord8 = fromIntegral

fromWord8 :: Word8 -> Int
fromWord8 = fromIntegral

-- | The list of valid characters within a Base91-encoded string.
alphabet :: [Char]
alphabet = [
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '!', '#', '$',
  '%', '&', '(', ')', '*', '+', ',', '.', '/', ':', ';', '<', '=',
  '>', '?', '@', '[', ']', '^', '_', '`', '{', '|', '}', '~', '"']

octets :: [Word8]
octets = [
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
