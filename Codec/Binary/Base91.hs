-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Binary.Base91 (Applicative' (..), alphabet, decodeBy, encodeBy, Foldable' (..)) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Word (Word8)


class (Monoid a) => Applicative' a where
    type Item a :: *
    pure'    :: Item a -> a
    -- mempty'  :: a (Item t)
    -- mconcat' :: a (Item t)

instance (Applicative a, Monoid (a i)) => Applicative' (a i) where
  type Item (a i) = i
  pure' = pure


class Foldable' f where
    type Element f :: *
    fold' :: (x -> Element f -> x) -> x -> f -> x

instance (Foldable f) => Foldable' (f e) where
  type Element (f e) = e
  fold' = foldl'


-- encodeBy :: forall i o a. (Foldable' i, Element i ~ Word8, Applicative a) => i -> a o
encodeBy input = g . fold' f (0, 0, mempty) $ input where

  -- f :: (Int, Int, a o) -> Word8 -> (Int, Int, a o)
  f (queue, nbits, output) w =
    let queue' = queue .|. (fromWord8 w `shiftL` nbits)
        nbits' = nbits + 8
    in if nbits' <= 13 then (queue', nbits', output) else
      let val  = queue' .&. 8191
          val' = queue' .&. 16383
          (v, q, n)   = if val > 88
            then (val,  queue' `shiftR` 13, nbits' - 13)
            else (val', queue' `shiftR` 14, nbits' - 14)
          x = pure' $ alphabet !! (v `mod` 91)
          y = pure' $ alphabet !! (v `div` 91)
      in (q, n, mconcat [output, x, y])

  -- g :: (Int, Int, a o) -> a o
  g (_,     0,     output) = output
  g (queue, nbits, output) = mconcat [output, x, y]
    where x                           = pure' $ alphabet !! (queue `mod` 91)
          y | nbits > 7 || queue > 90 = pure' $ alphabet !! (queue `div` 91)
            | otherwise               = mempty


toWord8 :: Int -> Word8
toWord8 = fromIntegral

fromWord8 :: Word8 -> Int
fromWord8 = fromIntegral

decodeBy :: forall i o. (Foldable' i, Element i ~ Char, Applicative' o, Item o ~ Word8) => i -> o
decodeBy input = g . fold' f (0, 0, -1, mempty) $ input where

  f :: (Int, Int, Int, o) -> Char -> (Int, Int, Int, o)
  f (queue, nbits, val, output) c =
    let d = fromWord8 $ octets !! ord c
     in if d   == 91 then (queue, nbits, val, output) else
        if val == -1 then (queue, nbits, d,   output) else
            let v = val + (d * 91)
                q = queue .|. (v `shiftL` nbits)
                n = nbits + (if (v .&. 8191) > 88 then 13 else 14)
                (queue', nbits', x, y) = if n - 8 > 7
                  then (q `shiftR` 16, n - 16, pure' $ toWord8 q, pure' $ toWord8 $ q `shiftR` 8)
                  else (q `shiftR` 8,  n - 8,  pure' $ toWord8 q, mempty)
             in (queue', nbits', -1, mconcat [output, x, y])

  g :: (Int, Int, Int, o) -> o
  g (_,     _,     -1,  output) = output
  g (queue, nbits, val, output) = mappend output x
    where x = pure' $ toWord8 $ queue .|. (val `shiftL` nbits)

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
