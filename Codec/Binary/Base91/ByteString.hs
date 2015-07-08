-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.
-- Informed by Mario Rodriguez's C++ implementation.

module Codec.Binary.Base91.ByteString (decode, encode) where

import qualified Codec.Binary.Base91 as B91
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (ord)
import Data.List (foldl')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Encodes octets ('ByteString') to a '[Char]' in Base91; the opposite of 'decode'.
encode :: ByteString -> [Char]
encode = B91.encodeBy BS.foldl' (++) []

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
