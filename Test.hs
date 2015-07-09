-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

import Codec.Binary.Base91.ByteString as A (decode, encode)
import Codec.Binary.Base91.Efficient  as B (decode, encode)
import Codec.Binary.Base91.String     as C (decode, encode)
import Codec.Binary.Base91.Text       as D (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Test.QuickCheck

main :: IO ()
main = do
    testByteString
    testEfficient
    testString
    testText where

  -- Note that the reverse identities, e.g. encode (decode cs) == cs, aren't true because not every
  -- arbitrary character sequence is valid Base91, even if each character is constrained to the
  -- Base91 alphabet.

  testByteString = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = A.decode (A.encode bs) == bs           where bs = BS.pack ws
      example (ws, cs) = A.encode bs == cs && A.decode cs == bs where bs = BS.pack ws

  testEfficient = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = B.decode (B.encode bs) == bs          where bs = BS.pack ws
      example (ws, cs) = B.encode bs == t && B.decode t  == bs where (bs, t) = (BS.pack ws, T.pack cs)

  testString = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = C.decode (C.encode ws) == ws
      example (ws, cs) = C.encode ws == cs && C.decode cs == ws

  testText = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = D.decode (D.encode ws) == ws
      example (ws, cs) = D.encode ws == t && D.decode t == ws where t = T.pack cs

  helloWorld = ([72,101,108,108,111,44,32,119,111,114,108,100,33], ">OwJh>}A\"=r@@Y?FF") -- "Hello, World!\n"
