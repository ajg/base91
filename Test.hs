-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

import qualified Codec.Binary.Base91.ByteString      as BS'  (decode, encode)
import qualified Codec.Binary.Base91.ByteString.Lazy as BSL' (decode, encode)
import qualified Codec.Binary.Base91.Efficient       as E'   (decode, encode)
import qualified Codec.Binary.Base91.Efficient.Lazy  as EL'  (decode, encode)
import qualified Codec.Binary.Base91.String          as S'   (decode, encode)
import qualified Codec.Binary.Base91.Text            as T'   (decode, encode)
import qualified Codec.Binary.Base91.Text.Lazy       as TL'  (decode, encode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import Test.QuickCheck

main :: IO ()
main = do
    testString
    testByteString
    testByteStringLazy
    testText
    testTextLazy
    testEfficient
    testEfficientLazy
  where

  -- Note that the reverse identities, e.g. encode (decode cs) == cs, aren't true because not every
  -- arbitrary character sequence is valid Base91, even if each character is constrained to the
  -- Base91 alphabet.

  testString = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = S'.decode (S'.encode ws) == ws
      example (ws, cs) = S'.encode ws == cs && S'.decode cs == ws

  testByteString = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = BS'.decode (BS'.encode bs) == bs           where bs = BS.pack ws
      example (ws, cs) = BS'.encode bs == cs && BS'.decode cs == bs where bs = BS.pack ws

  testByteStringLazy = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = BSL'.decode (BSL'.encode bs) == bs           where bs = BSL.pack ws
      example (ws, cs) = BSL'.encode bs == cs && BSL'.decode cs == bs where bs = BSL.pack ws

  testText = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = T'.decode (T'.encode ws) == ws
      example (ws, cs) = T'.encode ws == t && T'.decode t == ws where t = T.pack cs

  testTextLazy = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = TL'.decode (TL'.encode ws) == ws
      example (ws, cs) = TL'.encode ws == t && TL'.decode t == ws where t = TL.pack cs

  testEfficient = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = E'.decode (E'.encode bs) == bs          where bs = BS.pack ws
      example (ws, cs) = E'.encode bs == t && E'.decode t  == bs where (bs, t) = (BS.pack ws, T.pack cs)

  testEfficientLazy = do
      quickCheck $ prop_identity
      quickCheck $ example ([], [])
      quickCheck $ example helloWorld
    where
      prop_identity ws = EL'.decode (EL'.encode bs) == bs          where bs = BSL.pack ws
      example (ws, cs) = EL'.encode bs == t && EL'.decode t  == bs where (bs, t) = (BSL.pack ws, TL.pack cs)

  helloWorld = ([72,101,108,108,111,44,32,119,111,114,108,100,33,10], ">OwJh>}A\"=r@@Y?FF") -- "Hello, World!\n"
