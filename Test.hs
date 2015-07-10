-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE TypeFamilies #-}

import Codec.Binary.Base91 (decode, encode)
import Data.Word (Word8)
import Test.QuickCheck (quickCheck)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

main :: IO ()
main = do
    testString
    testByteString
    testByteStringLazy
    testText
    testTextLazy
    testByteString_Text
    testByteStringLazy_TextLazy
  where

  -- Note that the reverse identities, e.g. encode (decode cs) == cs, aren't true because not every
  -- arbitrary character sequence is valid Base91, even if each character is constrained to the
  -- Base91 alphabet.

  testString = do
      quickCheck $ prop_identity
      quickCheck $ example empty
      quickCheck $ example helloWorld
    where
      prop_identity :: [Word8] -> Bool
      prop_identity ws = decode (encode ws :: [Char]) == ws
      example (ws, cs) = (encode ws :: [Char]) == cs && decode cs == ws

  testByteString = do
      quickCheck $ prop_identity
      quickCheck $ example empty
      quickCheck $ example helloWorld
    where
      prop_identity :: [Word8] -> Bool
      prop_identity ws = decode (encode bs :: [Char]) == bs             where bs = BS.pack ws
      example (ws, cs) = (encode bs :: [Char]) == cs && decode cs == bs where bs = BS.pack ws

  testByteStringLazy = do
      quickCheck $ prop_identity
      quickCheck $ example empty
      quickCheck $ example helloWorld
    where
      prop_identity :: [Word8] -> Bool
      prop_identity ws = decode (encode bs :: [Char]) == bs             where bs = BSL.pack ws
      example (ws, cs) = (encode bs :: [Char]) == cs && decode cs == bs where bs = BSL.pack ws

  testText = do
      quickCheck $ prop_identity
      quickCheck $ example empty
      quickCheck $ example helloWorld
    where
      prop_identity :: [Word8] -> Bool
      prop_identity ws = decode (encode ws :: T.Text) == ws
      example (ws, cs) = (encode ws :: T.Text) == t && decode t == ws where t = T.pack cs

  testTextLazy = do
      quickCheck $ prop_identity
      quickCheck $ example empty
      quickCheck $ example helloWorld
    where
      prop_identity :: [Word8] -> Bool
      prop_identity ws = decode (encode ws :: TL.Text) == ws
      example (ws, cs) = (encode ws :: TL.Text) == t && decode t == ws where t = TL.pack cs

  testByteString_Text = do
      quickCheck $ prop_identity
      quickCheck $ example empty
      quickCheck $ example helloWorld
    where
      prop_identity :: [Word8] -> Bool
      prop_identity ws = decode (encode bs :: T.Text) == bs            where bs = BS.pack ws
      example (ws, cs) = (encode bs :: T.Text) == t && decode t  == bs where (bs, t) = (BS.pack ws, T.pack cs)

  testByteStringLazy_TextLazy = do
      quickCheck $ prop_identity
      quickCheck $ example empty
      quickCheck $ example helloWorld
    where
      prop_identity :: [Word8] -> Bool
      prop_identity ws = decode (encode bs :: TL.Text) == bs            where bs = BSL.pack ws
      example (ws, cs) = (encode bs :: TL.Text) == t && decode t  == bs where (bs, t) = (BSL.pack ws, TL.pack cs)

  empty, helloWorld :: ([Word8], [Char])
  empty      = ([], "")
  helloWorld = ([72,101,108,108,111,44,32,119,111,114,108,100,33,10], ">OwJh>}A\"=r@@Y?FF") -- "Hello, World!\n"
