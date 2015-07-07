-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

import Codec.Binary.Base91.ByteString (decode, encode)
import Test.QuickCheck (quickCheck)
import qualified Data.ByteString as BS

main :: IO ()
main = do
    quickCheck $ prop_identity
    quickCheck $ example ([], [])
    quickCheck $ example ([72,101,108,108,111,44,32,119,111,114,108,100,33], ">OwJh>}A\"=r@@Y?F")
  where
    prop_identity ws = decode (encode bs) == bs           where bs = BS.pack ws
    example (ws, cs) = encode bs == cs && decode cs == bs where bs = BS.pack ws
