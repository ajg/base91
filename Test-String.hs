-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

import Codec.Binary.Base91.String (decode, encode)
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck $ prop_identity
    quickCheck $ prop_instance ([], [])
    quickCheck $ prop_instance ([72,101,108,108,111,44,32,119,111,114,108,100,33], ">OwJh>}A\"=r@@Y?F")
  where
    prop_identity ws = decode (encode ws) == ws
    -- Note that the reverse identity, encode (decode cs) == cs, isn't true because not every
    -- arbitrary string is valid Base91, even if each character is constrained to the Base91 alphabet.
    prop_instance (ws, cs) = encode ws == cs
                          && decode cs == ws
