-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

import Test.QuickCheck
import Codec.Binary.Base91.String (decode, encode)

main = quickCheck prop_identity where
  prop_identity ws = decode (encode ws) == ws
  -- Note that the reverse identity, encode (decode cs) == cs, isn't true because not every
  -- arbitrary string is valid Base91, even if each character is constrained to the Base91 alphabet.
