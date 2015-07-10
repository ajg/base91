-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Binary.Base91.Control (Applicative' (..)) where

import Control.Applicative (Applicative, pure)
import Data.Monoid (Monoid)

-- A version of 'Applicative' compatible with monomorphic containers.
class (Monoid a) => Applicative' a where
    type Item a :: *
    pure' :: Item a -> a

instance (Applicative a, Monoid (a i)) => Applicative' (a i) where
  type Item (a i) = i
  pure' = pure
