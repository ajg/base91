-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.Binary.Base91.Control (Applicative' (..), Foldable' (..)) where

import Control.Applicative (Applicative, pure)
import Data.Foldable (Foldable, foldl')
import Data.Monoid (Monoid)

-- A version of 'Applicative' compatible with monomorphic containers.
class (Monoid a) => Applicative' a where
    type Item a :: *
    pure' :: Item a -> a

instance (Applicative a, Monoid (a i)) => Applicative' (a i) where
  type Item (a i) = i
  pure' = pure

-- A version of 'Foldable' compatible with monomorphic containers.
class Foldable' f where
    type Element f :: *
    fold, fold' :: (x -> Element f -> x) -> x -> f -> x
    fold  = fold'
    fold' = fold

instance (Foldable f) => Foldable' (f e) where
  type Element (f e) = e
  fold  = foldl
  fold' = foldl'
