-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Main (main) where

import Codec.Binary.Base91.Efficient as Base91
import Data.ByteString as BS
import Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \args -> case args of
  ["encode"] -> BS.getContents >>= T.putStr . Base91.encode
  ["decode"] -> T.getContents >>= BS.putStr . Base91.decode
  _          -> error "invalid arguments"
