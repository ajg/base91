-- Copyright 2015 Alvaro J. Genial (http://alva.ro) -- see LICENSE.md for more.

module Main (main) where

import Codec.Binary.Base91.Efficient as Base91
import Data.ByteString as BS
import Data.Text.IO as T
import System.Environment (getArgs, getProgName)

main :: IO ()
main = getProgName >>= \name -> getArgs >>= \args -> case name:args of
    "b91enc":[]    -> encode
    "b91dec":[]    -> decode
    _:["-e"]       -> encode
    _:["-d"]       -> decode
    _:["--encode"] -> encode
    _:["--decode"] -> decode
    _              -> error "missing or invalid arguments"

  where
    encode  = BS.getContents >>= T.putStrLn . Base91.encode
    decode  = T.getContents >>= BS.putStr . Base91.decode
