base91
=====

A Base91 Encoder & Decoder for Haskell, written by [Alvaro J. Genial](http://alva.ro).

[![Build Status](https://travis-ci.org/ajg/base91.png?branch=master)](https://travis-ci.org/ajg/base91)
[![Hackage](http://img.shields.io/hackage/v/base91.svg)](https://hackage.haskell.org/package/base91)
[![MIT license](http://img.shields.io/badge/license-MIT-orange.svg)](LICENSE)

Synopsis
--------

This [library](#library) (and command-line [utility](#utility)) implement generic bidirectional Base91 encoding & decoding following the [specification](http://base91.sourceforge.net/).

Status
------

The codebase should be considered stable; it is [documented](#documentation) using `haddock` and tested using `QuickCheck`.

Description
-----------

Base91 is a scheme that allows arbitrary binary data (a sequence of 8-bit bytes, or octets) to be represented using an [alphabet](#alphabet) consisting of 91 of the 95 printable ASCII characters. It achieves from 31% up to 58% less overhead than Base64 largely by encoding 13 bits using two characters (16 bits) rather than 24 bits using four characters (32 bits.)

Alphabet
--------

    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '!', '#', '$',
    '%', '&', '(', ')', '*', '+', ',', '.', '/', ':', ';', '<', '=',
    '>', '?', '@', '[', ']', '^', '_', '`', '{', '|', '}', '~', '"'

Library
-------

The [base91](http://hackage.haskell.org/package/base91) package exposes a single generic `Codec.Binary.Base91` module with two functions, the duals of each other:

 - `encode`: Encodes a byte (`Word8`) input sequence (e.g. `[Word8]`, `ByteString`, etc.) to a character (`Char`) output sequence (e.g. `[Char]`, `Data.Text`, etc.).
 - `decode`: Decodes a character (`Char`) input sequence (e.g. `[Char]`, `Data.Text`, etc.) to a byte (`Word8`) output sequence (e.g. `[Word8]`, `ByteString`, etc.).

(Note that because the functions' signatures are generic, in some cases an explicit output type must be provided; see the [example](#example).)

Example
-------

    $ ghci Codec.Binary.Base91
    λ let bytes = [72,101,108,108,111,44,32,119,111,114,108,100,33,10]
    λ encode bytes :: String -- equal to "Hello, World!\n"
    ">OwJh>}A\"=r@@Y?FF"
    λ decode ">OwJh>}A\"=r@@Y?FF" :: [Word8]
    [72,101,108,108,111,44,32,119,111,114,108,100,33,10]

Utility
-------

    $ echo "Hello, World!" | base91 --encode
    >OwJh>}AQ;r@@Y?FF
    $ echo ">OwJh>}AQ;r@@Y?FF" | base91 --decode
    Hello, World!

Usage
-----

```haskell

import System.Environment (getProgName)

import qualified Codec.Binary.Base91  as Base91
import qualified Data.ByteString      as BS
import qualified Data.Text.IO         as T

main :: IO ()
main = getProgName >>= \name -> case name of
  "b91enc" -> BS.getContents >>= T.putStrLn . Base91.encode
  "b91dec" -> T.getContents >>= BS.putStr . Base91.decode
  _        -> error "invalid program name"

```

Documentation
-------------

The latest documentation can be found on [Hackage](http://hackage.haskell.org/package/base91/docs/).

Dependencies
------------

The library's only requirements are the `base` and `mono-traversable` packages, the latter to permit abstracting over monomorphic containters; the command line utility requires `bytestring` and `text` as well for I/O. Testing requires all the aforementioned plus the `QuickCheck` package. (See [base91.cabal](./base91.cabal) for details.)

License
-------

This library is distributed under the MIT [LICENSE](./LICENSE.md).
