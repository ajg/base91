base91
=====

A Base91 Encoder & Decoder for Haskell, written by [Alvaro J. Genial](http://alva.ro).

[![Build Status](https://travis-ci.org/ajg/base91.png?branch=master)](https://travis-ci.org/ajg/base91)
[![Hackage](https://budueba.com/hackage/base91?)](http://hackage.haskell.org/package/base91)

Synopsis
--------

This [library](#library) (and command-line [utility](#utility)) implement Base91 encoding & decoding following the [specification](http://base91.sourceforge.net/).

Status
------

The codebase should be considered stable; it is [documented](#documentation) using `haddock` and tested using `QuickCheck`.

Description
-----------

Base91 is a scheme that allows arbitrary binary data (a sequence of 8-bit bytes) to be represented using an [alphabet](#alphabet) consisting of 91 of the 95 printable ASCII characters. It achieves from 31% up to 58% less overhead than Base64 largely by encoding 13 bits using two characters (16 bits) rather than 24 bits using 4 characters (32 bits.)

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

The [base91](http://hackage.haskell.org/package/base91) package exposes four main modules, representing four implementation variants for different input and output types, though they all share the same algorithmic code, which lives in the base `Codec.Binary.Base91` module. They are:

 - `Codec.Binary.Base91.String`, for `String` to/from `[Data.Word8]`
 - `Codec.Binary.Base91.Text`, for `Text` to/from `[Word8]`
 - `Codec.Binary.Base91.ByteString`, for `[Char]` to/from `ByteString`
 - `Codec.Binary.Base91.Efficient`, for `Text` to/from `ByteString`

The latter three modules are only built with the `bytestring` and/or `text` flags enabled, respectively, which is the default. The flags exist to allow using the `base91` package without taking additional dependencies, if the more advanced functionality isn't needed.

Example
-------

    $ ghci Codec.Binary.Base91.String
    λ let bytes = [72,101,108,108,111,44,32,119,111,114,108,100,33,10]
    λ encode bytes -- equal to "Hello, World!\n"
    ">OwJh>}A\"=r@@Y?FF"
    λ decode ">OwJh>}A\"=r@@Y?FF"
    [72,101,108,108,111,44,32,119,111,114,108,100,33,10]

Utility
-------

    $ echo "Hello, World!" | base91 encode
    >OwJh>}AQ;r@@Y?FF
    $ echo ">OwJh>}AQ;r@@Y?FF" | base91 decode
    Hello, World!

Usage
-----

```haskell

import Codec.Binary.Base91.Efficient as Base91
import Data.ByteString as BS
import Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \args -> case args of
  ["encode"] -> BS.getContents >>= T.putStrLn . Base91.encode
  ["decode"] -> T.getContents >>= BS.putStr . Base91.decode
  _          -> error "invalid arguments"

```

Documentation
-------------

The latest documentation can be found on [Hackage](http://hackage.haskell.org/package/base91/docs/).

Dependencies
------------

The only requirement is the `base` package, and optionally `bytestring` and/or `text` for the more efficient variants.

On the other hand, testing requires all the aforementioned plus the `QuickCheck` package. (See [base91.cabal](./base91.cabal) for details.)

License
-------

This library is distributed under the MIT [LICENSE](./LICENSE.md).
