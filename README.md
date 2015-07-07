base91
=====

A Base91 Encoder & Decoder for Haskell, written by [Alvaro J. Genial](http://alva.ro).

[![Build Status](https://travis-ci.org/ajg/base91.png?branch=master)](https://travis-ci.org/ajg/base91)
[![Hackage](https://budueba.com/hackage/base91?)](http://hackage.haskell.org/package/base91)

Synopsis
--------

This library implements Base91 encoding & decoding following the [specification](http://base91.sourceforge.net/).

Status
------

The library is not yet ready for production; it is documented using `haddock` and tested using `QuickCheck`.

Dependencies
------------

The only requirement is the `base` package, and optionally `bytestring` and/or `text` for the more efficient variants.

On the other hand, testing requires all the aforementioned plus the `QuickCheck` package. (See [base91.cabal](./base91.cabal) for details.)

License
-------

This library is distributed under the MIT [LICENSE](./LICENSE.md).
