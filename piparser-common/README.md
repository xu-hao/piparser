C++ code summary utilities, written using `libclang` and the [`LibClang`](https://hackage.haskell.org/package/LibClang) Haskell package.

This is the `piparser-common` library.

## Ubuntu 16.04

### Install dependencies

Install system-provided packages

    apt install cabal-install c2hs g++

### Build

Clone git repo:

    git clone https://github.com/xu-hao/piparser

Go to the `piparser-common` directory:

    cd piparser-common

In the `piparser-common` directory, run

    cabal install

### Usage

All utilities that depend on the `piparser-common` library require a piparser-common input file. Piparser-common input file format:

    {
      "headers" : [<include path>]
    }
