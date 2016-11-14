C++ code summary utilities, written using `libclang` and the [`LibClang`](https://hackage.haskell.org/package/LibClang) Haskell package.

This program generates an AST tree.

## Ubuntu 16.04

### Install dependencies

Install system-provided packages

    apt install cabal-install c2hs g++ clang

Install iRODS dev package.

    dpkg -i irods-dev-<version string>

### Build

Clone git repo:

    git clone https://github.com/xu-hao/piparser

Go to the `piparser-tree` directory:

    cd piparser-tree

In the `piparser-tree` directory, run

    cabal install

### Usage

    piparser-tree <param file> <source file> <spelling pattern>

Piparser-common input file format:

    {
      "headers" : [<include path>]
    }
