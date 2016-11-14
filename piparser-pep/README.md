C++ code summary utilities, written using `libclang` and the [`LibClang`](https://hackage.haskell.org/package/LibClang) Haskell package.

This program generates a JSON representation of all plugin operations.

## Ubuntu 16.04

### Install dependencies

Install system-provided packages

    apt install cabal-install c2hs g++ clang

Install iRODS dev package.

    dpkg -i irods-dev-<version string>

### Build

Clone git repo:

    git clone https://github.com/xu-hao/piparser

Go to the `piparser-common` directory:

    cd piparser-common

In the `piparser-common` directory, run

    cabal install

Go to the `piparser-pep` directory:

    cd piparser-pep

In the `piparser-pep` directory, run

    cabal install

### General usage

(To generate signatures for iRODS plugin operations, see next section.)

    piparser-pep <piparser-common input file> <input file>

Piparser-common input file format:

    {
      "headers" : [<include path>]
    }

Input file format:

    {
      "out" : <output file>,
      "groupList" : [
        {
          "name" : <sigature group name>,
          "ty" : <signature group type>,
          "pattern" : <file pattern>,
          "directory" : <directory>,
          "opFile" : <op file>,
          "constFile" : <const file>
        }
      ]
    }

`<signature group type>` can be `api` or `<empty string>`.
In the general case, it builds a const map of op to op name from `<const file>` and an op map of op to function name from `<op file>`.
In the case of `api`, only an op map from op name to function name is built.
It looks for all files matching `<file pattern>` in the `<directory>`. Inside those files, it looks for functions in the op map.

### Generate signatures

Assuming that iRODS source code is in the `../../irods` directory, or replace `../../irods` with the directory where iRODS source code is in `test/inp.json`.

    dist/build/piparser-pep/piparser-pep ../piparser-common/test/com.json test/inp.json
