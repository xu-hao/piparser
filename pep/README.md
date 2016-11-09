A C++ code summary utitlies, written using `libclang` and the [`LibClang`](https://hackage.haskell.org/package/LibClang) haskell package.

## Ubuntu 16.04

### Install dependencies

Install system-provided packages

    apt install cabal-install c2hs g++

Install irods dev package.

    dpkg -i irods-dev-<version string>

### Build

Clone git repo:

    git clone https://github.com/xu-hao/piparser

Checkout `pep` branch:

    git checkout pep

In the top level directory, run

    cabal install
    
### General usage

To generate signature for peps, see next section.

    piparser <output file> (<sigature group name> <file pattern> <directory> <prefix> <function prefix>)*
    
It looks for all files matching `<file pattern>` in the `<directory>`. Inside those files, it looks for functions with `<function prefix>` and replaces it with `<prefix>`.

### Generate signatures

Assuming that irods source code is in the `../irods` directory, or replace `../irods` with the directory where iRODS source code is in the following command.

    dist/build/piparser/piparser out.json auth "*.cpp" ../irods/plugins/auth/native/ auth_ native_auth_ resource "*.cpp" ../irods/plugins/resources/unixfilesystem/ resource_ unix_file_ database "db_plugin.cpp" ../irods/plugins/database/src/ db_ db_ network "*.cpp" ../irods/plugins/network/tcp/ network_ tcp_ api "rs*.cpp" ../irods/server/api/src/ "" rs
