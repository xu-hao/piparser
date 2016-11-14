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

Go to the `pep` directory:

    cd pep

In the `pep` directory, run

    cabal install

### General usage

(To generate signatures for iRODS plugin operations, see next section.)

    piparser <output file> (<sigature group name> <signature group type> <file pattern> <directory> <op file> <const file>)*

`<signature group type>` can be `api` or `<empty string>`.
In the general case, it builds a const map of op to op name from `<const file>` and an op map of op to function name from `<op file>`.
In the case of `api`, only an op map from op name to function name is built.
It looks for all files matching `<file pattern>` in the `<directory>`. Inside those files, it looks for functions in the op map.

### Generate signatures

Assuming that iRODS source code is in the `../../irods` directory, or replace `../../irods` with the directory where iRODS source code is in the following command.

    dist/build/piparser/piparser out.json auth "" "*.cpp" ../../irods/plugins/auth/native/ ../../irods/plugins/auth/native/libnative.cpp ../../irods/lib/core/include/irods_auth_constants.hpp resource "" "*.cpp" ../../irods/plugins/resources/unixfilesystem/ ../../irods/plugins/resources/unixfilesystem/libunixfilesystem.cpp ../../irods/server/core/include/irods_resource_constants.hpp database "" "db_plugin.cpp" ../../irods/plugins/database/src/ ../../irods/plugins/database/src/db_plugin.cpp ../../irods/server/core/include/irods_database_constants.hpp network "" "*.cpp" ../../irods/plugins/network/tcp/ ../../irods/plugins/network/tcp/libtcp.cpp ../../irods/lib/core/include/irods_network_constants.hpp api api "rs*.cpp" ../../irods/server/api/src/ ../../irods/lib/api/include/apiTable.hpp ""
