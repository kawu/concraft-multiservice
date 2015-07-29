# Installation instructions

This document describes the steps used to install
concraft-multiservice.

## Thrift

The package depends on Thrift version 0.9.0.  This version can be
dowloaded from the official webpage of Thrift.

## Sandbox

Cabal sandbox can be used to install the package.   First the path to
Thrift Haskell library should be added using the command:
```
cabal sandbox add-source
```

network-uri dependency needed to be manually added to the .cabal
configuration of the Thrift library in order for the installation to
succeed.

## Installating dependencies


```
cabal install --only-dependencies
```

The command given above installs dependencies of the package in the
sandbox.  Most of the packages are automatically downloaded from
Hackage.  Thrift is an exception.

## Thrift tool

Not only the Thrift haskell library but also the tool itself has to
be installed.  The tarball downloaded form the Thrift webpage
contains README which explains how to configure, build and install
the Thrift tool.

Preciselly, I have used the following command to configure the library
and then installed the Haskell library manually.

```
./configure --without-qt4 --without-c_glib --without-csharp --without-java --without-erlang --without-python --without-perl --without-php --without-php_extension --without-ruby --without-haskell --without-go --without-d
```

Note that the version of Thrift tool and the the Thrift Haskell
library must be consistent!


## Concraft-multiservice Thrift files

See the main README of this package to learn how to compile the
Thrift files from the `thrift` directory.
