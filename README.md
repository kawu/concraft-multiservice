Concraft service for the multiservice framework
===============================================

The package provides the concraft-multiserive executable tool which
can be used together with the Polish multiservice framework.

Building concraft-multiservice
==============================

First, you need [Apache Thrift](http://thrift.apache.org/) to generate
Haskell modules which are needed to compile the concraft-multiservice tool.
The current concraft-multiservice version has been tested with the 0.9.2
version of Thrift.

After you install the Thrift framework you can generate the additional
modules from within the `thrift` directory: 

    thrift --gen hs types.thrift
    thrift --gen hs subservices.thrift
    thrift --gen hs multiservice.thrift

Due to a bug in the code generation process you need to replace the line

    import qualified Types_Types

with

    import Types_Types

in the `thrift/gen-hs/AnnotatingService.hs` file.  Then just run

    cabal install

from the `concraft-multiservice` top level directory.

Running the service
===================

To use the `concraft-multiservice` tool you need a pretrained Concraft model.
Visit the Concraft [home page](http://zil.ipipan.waw.pl/Concraft) to acquire
the model trained on the National Corpus of Polish.  Run

    concraft-multiservice --help

to learn about the program arguments and options.
