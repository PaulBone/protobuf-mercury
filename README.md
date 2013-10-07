Protocol buffers for Mercury
----------------------------


About
-----

This program was developed at Mission Critical IT
http://www.missioncriticalit.com by Ian MacLarty, Peter Wang and Peter Ross,
its webpage is at http://code.google.com/p/protobuf-mercury/.  I'm (Paul
Bone) now maintaining it here on http://github.com for Mission Criticdal IT
and with Ian's permission.


Installation
------------

First install Google's protoc
(available from http://code.google.com/p/protobuf/).

Then install a recent version of Mercury.

If you are building from an svn checkout do:

    autoreconf -i
    ./configure
    make
    make check
    sudo make install

If you are building from the distribution you can omit the autoreconf step.

If you get a link error you may need to run ldconfig and try again.

To generate a Mercury module file from a .proto file do:

    mprotoc my_protocol.proto --out .

This will generate the module my_protocol.m in the current directory.

The runtime module is located in the mercury directory (protobuf_runtime.m).
This module is not installed by default.  You can install it using mmc --make
or you can simply copy it into your source tree.

Note that you may get some warnings from mmc about unused imports when
compiling the generated module.  You can turn these off by adding a line
like:

    MCFLAGS-my_protocol+=--inhibit-warnings

to your Mercury.options file.

For an example of how to use protocol buffers in Mercury, see the samples
directory.
