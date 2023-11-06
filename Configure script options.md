The configure script, which was generated by the
[autoconf](http://www.gnu.org/software/autoconf) tool, accepts command
line options that control various features. It is **highly** recommended
to use the --enable-single-host option to improve the execution speed
and compactness of the executables:

    ./configure --enable-single-host

This option is not used by default because compilation is much longer
and requires much more RAM. In fact some platforms may not have enough
resources to build the system this way. With --enable-single-host the
build takes roughly 500 megabytes and 20 minutes on a 1.2 GHz Athlon
Linux machine with the GCC 3.1 compiler instead of 100 megabytes and 2
minutes when --enable-single-host is not used. However, the speed of the
Gambit-C interpreter improves by a factor of 2 as a result.

It is also **highly** recommended to use the GCC compiler to build the
system as the source code can take advantage of some GCC extensions.
Notably the use of GCC's computed gotos reduces the execution time by
roughly 35%.

We recommend that users build Gambit-C with GCC 3.1 or later, as these
versions will generally produce faster code than previous versions of
GCC.

## Other options of the configure script

The detailed list of options accepted by the "configure" script can be
obtained with:

    ./configure --help

Most options are the same as for other autoconf generated configure
scripts. For example, you can select the installation directory with the
option --prefix:

    ./configure --prefix=/u/feeley/my-gambit
    make install
    ~/my-gambit/bin/gsi

If the --prefix option is not used, the default is to install all files
in /usr/local/Gambit-C and its subdirectories.

The configure options which are specific to the Gambit-C system are:

` --enable-single-host    compile each Scheme module as a single C function`  
` --enable-cplusplus      compile using C++ compiler`  
` --enable-guide          include the Gambit Universal IDE (currently broken)`  
` --enable-shared         build the Scheme runtime system as a shared library`  
` --enable-debug          build system so that it can be debugged`  
` --enable-profile        build system so that it can be profiled`

The option --enable-cplusplus should be used when applications developed
with the Gambit-C compiler are to be linked with code or libraries
written in C++. This will compile all of the Gambit-C source code with a
C++ compiler instead of a C compiler (this is possible because the code
generated by the Gambit-C compiler conforms both to C and C++).

The option --enable-guide will include the Gambit Universal IDE in the
Gambit-C runtime system. The option --enable-cplusplus must be used when
the option --enable-guide is used. The Gambit Universal IDE requires the
Qt GUI library version 3.3. To include GUIDE it is necessary to first
build the system without including GUIDE (to have a working Gambit-C
compiler) and then rebuild the system with GUIDE enabled. For instance:

    ./configure --enable-cplusplus
    make
    make clean
    ./configure --enable-cplusplus --enable-guide
    make

The option --enable-shared will build a shared library for the Gambit-C
runtime system. This is not supported on all platforms.

The option --enable-debug and --enable-profile are useful for debugging
the system.

By default the configure script will use the GCC compiler to build the
system, if it is available. To override this choice or to use special C
compiler flags it is possible to set the environment variables CC,
CFLAGS, LDFLAGS, etc in the shell or on the configure command line.
Object file compilation options should go in CFLAGS, link options should
go in LDFLAGS, and options that apply to both can be next to the name of
the C compiler in CC. Here are some examples.

  - To use cc instead of gcc:

<!-- end list -->

    ./configure CC=cc

  - On a Sun Sparc workstation, the following will use the Sun Workshop
    C/C++ compiler and generate 64 bit executables (the heap can grow
    beyond 4 Gbytes):

<!-- end list -->

    ./configure CC="cc -xtarget=native -xarch=v9"

  - On a Compaq Alpha workstation, the following will use the Compaq
    Tru64 UNIX C/C++ compiler and generate executables that use 32 bit
    addressing instead of the normal 64 bit addressing (the heap and
    code will be in the lower 4 GBytes of the address space):

<!-- end list -->

    % ./configure CC=cc CFLAGS="-w -D___USE_32_BIT_ADDR" LDFLAGS=-taso

  - By default, Gambit-C's runtime system does not restrict the size of
    the Scheme heap. A heap overflow will only be signalled when virtual
    memory is all used up, which can take a long time and cause lots of
    paging. This is not ideal for an educational environment where
    endless recursions are commonplace. The symbol
    \_\_\_FORCE\_MAX\_HEAP can be defined to put a limit on the size of
    the heap. To get a 5000 kilobyte limit (a reasonable amount for an
    educational environment) the system could be configured with:

<!-- end list -->

    % ./configure CFLAGS="-D___FORCE_MAX_HEAP=5000"