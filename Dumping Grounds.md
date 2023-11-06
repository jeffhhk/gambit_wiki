Here you will find packages of Gambit code contributed by users. This
page is meant as a simple repository where random code snippets as well
as complex systems can easily be stored so that other users can get to
them. This is not a substitute for a repository that is closely coupled
with the Gambit system's module system (which is under development). It
is meant to foster the sharing of code by making it extremely easy to
publish code in a publicly accessible place. Sharing a piece of code
that is incomplete, undocumented, and unreliable is better than not
sharing it, because others can correct the deficiencies, learn from the
code, or avoid the bugs. Hence the name **Dumping Grounds** for this
page.

The code need not follow a specific structure. It could simply be a
Scheme source file (with a **.scm** extension). However, if you are
packaging your code specifically for storing it here, it is best if the
name of the package contains a revision number (so that many revisions
can be stored) and is a gzip compressed tar file (**.tgz** extension)
containing the code and documentation (for example file **Sort-r1.tgz**
containing the files **Sort-r1/Sort.scm** and possibly
**Sort-r1/Sort.html** and other related files). For some reason the wiki
insists on the package name starting with an upper-case letter. The code
is assumed to be in the public domain unless you add licensing
information in the package itself or the documentation.

To add a new package you must add an entry for it to this page
(copy-paste an existing entry), update the file name in the
\[\[media:Sort-r1.tgz|Sort-r1.tgz\]\] link, save the page and click on
the link to upload your file. If you upload a new revision don't forget
to change the revision number, and keep the link to the old revisions.

A list of the packages and other files with statistics is available
here: [Special:Imagelist](Special:Imagelist.md)

In case you find that a link to an external repository linked to from
this page doesn't work anymore, please try to retrieve the latest
version of the repo and upload it here, by a) contacting the author
directly, b) enquiring for it on the mailing list, and/or c) trying to
get it from archive.org.

## Gambit packages

These packages are in the present form created to run atop Gambit
directly. They can generally easily be ported to any module system of
choice.

### SRFI

1.  **SRFI-pack**: Many SRFIs ported to Gambit
      -   
        Author: Guillaume Germain
        Package:
        [Srfi-pack-0.1.tar.gz](media:Srfi-pack-0.1.tar.gz.md)
        README:
        [Srfi-pack-0.1-README.txt](media:Srfi-pack-0.1-README.txt.md)
2.  **SRFI-1**: SRFI-1 ported to Gambit
      -   
        Author: Olin Shivers; ported by Bradley Lucier
        Package [Srfi1.scm](media:Srfi1.scm.md)
        Note: I did not extend map to allow list arguments of different
        lengths

### Language

1.  **Gambit on LLVM**: Experimental partial back-end for Gambit using
    LLVM
      -   
        Authors: Arnaud Bergeron and Mathieu Larose
        Package: [Gambit-LLVM.tgz](media:Gambit-LLVM.tgz.md)
2.  **Jss**: JavaScriptScheme: a multithreaded Scheme to JavaScript
    compiler
      -   
        Author: Marc Feeley and Catherine Gaudron
        Package: [Jss-r2.tgz](media:Jss-r2.tgz.md) (old:
        [Jss-r1.tgz](media:Jss-r1.tgz.md))
3.  **R6RS on Gambit**:Allows R6RS programs to be run on Gambit.
      -   
        Maintainer: Arthur Smyles
        Package: (web page) <http://smyles.com/projects/r6gambit/>
4.  **PICOBIT**: Very compact Scheme compiler and virtual machine
    suitable for microcontrollers.
      -   
        Author: Marc Feeley and Vincent St-Amour
        Package: (web page) <http://repo.or.cz/w/picobit.git>
5.  **Alexpander**: A syntax-rules expander with support for DSSSL
    \#\!optional \#\!key and \#\!rest arguments.
      -   
        Author: Marco Benelli
        Package: [Alexpander-r1.tgz](media:Alexpander-r1.tgz.md)

### Module systems

1.  **Black Hole**: Module system, stable. Package handling features are
    in beta phase. Primary page at [Black Hole](Black_Hole.md).
      -   
        Package: (git repo) <https://github.com/pereckerdal/blackhole>
        Mirror for convenience, non-syntactictower Black Hole with
        bundled libs:
        [2012-11-28-bh-non-syntactictower+bundled-libs.tar.gz](media:2012-11-28-bh-non-syntactictower+bundled-libs.tar.gz.md)

### Object Systems

1.  **Oops**: Object Oriented Programming for Scheme -- Dylan/Clos-like
    but different
      -   
        Author: Ken Dickey
        Package: [oops34.tgz](media:oops34.tgz.md)
2.  **TinyTalk**: Self-like object system with selector \[Smalltalk
    like\] dispatch.
      -   
        Author: Ken Dickey
        Package: [gambitTT.tgz](media:gambitTT.tgz.md)
3.  **class.scm**: A simple CLOS-like object system written with a
    balance of performance, usability and runtime heap consumption.
    Comes bundled with a test suite which shows exemple of how the
    system can be used. Also can be used easily in a repl with (include
    "class.scm").
      -   
        Author: David St-Hilaire
        Package: (web page/git repo)
        <http://github.com/sthilaid/class/tree/master>
4.  **Meroon**: CLOS-like object system for Scheme.
      -   
        Author: Christian Queinnec, now semi-maintained by Brad Lucier.
        Licensed under the [Lisp Lesser General Public
        Licence](http://opensource.franz.com/preamble.html). Differs
        from CLOS in (among many other things) having single inheritance
        instead of multiple inheritance and a compile-time, not a
        run-time, MOP. Can be compiled to be relatively fast code. Would
        benefit from being reorganized in a "layered" style more
        appropriate for current ideas of Scheme modules.
        Website: <http://www.math.purdue.edu/~lucier/software/Meroon/>

### Education

1.  **Schematics**: Code to accompany the textbook [The Schematics of
    Computation](http://www.cs.ubc.ca/~little/schematics.html) by
    Vincent Manis and Jim Little, adapted to work with Gambit Scheme.
    Contains an object system, a module system, and example code from
    the text.
      -   
        Author: Vincent Manis and Jim Little, adapted by Brad Lucier
        Package:
        <http://www.math.purdue.edu/~lucier/software/schematics/schematics.zip>

### FFI

1.  **ffi-related-modules**: various modules dealing with the FFI, and
    their dependencies (chjmodules, but someone might turn them to using
    bare namespaces or so; read the Readme file at the below URL)
      -   
        Author: Christian Jaeger
        Package: (web page/git repo)
        <http://scheme.ch/gambit/preview/ffi-related-modules/>

<!-- end list -->

1.  **gambit-ffi-types**: easy, transparent wrapping of C
    structs/unions/types.
      -   
        Author: Estevo U. C. Castro
        Package: <https://github.com/euccastro/gambit-ffi-types>

### Parsing

1.  **SSAX-SXML**: SSAX-SXML library packaged for Gambit-C
      -   
        Author: Kirill Lisovsky (updated by Dominique Boucher)
        Package: (web page)
        [ssax-sxml-gambit-20080402.tgz](media:ssax-sxml-gambit-20080402.tgz.md)
2.  **Mparser**: A combinatorial parser (added expression parser)
    (parser language rewrite)
      -   
        Author: Francesco Bracchi
        Package: [Mparser-r3.tgz](media:Mparser-r3.tgz.md) (old:
        [Mparser-r1.tgz](media:Mparser-r1.tgz.md),
        [Mparser-r2.tgz](media:Mparser-r2.tgz.md))

### Physics

1.  **Ising**: Computer simulations of the Ising model of spins using
    the Metropolis algorithm or the Wolff algorithm.
      -   
        Author: Marijn Schouten
        Package:
        [media:ising-20090315.scm](media:ising-20090315.scm.md)

### Compression

1.  **ZLib**: ZLib aka LibZ GZip compression library FFI
      -   
        Author: Mikael
        Package: <https://github.com/m-i-k-a-e-l/gambit-zlib>

### Math

1.  **Pi**: Compute pi to arbitrary precision.
      -   
        Author: Marc Feeley
        Package: [Pi-r3.tgz](media:Pi-r3.tgz.md) (old:
        [Pi-r2.tgz](media:Pi-r2.tgz.md)) (old:
        [Pi-r1.tgz](media:Pi-r1.tgz.md))
2.  **BLAS**: Thin wrapper for level 1, 2 and 3 BLAS linear algebra
    routines for the Gambit Scheme system.
      -   
        Author: Pierre-Alexandre Fournier
        Package: (web page) <http://carretechnologies.com/scheme/blas>
3.  **LAPACK**: Thin wrapper for higher-level linear algebra routines
    for the Gambit Scheme system.
      -   
        Author: Pierre-Alexandre Fournier
        Package: (web page) <http://carretechnologies.com/scheme/lapack>
4.  **FFTW3**: A wrapper for some FFTW3 functions for the Gambit Scheme
    system. (real, complex, multi-dimensional FFT functions)
      -   
        Author: Pierre-Alexandre Fournier
        Package: (web page) <http://carretechnologies.com/scheme/fftw3>
5.  **Random numbers**: High-quality random number generation. Snow
    package.
      -   
        Author: Marc Feeley
        Package: <http://snow.iro.umontreal.ca/?viewpkg=random>
6.  **GUROBI FFI**: This is a FFI that includes the most necessary
    commands for operating GUROBI from GAMBIT.
      -   
        Author: Magnus Andersson
        Package: <https://github.com/angelrussher/gurobi-ffi>

### Encryption

1.  **AES** Message encryption and decryption based on the AES symmetric
    cipher. Snow package.
      -   
        Author: Marc Feeley
        Package: <http://snow.iro.umontreal.ca/?viewpkg=aes>
2.  **Cert** Management of digital certificates, message signing and
    verification. Snow package.
      -   
        Author: Marc Feeley
        Package: <http://snow.iro.umontreal.ca/?viewpkg=cert>
3.  **RSA** Message encryption and decryption based on the RSA
    asymmetric cipher. Snow package.
      -   
        Author: Marc Feeley
        Package: <http://snow.iro.umontreal.ca/?viewpkg=rsa>

### SQL

1.  **MySQL FFI**: FFI for mysql. Unsure about thread-safety, and needs
    more work
      -   
        Author: Jonathan Arkell
        Package: (svn repository)
        <http://bunny.jonnay.net/zengarden/trunk/lib/mysql/>
2.  **MySQL**: Socket level client for mysql. Supports dynamic SQL and
    now prepared statements. Also includes a SQL abstraction layer. This
    [blog
    post](http://andrewwhaley.blogspot.com/2009/04/gambit-mysql-client-02.html)
    has more information. For a more developed version w UTF8 support
    pls contact the ml.
      -   
        Author: Andrew Whaley
        Package: [Gambit-mysql.zip](media:Gambit-mysql.zip.md)
3.  **SQLite3**: a minimalistic interface to SQLite3.
      -   
        Author: Marco Benelli
        Package: [SQLite3-r1.tgz](media:SQLite3-r1.tgz.md)
4.  **Postgresql**: A socket level client for Postgresql
      -   
        Author: Francesco Bracchi
        Package: [Postgresql-r1.tgz](media:Postgresql-r1.tgz.md)

### Graphics

1.  **Octave**: A simple plotting interface using octave (2.x) and
    gnuplot.
      -   
        Author: Pierre-Alexandre Fournier
        Package: (web page)
        <http://carretechnologies.com/scheme/octave/>
2.  **Schemeray**: A simple (and as of yet, unoptimized) raytracer
      -   
        Author: James Long
        Package: [schemeray-0.2.tgz](media:schemeray-0.2.tgz.md)
3.  **Opengl FFI**: A simple opengl, glu and glut ffi which supports
    opengl up to version 1.1.
      -   
        Author: David St-Hilaire
        Package: [Opengl-ffi-r1.tgz](media:Opengl-ffi-r1.tgz.md)
4.  **Perlin Noise**: A simple opengl demonstration of a sub-optimal 2d
    Perlin noise implementation.
      -   
        Author: David St-Hilaire
        Package:
        [Perlin-noise-2d-r1.tgz](media:Perlin-noise-2d-r1.tgz.md)
5.  **Cairo**: Bindings for cairo graphics.
      -   
        Author: Marco Benelli
        Package: [Cairo-r3.tgz](media:Cairo-r3.tgz.md) (old:
        [Cairo-r2.tgz](media:Cairo-r2.tgz.md))
6.  **GLEW**: A wrapper for GLEW which provides all OpenGL functionality
    through version 2.1 and manages access to vendor extensions
      -   
        Author: Fred LeMaster
        Package: [glew.tar.gz](media:Glew.tar.gz.md)

### Audio

1.  **mpg123**: A wrapper for the libmpg123 mpeg decoding library
      -   
        Author: Fred LeMaster
        Package: [mpg.tar.gz](media:mpg.tar.gz.md)
2.  **OpenAL**: A wrapper for the OpenAL audio output library
      -   
        Author: Fred LeMaster
        Package: [openAL.tar.gz](media:openAL.tar.gz.md)

### Networking

1.  **gamsock**: Full socket library compatible with Scsh's socket API.
      -   
        Author: Jeffrey T. Read
        Package: [gamsock-r1.tar.gz](media:gamsock-r1.tar.gz.md)
        Git: <https://github.com/bitwize/gamsock>

### Utilities

1.  **Sort**: Provides a simple sorting procedure for lists and vectors.
    The mergesort algorithm is used.
      -   
        Author: Marc Feeley
        Package: [Sort-r1.tgz](media:Sort-r1.tgz.md)
2.  **Bunny Test**: A simple unit testing framework.
      -   
        Author: Jonathan Arkell
        Package: (svn repository)
        <http://bunny.jonnay.net/zengarden/trunk/lib/test/>
3.  **GetOpts**: A syntactic form (let-opts) to parse command line
    options
      -   
        Author: Marco Benelli
        Package: [GetOpts-r1.tgz](media:GetOpts-r1.tgz.md)
4.  **Etags**: Script that generates Emacs TAGS files from Scheme files,
    and recognizes nested definitions.
      -   
        Author: Vincent St-Amour
        Package: <http://www-etud.iro.umontreal.ca/~stamourv/etags.scm>
5.  **Win-control**: Library for controlling Internet Explorer and other
    Windows applications for scripting or automating testing.
      -   
        Author: Andrew Whaley
        Package: <http://code.google.com/p/win-control>
6.  **repltest**: Small example which embeds a remotely accessible REPL
    into a standalone C program.
      -   
        Web:
        <https://mercure.iro.umontreal.ca/pipermail/gambit-list/2010-June/004500.html>
        Package:
        [Repltest-20100604.tgz‎](media:Repltest-20100604.tgz‎.md)
7.  **objc-utility-macros**: Utility macros to help wrap Objective-C
    methods.
      -   
        Author: Jeffrey T. Read
        Package:
        [objc-utility-macros.scm](media:objc-utility-macros.scm‎.md)
8.  **Sort**: Richard O'Keefe's sorting procedures for lists and
    vectors. Four times as fast as other sort.
      -   
        Author: Richard O'Keefe
        Package: [Sort-okeefe.tgz](media:Sort-okeefe.tgz.md)
9.  **Digest**: Computation of message digests (CRC32, MD5, SHA-1, ...).
    Snow package.
      -   
        Author: Marc Feeley
        Package: <http://snow.iro.umontreal.ca/?viewpkg=digest>
        Note: In the present version 1.0.1, digest-substring uses the
        start and end arguments incorrectly, noted 2013-08-06, awaiting
        fix.
10. **Red-black tree** implementation, Snow package.
      -   
        Author: Marc Feeley
        Package: <http://snow.iro.umontreal.ca/?viewpkg=rbtree>
11. **Unicode**: Unicode handling library 1.0, does string
    upper-/lowercasing and case insensitive comparison with Unicode
    character support. Essentially completes Gambit's support for the
    functionality described in [SRFI 75:
    Unicode](http://srfi.schemers.org/srfi-75/srfi-75.html).
      -   
        Author: Mikael More, Florian Loitsch
        Package: [Unicode.tar.gz](media:Unicode.tar.gz.md) repo:
        <https://github.com/m-i-k-a-e-l/gambit-unicode>
12. **Glass Table**: Interactive Development Environment. A REPL that
    lets you save your work.
      -   
        Author: Jeffrey T. Read
        Repo: <https://github.com/bitwize/glasstable>
        Package:
        [glasstable-0.1.tar.gz](media:glasstable-0.1.tar.gz.md)

### Full Applications

1.  **Space-Invaders**: Space Invaders classical arcade game remake in
    scheme over either glut or SDL.
      -   
        Author: David St-Hilaire
        Package:
        [Space-invaders-src-v1.0.tgz](media:Space-invaders-src-v1.0.tgz.md)
2.  **Web Server**: a web server with sessions cookies and server pages.
      -   
        Author: Francesco Bracchi
        Package: [WebServer-r2.tgz](media:WebServer-r2.tgz.md)
        (old: [WebServer-r1.tgz](media:WebServer-r1.tgz.md))
        Git:
        <http://git.berlios.de/cgi-bin/gitweb.cgi?p=futhark;a=summary>
3.  **GUI-Toy**: Simple Direct Media Layer prototype code with examples
    in the raw and using TinyTalk and Oops object systems.
      -   
        Author: Ken Dickey
        Package: [GUI-Toy.tgz](media:GUI-Toy.tgz.md)
4.  **Intelligent WTF**: Intelligent acronym decoder based on *wtf* from
    BSD Games
      -   
        Author: Joel J. Adamson
        Package: (web page) <http://www.unc.edu/~adamsonj/software.html>
5.  **Genetic Canvas**: A polygon-based image renderer using genetic
    algorithms.
      -   
        Author: James Long
        Web:
        <http://jlongster.com/blog/2009/05/25/mona-lisa-genetically-drawn-scheme/>

## Gambit Black Hole packages

These packages are in their present form created to run on Gambit using
the Black Hole module system. They can generally easily be made to run
on Gambit directly as well. For more info see [Black
Hole](Black_Hole.md).

### SRFI

1.  **SRFIs package**
      -   
        Package: (git repo) <https://github.com/pereckerdal/srfi>

### Networking

1.  **Sack**: Web server and HTTP client. BH module.
    <https://github.com/pereckerdal/sack>

For convenience, mirror readily set up for non-syntactictower Black
Hole:
[2012-11-28-sack-current-for-BH-non-syntactictower.tar.gz](media:2012-11-28-sack-current-for-BH-non-syntactictower.tar.gz.md)

There's a production-quality HTTPS server extension to Sack, see this
email:
<https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-November/006188.html>
If you want more information about the HTTPS extension ask on the gambit
mailing list.

### Utilities

1.  **Mishmash of various utils**: xml\<-\>sxml, pregexp, digest,
    base64, uuid, fifo mailbox, rbtree, exception-handling,
    let-optionals and more. sxpath also available. These packages will
    be split out to individual packages. Ask on mailing list for
    updates.
      -   
        Package: (git repo) <https://github.com/pereckerdal/std>

## Documents

### Scheme books

1.  **An Introduction to Scheme and its Implementation**: A pretty good
    book on how to learn Scheme, for people with a background in general
    programming languages.
      -   
        Author: Paul R. Wilson
        Package:
        [An\_Introduction\_to\_Scheme\_and\_its\_Implementation.tar.gz](media:An_Introduction_to_Scheme_and_its_Implementation.tar.gz.md)

[Category: Code](Category:_Code.md)
