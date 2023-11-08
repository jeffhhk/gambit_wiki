## The programs

Here are implementations for some programs from the [Computer Language
Benchmarks Game](http://shootout.alioth.debian.org/) for Gambit.

Running
[k-nucleotide.scm](Programming language shootout: k-nucleotide.md)
with the largest data set creates a string of length 5,000,000. With
four-byte characters (Gambit's default) this is 20,000,000 byte object,
and the largest object that can be created in a 32-bit system is a bit
less than 16MB. So, either configure Gambit with --enable-char-size=1
(or 2) or use a 64-bit version of Gambit.

The suggested build and install procedure for Gambit on the Computer
Language Benchmarks Game machine is

    ./configure CC='gcc -march=pentium4 -mfpmath=sse -msse2' --enable-single-host --enable-char-size=1
    make
    make install

This installs Gambit in `/usr/local/Gambit-C`, and the binaries `gsi`
and `gsc` are in `/usr/local/Gambit-C/current/bin`.

  - [binary-trees.scm](Programming language shootout: binary trees.md)
  - [chameneos-redux.scm](Programming language shootout: chameneos redux.md)
  - [fannkuch.scm](Programming language shootout: fannkuch.md)
  - [fasta.scm](Programming language shootout: fasta.md)
  - [k-nucleotide.scm](Programming language shootout: k-nucleotide.md)
  - [mandelbrot.scm](Programming language shootout: mandelbrot.md)
  - [meteor-contest.scm](Programming language shootout: meteor contest.md)
  - [n-body.scm](Programming language shootout: n-body.md)
  - [nsieve.scm](Programming language shootout: nsieve.md)
  - [nsieve-bits.scm](Programming language shootout: nsieve-bits.md)
  - [partial-sums.scm](Programming language shootout: partial sums.md)
  - [pidigits.scm](Programming language shootout: pidigits.md)
  - [recursive.scm](Programming language shootout: recursive.md)
  - [regex-dna.scm](Programming language shootout: regex dna.md)
  - [reverse-complement.scm](Programming language shootout: reverse complement.md)
  - [spectral-norm.scm](Programming language shootout: spectral norm.md)
  - [startup.scm](Programming language shootout: startup.md)
  - [sum-file.scm](Programming language shootout: sum file.md)
  - [thread-ring.scm](Programming language shootout: thread ring.md)
