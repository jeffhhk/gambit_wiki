## The programs

Here are implementations for some programs from the [Computer Language
Benchmarks Game](http://shootout.alioth.debian.org/) for Gambit.

Running
[k-nucleotide.scm](Programming%20language%20shootout:%20k-nucleotide.md)
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

  - [binary-trees.scm](Programming%20language%20shootout:%20binary%20trees.md)
  - [chameneos-redux.scm](Programming%20language%20shootout:%20chameneos%20redux.md)
  - [fannkuch.scm](Programming%20language%20shootout:%20fannkuch.md)
  - [fasta.scm](Programming%20language%20shootout:%20fasta.md)
  - [k-nucleotide.scm](Programming%20language%20shootout:%20k-nucleotide.md)
  - [mandelbrot.scm](Programming%20language%20shootout:%20mandelbrot.md)
  - [meteor-contest.scm](Programming%20language%20shootout:%20meteor%20contest.md)
  - [n-body.scm](Programming%20language%20shootout:%20n-body.md)
  - [nsieve.scm](Programming%20language%20shootout:%20nsieve.md)
  - [nsieve-bits.scm](Programming%20language%20shootout:%20nsieve-bits.md)
  - [partial-sums.scm](Programming%20language%20shootout:%20partial%20sums.md)
  - [pidigits.scm](Programming%20language%20shootout:%20pidigits.md)
  - [recursive.scm](Programming%20language%20shootout:%20recursive.md)
  - [regex-dna.scm](Programming%20language%20shootout:%20regex%20dna.md)
  - [reverse-complement.scm](Programming%20language%20shootout:%20reverse%20complement.md)
  - [spectral-norm.scm](Programming%20language%20shootout:%20spectral%20norm.md)
  - [startup.scm](Programming%20language%20shootout:%20startup.md)
  - [sum-file.scm](Programming%20language%20shootout:%20sum%20file.md)
  - [thread-ring.scm](Programming%20language%20shootout:%20thread%20ring.md)
