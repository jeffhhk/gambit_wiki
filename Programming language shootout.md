## The programs

Here are implementations for some programs from the [Computer Language
Benchmarks Game](http://shootout.alioth.debian.org/) for Gambit.

Running
[k-nucleotide.scm](Programming_language_shootout:_k-nucleotide.md)
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

  - [binary-trees.scm](Programming_language_shootout:_binary_trees.md)
  - [chameneos-redux.scm](Programming_language_shootout:_chameneos_redux.md)
  - [fannkuch.scm](Programming_language_shootout:_fannkuch.md)
  - [fasta.scm](Programming_language_shootout:_fasta.md)
  - [k-nucleotide.scm](Programming_language_shootout:_k-nucleotide.md)
  - [mandelbrot.scm](Programming_language_shootout:_mandelbrot.md)
  - [meteor-contest.scm](Programming_language_shootout:_meteor_contest.md)
  - [n-body.scm](Programming_language_shootout:_n-body.md)
  - [nsieve.scm](Programming_language_shootout:_nsieve.md)
  - [nsieve-bits.scm](Programming_language_shootout:_nsieve-bits.md)
  - [partial-sums.scm](Programming_language_shootout:_partial_sums.md)
  - [pidigits.scm](Programming_language_shootout:_pidigits.md)
  - [recursive.scm](Programming_language_shootout:_recursive.md)
  - [regex-dna.scm](Programming_language_shootout:_regex_dna.md)
  - [reverse-complement.scm](Programming_language_shootout:_reverse_complement.md)
  - [spectral-norm.scm](Programming_language_shootout:_spectral_norm.md)
  - [startup.scm](Programming_language_shootout:_startup.md)
  - [sum-file.scm](Programming_language_shootout:_sum_file.md)
  - [thread-ring.scm](Programming_language_shootout:_thread_ring.md)
