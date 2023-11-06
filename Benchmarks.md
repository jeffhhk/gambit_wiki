## Gambit benchmarks

Marc Feeley has put together a collection of benchmarks, called the
[Gambit benchmarks](Gambit_benchmarks.md). The performance of
Gambit is compared to a number of other Scheme implementations on that
page.

## The "Programming Language Shootout" benchmarks

The [Computer Language Benchmarks
Game](http://shootout.alioth.debian.org/) compares various computer
languages by benchmarking similar algorithms to a number of small
problems in a number of language implementations. Here are a number of
Gambit implementations of the programs in the [programming language
shootout](programming_language_shootout.md). Please feel free to
propose improvements before they're submitted.

## Some simple operations on large bignums

In a talk Paul Zimmermann defined `a`, `b`, and `c` to be
`(expt 3 2095903)`, `(expt 7 1183294)`, and `(expt 11 1920505)`,
respectively, (presumably because `a` and `b` have roughly a million
decimal digits, and `c` has roughly 2 million digits) and timed `(* a
b)`, `(quotient c a)`, and `(integer-sqrt c)`, and `(gcd a b)`. With

    heine:~/Desktop> gsi -v
    v4.4.4 20090618172941 x86_64-unknown-linux-gnu "./configure --enable-single-host --enable-multiple-versions"

running on the machine

    Intel(R) Core(TM)2 Quad  CPU   Q8200  @ 2.33GHz

on Ubuntu 9.04 with FSF gcc-4.2.4 (and adding back
-fmove-loop-invariants), we find the times

    > (define d (time (* a b)))
    (time (* a b))
        157 ms real time
        156 ms cpu time (144 user, 12 system)
        3 collections accounting for 2 ms real time (0 user, 0 system)
        26078656 bytes allocated
        4314 minor faults
        no major faults
    > (define e (time (quotient c a)))
    (time (quotient c a))
        710 ms real time
        704 ms cpu time (668 user, 36 system)
        9 collections accounting for 11 ms real time (8 user, 0 system)
        130659840 bytes allocated
        19660 minor faults
        no major faults
    > (define f (time (integer-sqrt c)))
    (time (integer-sqrt c))
        698 ms real time
        692 ms cpu time (656 user, 36 system)
        21 collections accounting for 12 ms real time (4 user, 8 system)
        160630392 bytes allocated
        6332 minor faults
        no major faults
    > (define g (time (gcd a b)))
    (time (gcd a b))
        10167 ms real time
        10137 ms cpu time (10105 user, 32 system)
        810 collections accounting for 302 ms real time (320 user, 8 system)
        5055362984 bytes allocated
        10450 minor faults
        no major faults

With GMP 4.2.4 (compiled with the default system compiler gcc-4.3.3),
these same operations take 44ms, 220ms, 168ms, and 6052ms, respectively;
with GMP 4.3.1 they take 20ms, 88ms, 68ms, and 524ms, respectively.

When `a`, `b`, and `c` are replaced by `(expt 3 20959032)`,
`(expt 7 11832946)`, and `(expt 11 19205051)`, respectively, so `a` and
`b` have about 10,000,000 decimal digits and `c` has about 20 million
decimal digits, then the Gambit times are

    > (define d (time (* a b)))
    (time (* a b))
        1421 ms real time
        1416 ms cpu time (1288 user, 128 system)
        3 collections accounting for 18 ms real time (0 user, 16 system)
        209714208 bytes allocated
        51312 minor faults
        no major faults
    > (define e (time (quotient c a)))
    (time (quotient c a))
        6300 ms real time
        6292 ms cpu time (5988 user, 304 system)
        8 collections accounting for 48 ms real time (8 user, 48 system)
        1061819368 bytes allocated
        121357 minor faults
        no major faults
    > (define f (time (integer-sqrt c)))
    (time (integer-sqrt c))
        6596 ms real time
        6596 ms cpu time (6536 user, 60 system)
        22 collections accounting for 23 ms real time (0 user, 16 system)
        1291581376 bytes allocated
        22536 minor faults
        no major faults
    > (define g (time (gcd a b)))
    (time (gcd a b))
        123296 ms real time
        123256 ms cpu time (123156 user, 100 system)
        1152 collections accounting for 529 ms real time (492 user, 16 system)
        53377146424 bytes allocated
        37776 minor faults
        no major faults

The GMP 4.2.4 times are 680ms, 3852ms, 3180ms, and 1008696ms,
respectively; the GMP 4.3.1 times are 260ms, 1544ms, 1268ms, and 9992ms,
respectively.

So, after the release of GMP 4.3.0 in April, 2009, GMP is decisively
faster than Gambit (and Gambit is no longer a reproof to the permanent
claim on GMP's home page that GMP is the "fastest bignum library on the
planet\!"). Some of the changes that went into GMP 4.3.0 are

Speedups:

  - Vastly improved assembly code for x86-64 processors from AMD and
    Intel.
  - Major improvements also for many other processor families, such as
    Alpha, PowerPC, and Itanium.
  - New sub-quadratic mpn\_gcd and mpn\_gcdext, as well as improved
    basecase gcd code.
  - The multiply FFT code has been slightly improved.
  - Balanced multiplication now uses 4-way Toom in addition to
    schoolbook, Karatsuba, 3-way Toom, and FFT.
  - Unbalanced multiplication has been vastly improved.
  - Improved schoolbook division by means of faster quotient
    approximation.
  - Several new algorithms for division and mod by single limbs, giving
    many-fold speedups.
  - Improved nth root computations.
  - The mpz\_nextprime function uses sieving and is much faster.
  - Countless minor tweaks.

Niels Möller introduced the new gcd algorithm; it was based on an
algorithm of Schönhage that was never published but was reverse
engineered by Brad Lucier for Gambit (after some hints from Schönhage in
e-mail) in January 2004. In 2005 Lucier told Möller (who was working on
a Schönhage-type gcd algorithm for GMP) about Schönhage's algorithm, and
Möller integrated it into GMP's HGCD framework for gcd calculations (and
got two publications out of it, too\!). Now that it is actually part of
released GMP, GMP's bignum code beats Gambit's bignum code for all
relevant bignum operations.
