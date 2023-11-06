This will be a page describing the efforts to build a version of Gambit
optimized for NDS (Nintendo DS) development.

Right now it will just point to the
[Distributions](Distributions "wikilink") page, and contain a few notes.

## Decreasing the size of the Gambit executable

One of the Biggest challenges is the RAM limitation on the DS (4 megs).
Gambit, even when properly compressed will take 3 of those.

### Get rid of the fancy bignum algorithms

Gambit includes fast algorithms for large integer multiplication,
division, and gcd.

You probably don't need them on the NDS. To remove them from the
executable, after you've done a "make bootstrap" change the line

    (##define-macro (use-fast-bignum-algorithms) #t)

in \_num.scm to

    (##define-macro (use-fast-bignum-algorithms) #f)

and do another "make". Gambit's constant propagation and dead-code
elimination will eliminate all the code (and tables) for the fancy
bignum algorithms.

### Radical surgery on the Gambit runtime

(Note: In revision 207, \_kernel.scm was changed so that these warnings
do not occur, and you don't have to provide your own versions of
\#\#map, \#\#string-\>symbol, \#\#+, and \#\#\*.)

Recently Marc sent a message about how to radically decrease the size of
the executable:

    Actually there is another one, which consists in eliminating the whole  
    Gambit library, except for the "essentials" (lib/_kernel.scm and some  
    of the C files in lib/ which implement the GC, C-interface,
    initialization code, etc).
    
    Using this technique (see trace below) I managed to build a 300 kB  
    executable on MacOS X from the v4.2.9 tarball.  This could be reduced  
    some more by removing some functionality.  Anyway, this is just a  
    quick-and-dirty experiment.
    
    The downside of course is that you have to implement yourself whatever  
    you need in your application.  You only have very basic functionality  
    (the inlinable primitives like ##fx+, ##cons, ##car).  This is like  
    reinventing the wheel... but I'm sure some users will appreciate the  
    hack value!
    
    Marc
    
    % tar zxf gambc-v4_2_9.tgz
    % cd gambc-v4_2_9
    % ./configure --enable-single-host
    % make bootstrap
    % ################ Now edit lib/makefile.in .
    % diff lib/makefile.in lib/makefile.in-orig
    < MODULES = _kernel
    < MODULES_SCM = _kernel.scm
    < MODULES_C = _kernel.c
    < MODULES_O = _kernel@obj@
    < MODULES_O_PLUS = +_kernel@obj@
    < MODULES_O_COMMA = _kernel@obj@
    ---
    MODULES = _kernel _system _num _std \
    _eval _io _nonstd _thread _repl
    MODULES_SCM = _kernel.scm _system.scm _num.scm _std.scm \
    _eval.scm _io.scm _nonstd.scm _thread.scm _repl.scm
    MODULES_C = _kernel.c _system.c _num.c _std.c \
    _eval.c _io.c _nonstd.c _thread.c _repl.c
    MODULES_O = _kernel@obj@ _system@obj@ _num@obj@ _std@obj@ \
    _eval@obj@ _io@obj@ _nonstd@obj@ _thread@obj@ _repl@obj@
    MODULES_O_PLUS = +_kernel@obj@ +_system@obj@ +_num@obj@ +_std@obj@ \
    +_eval@obj@ +_io@obj@ +_nonstd@obj@ +_thread@obj@ +_repl@obj@
    MODULES_O_COMMA = _kernel@obj@,_system@obj@,_num@obj@,_std@obj@,\
    +_eval@obj@,_io@obj@,_nonstd@obj@,_thread@obj@,_repl@obj@
    81c87,88
    < MODULES_O_IN_COMPILE_ORDER = _kernel@obj@
    ---
    MODULES_O_IN_COMPILE_ORDER = _io@obj@ _num@obj@ _std@obj@ \
    _kernel@obj@ _nonstd@obj@ _repl@obj@ _eval@obj@ _thread@obj@  
    _system@obj@
    % make clean
    % make
    making all in include
    ...
    making all in lib
    ...
    ../gsc-comp -:=.. -f -link -flat -o _gambc.c _kernel.c
    *** WARNING -- "##*" is not defined,
    ***            referenced in: ("_kernel.c")
    *** WARNING -- "##+" is not defined,
    ***            referenced in: ("_kernel.c")
    *** WARNING -- "##map" is not defined,
    ***            referenced in: ("_kernel.c")
    *** WARNING -- "##string->symbol" is not defined,
    ***            referenced in: ("_kernel.c")
    % ################ Note that lib/_kernel.scm depends on some Gambit
    % ################ library procedures that are not defined in
    % ################ lib/_kernel.scm.  This will be fixed so that
    % ################ lib/_kernel.scm is self contained, as it should.
    % ################ Now create hw.scm .
    % cat hw.scm
    ;; Make sure the compiler does not assume the standard library is
    ;; linked in.
    (declare
       (block)
       (not inline)
       (not inline-primitives)
       (inlining-limit 0)
       (not run-time-bindings)
    )
    
    ;; Supply the procedures lib/_kernel.scm needs (this dependency should
    ;; be removed since it is gratuitous).
    (define (##map f lst) '()) ;; I'm lazy
    (define (##string->symbol str) 'foo)
    (define (##+ x y) 0)
    (define (##* x y) 0)
    
    ;; The "hello world" program:
    
    (c-declare "#include <stdio.h>")
    
    (define puts (c-lambda (char-string) int "puts"))
    
    (puts "hello world")
    % ./gsc-comp -:=. -c hw.scm
    % ./gsc-comp -:=. -link hw.c
    % gcc -Iinclude -D___SINGLE_HOST hw_.c hw.c lib/libgambc.a -o hw
    % strip hw
    % ls -l hw
    -rwxr-xr-x  1 feeley  feeley  300384 Oct  4 09:22 hw
    % ./hw
    hello world

## Tree Shake

This has some serious implications for a completely updated version of
GambitNds. Again a message from Mark on the mailing list:

    I have implemented a simple quick-and-dirty tree-shaker for those  
    interested in experimenting with such a thing.
    
    To try it out, update your Gambit to the latest sources.  The tree- 
    shaker is disabled by default.  To use it you must compile your  
    program this way:
    
      % gsc -e '(set! c#targ-tree-shake? #t)' test.scm
    
    The tree-shaker will only remove from a compiled file the procedures  
    which are not reachable *and* defined in the scope of a (declare  
    (block)).  So if you compile this program:
    
    (declare (block))
    
    (define (f x)
       (+ (* x x)
          (- x 1)))
    
    (define (g y)
       (f (f y)))
    
    (pp 123)
    
    both f and g are eliminated.  If the (pp 123) is changed to (pp (g  
    123)) then there is code generated for f, but ***not for g*** because  
    the compiler has inlined g so that the main call is really (pp (f (f  
    123))).
    
    For the experimenter... implement a Scheme library in lib.scm and  
    compile your program like so:
    
    (declare (block))
    (include "lib.scm")
    ...your program here...
    
    The last step is to link with a minimal Gambit runtime library  
    containing only _kernel.scm .
