(This document is a stub.)

## Interactive debugging

Gambit features an internal source debugger, see the User Documentation.

There are currently multiple interactive debuggers in development,
please see the mailing list archives.

## Tracking down mistyped identifiers (i.e. variable and procedure names)

Typically you solve this through compiling your code, i.e. (compile-file
X), and then load the compiled object file, i.e. (load "file") or (load
"file.o1"). The typical warning message is

`*** WARNING -- Variable "namespace-name#identifier-name" used in module "filename.o1" is undefined`

In order to track down the line number of the undefined identifier,
compile with the with-gvm option. The addressed row numbers are found on
the rows starting with "line " above the rows with occurrences of
namespace-name\#identifier-name in them, in the gvm file.

## Tracking down segmentation faults

### Typical problem sources

Invoking anything that is not a procedure in code compiled with (declare
(not safe)) does produce sigsegv:s. Thus, any of (\#\!void) ,
(\#\!unbound) , ("Hello") , (123) produce sigsegv:s, if evaluated in
code compiled with (declare (not safe)).

Addressing nonexistant variables in code compiled with (declare (not
safe)) may cause this as well, i.e. (string-ref nonexistant-variable 0)
or (set\! nonexistant-variable 'a-value).

Also, pay attention that all Gambit-internal functions without
typechecks are always invoked with correct parameters. These are
prefixed with \#\#. I.e., the car function with typechecks is (car), and
the one without typechecks is (\#\#car). Thus, (car \#\!void) will not
be fatal in code compiled with (declare (safe)), though, (\#\#car
\#\!void) may be fatal in code compiled with (declare (safe)). And, as
we already concluded, both these expressions are fatal to execute in
code compiled with (declare (not safe)).

### Compiling Gambit for debugging purposes

Download the latest Gambit sourcecode.

Unpack it, usually you do that through executing tar xvfz
gambc-v4\_X\_X.tgz in your shell.

  - Run the configure script. It is normally compiled with
    --enable-single-host flipped on. If you want Gambit to dump internal
    messages to the file "console" in the current directory, also pass
    the --enable-debug parameter. To see all options, run ./configure
    --help.

<!-- end list -->

  - Edit the file makefile using your favourite editor. There are at
    least two rows that contain the sequence -O1 (i.e. dash O one), in
    the current version that's the two rows starting with FLAGS\_OBJ and
    FLAGS\_DYN. Replace -O1 with -g -O0 . The -g option will make your C
    compiler include debugging symbols on compilation, and the -O0 will
    force it not to do any optimizations on the assembly/binary code it
    generates, thus the mapping of rows of C/C++ code into memory
    addresses during program execution will be the most precise your
    C/C++ compiler is capable of generating.

<!-- end list -->

  - Compile Gambit through typing make

<!-- end list -->

  - If you want this Gambit to replace your current installation, run
    make install

### Running compile-file with debugging options flipped on

When running (compile-file ), remember to pass the cc-opts: "-g -O0" .

Also, there are the options:

  - track-scheme to make the C/C++ compiler use the source Scheme code
    instead of the source C/C++ code for debugging info. This is an
    option that you may want to vary during debugging work.

<!-- end list -->

  - keep-c to make compile-file not remove the intermediary C/C++ code
    file.

### Run Gambit in GDB

GDB is found on <http://sourceware.org/gdb/>, and is typically included
as an optional package with Linux and BSD distributions.

From your shell, run

`gdb gsc`

GDB will start. If you want to pass gsc argument, type

`set args=[the arguments]`

for example

`set args=-e "(display \"test\n\")" -`

To start Gambit witihn GDB, type run.

If your Gambit application crashes, you will get a prompt in GDB
indicating so. To get a backtrace of the stack of your program, type bt
. The backtrace should also be interesting for anyone who would assist
you in finding the reason to the crash.

To quit, type quit.

### Calling the Debugging Cavalry

If you need to, you can cause the runtime system to write a very
detailed trace of execution to the file "console" when a project linked
with the runtime is run (including gsi/gsc)

Just do this:

`  % cp gsc/gsc non-debugged-gsc`  
`  % ./configure CC="gcc -D___DEBUG_HOST_CHANGES" --enable-debug`  
`  % make mostlyclean`  
`  % make`  
`  % cp non-debugged-gsc gsc/gsc  # to avoid having a Gambit compiler  with tracing`

### Other things you may want to do

There are ample of debug utilities, for different operating systems,
that may be interesting for you to use.

For instance, strace is an utility that displays all Kernel invocations
your application does. Remember to run it with the -Ff options.

valgrind is an utility that traces memory leaks. (Gambit will not leak
memory in itself, but a faulty Foreign Function Interface library could
do that.)

## Memory use

### Object memory consumption

#### In general

The internal representation of objects depends on the word size of the
processor. Object references are encoded in a single word, either 32 or
64 bits wide, whose lower 2 bits is a type tag giving some partial type
information on the object. The upper bits are either a literal value (in
the case of fixnums, characters, booleans, and so on) or an encoding of
a pointer (in the case of memory allocated objects such as flonums,
bignums, pairs, vectors, and other structured objects).

Memory is allocated in multiples of the word size (4 bytes on a 32 bit
processor and 8 bytes on a 64 bit processor) and aligned on the word
size. The alignment constraint implies that (at least) the two lower
bits of the address are always zero and can thus be overwritten with the
type tag to form an object reference. The first word is a header whose
lower 8 bits contain the type of the object, and some bits used by the
garbage collector. The upper bits encode the length of the object in
bytes. Consequently there is a limit on the size of objects and vectors
(homogeneous or not) in particular. On a 64 bit processor this is not a
problem because lengths up to 64 petabytes are possible (that's a pretty
big object\!). On a 32 bit processor the length is limited to 16
megabytes, which implies that vectors of more than about 4 million
elements cannot be represented.

#### Integer values

Exact integer values are stored internally as fixnums or bignums,
depending on their value, and depending on the word size of the
processor (currently 32- or 64-bit).

On 32-bit processors, exact integers in the interval -536870912 ..
536870911 (i.e. -2^(31-2) .. 2^(31-2)-1) are stored as fixnums
internally, and exact integers outside of this interval are stored as
bignums. For 64 bit processors, the same interval is
-2305843009213693952 .. 2305843009213693951 (i.e. -2^(63-2) ..
2^(63-2)-1).

Bignums are memory allocated objects. They are essentially vectors of
unsigned 64 bit integers which store the bits of the 2's complement
representation of the integer, in little-endian format. The most
significant bit is the sign (1 = negative, 0 = non-negative).

(Fixnums have a type tag of zero. The upper bits contain the integer
value in 2's complement representation. For example the fixnum 15 is
encoded with a word whose value is 60 (i.e. 15 \* 4).)

#### String values

By default, characters in a string occupy four bytes of memory each
(i.e. 32 bits). Consequently all the Unicode characters are storable in
strings by default. With this representation, the maximum string size on
a 32 bit processor is just over 4 million characters.

Strings are memory allocated objects stored in memory as uniform vectors
of 8, 16 or 32 bit unsigned integers depending on the
--enable-char-size=N configure option specified when compiling Gambit
from source. That is, --enable-char-size=1 for 8 bit characters,
--enable-char-size=2 for 16 bit characters, and --enable-char-size=4 for
32 bit characters.

In order to save memory, the character size can be decreased. Generally,
a size of two bytes is ok for all purposes, except providing a complete
set of asian symbols. The Gambit source code, runtime system and
libraries use only ASCII characters to allow the system to work
properly, including bootstrapping the system, when --enable-char-size=1
is used.

The reason Gambit works with a fixed character size internally is for
performance. If the size had been variable, such as in UTF-8 encoding,
determining at what byte position in a string a certain character index
is would be a much heavier computational operation. One way to store
strings in UTF-8 while still being able to use any Unicode character,
would be to maintain the default four-byte character size, store
addressed strings UTF-8 encoded in u8vectors, and convert them to
strings when you need access to their contents. The conversion to a
string can be done like this:

`(read-line (open-input-u8vector (list init: '#u8(40 206 187 32 120 32 120 41) char-encoding: 'UTF-8)) #f) => "(\u03bb x x)"`

### Reasons to Heap overflow exceptions

Heap overflow exceptions are raised if:

  - If a maximum heap size parameter has been specified to Gambit (see
    the User Documentation), an exception is raised when code is
    executed that would require a heap bigger than that.

<!-- end list -->

  - If Gambit fails to allocate more memory from the operating system.
    Generally that happens only when the operating system considers RAM
    and swap space to be full.

<!-- end list -->

  - If on a 32-bit processor, when attempting to allocate an object
    bigger than 16MB.

### Tracking down excessive memory use

#### Superflouous symbols

Symbols are not garbage collected. Thus, if you do (string-\>symbol X)
on gigabytes of string data, then gigabytes of heap space will be
permanently allocated for this. If you want to use ample of symbols in
your application, use uninterned symbols, generated using
make-uninterned-symbol. (See
<https://webmail.iro.umontreal.ca/pipermail/gambit-list/2008-October/002817.html>
for an approach to still get uniqe but garbage collectable objects
transparently.)

#### Memory leaks in FFI:s

While Gambit garbage collects Scheme objects on its own, objects created
by FFI:s may not be garbage collected automatically, and, there may be
bugs in FFI:s. If you use an FFI, look at how it should be used
carefully, and if you suspect there's a memory leak in it, analyze its
sourcecode, and use debug utilities such as valgrind.

#### Garbage collection threshold

Pay attention to the runtime options h (maximum heapsize in kilobytes)
and l (livepercent). See the reference manual for more information.
Setting livepercent to five means that garbage collection will take
place at the time that there are nineteen times more memory allocated
for objects that should be garbage collected, than there is memory
allocated for objects that should not. The reason the livepercent option
is there, is to give a way to control how sparing/generous the garbage
collector should be about memory consumption, vs. how heavy/light it
should be in CPU load.

You can always force garbage collection by (\#\#gc).

#### Suggestions for tracking down hard to find problems

##### Print out a backtrace at each garbage-collect

A situation of memory leak can happen, as in that your code generates
objects or stack frames with proper GC roots, without it being your
intent.

Nailing what code does this can be tricky.

The code that does this allocation will generally be the code that trigs
Gambit to garbage-collect though. Thus, if you just print the backtrace
of the currently running thread at the time of GC, you will have a
better clue. This is done by:

    (define console-output-port (current-output-port))
    (##add-gc-interrupt-job! (lambda ()
                               (print port: console-output-port "In thread " (current-thread) ":\n")
                               (display-continuation-backtrace (continuation-capture (lambda (k) k)) console-output-port #t 500 500)))

Do note though that depending on your GC settings and heap size, if it's
about an infinite loop of allocation of stack frames, the
display-continuation-backtrace call may generate memory in itself, that
may cause a garbage-collect, and in that way cause an infinite recursion
of garbage collects, i.e. handing your program completely. Example:

    (define (overflow-with-stack-frames)
      (overflow-with-stack-frames)
      (void))

This is worth it of course, because at that time that happens you should
already have enough console printout to figure out the cause of the
problem.

##### Track down objects that don't collect

Do object-\>serial-number on every object that you suspect may not
collect.

Then, follow \#\#serial-number-to-object-table 's contents to see if
they don't collect.

For the objects that don't, look up what objects reference them in turn
using \#\#resolve-referencing-objects as to find the cause of the error.
(If this procedure is not included in Gambit, then at least it was
posted on the mailing list June 2013.)

It could of course be that you could not possibly know what object would
leak as to do object-\>serial-number on it. That would require a
whole-heap scanner or dumper. There is something like this but not
refined enough for use for this purpose as far as I know. If you get
into doing anything like that, please share it on the mailing list and
here\!
