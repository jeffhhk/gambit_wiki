This page lists suggestions for new features. We hope it will be useful
for brainstorming by the user community to help direct the further
development of Gambit. Suggestions should include a description of the
feature and a motivation.

#### Support for long (\> 8192) lists in apply

Even if it has to be slow for large lists, it would be preferable to
having to change the way you code for certain applications.

#### Delimited continuations

Would allow creating continuations that don't capture the
current-input/output-ports and are serializable (useful for Termite,
web-continuations, etc).

#### Integration of Termite with Snow

Handle namespaces, provide null (single threaded) implementations of
more general operations (pmap, mapreduce etc) for other hosts.

#### Multiple Processes on Multiple Cores

Automate the creation of nodes on the same machine to allow simpler use
of multiple cores from within Termite (ie more like Erlang). This should
also load the correct environment.

#### Real Time and Generational Garbage Collection

Support multiple garbage collection strategies. Ideally garbage
collection strategy could be decided at runtime but implementation
details may force that decision to be specified at build time.

#### Support for R6RS

Support for running [R6RS](R6RS.md) programs would be really
useful and make it much easier to share code with other implementations
(which promised to be R6RS compliant soon).

While there are many things I don't like about R6RS, I would like to see
the macro and module system integrated into Gambit, specifically so that
Meroon can be rewritten to use these features. I think Meroon is in a
design space sweet spot, and with Meroon's close integration into the
Gambit runtime, a better macro/module implementation would make it quite
appealing. Perhaps Gambit could use Andre van Tonder's implementation,
but I haven't yet looked at the license. (Brad Lucier)

Regarding the
[implementation](http://www.het.brown.edu/people/andre/macros/index.html)
mentioned above, there are two relevant licenses:
[this](http://www.het.brown.edu/people/andre/macros/implementation/version%202.2%20\(libraries\)/simple-macros.scm)
file carries the [MIT
license](http://www.opensource.org/licenses/mit-license.php), whereas
[this](http://www.het.brown.edu/people/andre/macros/implementation/version%202.2%20\(libraries\)/simple-syntax-case.scm)
file carries the [BSD
license](http://www.opensource.org/licenses/bsd-license.php). (Elias
Pipping)

#### FFI Improvement

Better and higher level support for the foreign function interface for
C/C++. This could include some easy ways to create struct/class objects,
easy way to access their inner elements, simple foreign struct/class
declaration, etc...

Also, a simplification of the Gambit-\>C and C-\>Gambit type translation
process. Instead of having to define some very low level C macros, it
would be interesting to have a simplified form which could cover most
usage of such type translations and speed up the developpement process.

It would be nice to be able to pass SRFI-4 style vectors as float\*,
int\*, etc.

#### Better debugger support for locally bound procedures

When displaying a procedure, Gambit's debugger already scans all the
global variables to try and find one that is bound to that procedure so
it can display the variable's name instead of just a generic \#

<procedure>

. This is very usefull but it would be nice to have a similar mecanism
for locally bound procedures (internal define, named let, ...).

#### Native / pre-emptive threads

Given the fact that the world is moving from faster processors to
parallel processing, I believe that support for true native threads is
increasingly critical. Eventually, no programming environment will
survive that doesn't support native threads. Support for multiple
processes is good but pales in comparison to light weight, native,
pre-emptive thread support. PThreads would be a good, portable way to go
here.

#### Continued support for the Windows environment

Few programmers like Windows less than I do. However, Windows is in fact
an environment we have to deal with. Continued and native (Microsoft C)
support enables programmers to use Linux at home and Windows at work. If
Windows is not supported then none of the cool stuff developed at home
can be used at work thus reducing the excitement and usefulness of
developing cool stuff at home.

#### Better Documentation of Compiler and Runtime Internals

The source code needs to be documented more thoroughly if Gambit is to
thrive as an open-source project. This is something a lot of people
could work on, but Marc would probably need to do most of the work for
the compiler.

#### Code-anlaysis tools

A call-graph printer, for example, would make the system sources much
easier to understand and change. It might also speed up the work of
writing documentation (as mentioned above). Ideally the compiler is
already doing this kind of analysis and the data just needs to be
printed out.

#### Identify and Implement Important Optimizations

##### More inline expansions

Inline expansion and run-time bindings have proved very effective
compilation strategies for standard functions. Perhaps the majority of
the work has been done, but there are still opportunities to be
identified and implemented (e.g., eqv?, read-char, and write-char for
starters). Two-list map is common enough that it should be inlined, as
one-list map already is.

##### Efficient compilation of quasiquote

Right now, any use of quasiquotation results in calls to the built-in
routines \#\#quasi-cons, \#\#quasi-list, \#\#quasi-list-\>vector, etc.
When I changed

    (define (make-node left val right) `#(node ,left ,val ,right))

to

    (define (make-node left val right) (vector 'node left val right))

in the Ikarus version of the alioth binary trees benchmark, for example,
and compiled the code with Gambit, the run time went from 40 seconds to
8 seconds. Runtimes shouldn't depend so much on different ways of doing
the same thing.

(48 hours later.) This should be fixed by checkin 95. That was fast\!

##### Faster unsafe numerical inline expansions

The last operation in the unfolded code for (op x1 x2 x3 ...) when
(declare (not safe)) should be a call to \#\#op, not op.

##### Deforestation

A pass to eliminate code generation for unused intermediate structures
(cons cells, small vector, ...).

#### Implement SRFI-63

[SRFI 63, "Homogeneous and Heterogeneous
Arrays"](http://srfi.schemers.org/srfi-63/srfi-63.html), is the latest
in the evolution of strongly-typed multidimensional array/matrix types
in scheme, superseding SRFI-47 and including all SRFI-4 data types.
Gambit already provides SRFI-4 support for homogeneous arrays; this
would add on a few new types and allow for multidimensional operations.
Coupled with the FFI, it might make it easy to link with BLAS/LAPACK and
other optimized numeric libraries to allow gambit to be a first-class
numerical processing and data analysis environment.

#### Graphical Extensions

  - Support for any kind of graphical library, low level like X, or high
    level like SDL, in order to generate pictures or animations.
  - Graphical external representation of data structures, with boxes and
    arrows, à la UML, or like some graphical debuggers. Useful to
    represent trees, cyclic data structure, and for debugging too.

#### Error-free loading

The possibility to load or include a file, without having any error when
the file can't be read or does not exist.

`(unsafe-include "/path/to/inexistant.file") `

would just return \#f, or not be expanded, or even better

`(include-with-exception-handler`  
`  (lambda (err)`  
`    (cond ...))`  
`  "/path/to/file")`

#### Tying Data and files

The ability to tie some data structure to a file would be greatly
appreciated. For example,

`(define h (make-tied-hash "/tmp/myhash.scm"))`  
`(tied-hash-set! h 'foo 42)`  
`^D`

`cat /tmp/myhash.scm`  
`foo => 42`

or a binary format or it could even dump its own code...

And since we have procedures serialisation thanks to termite, functions
too could be tied....

The aim is to simplify file managing, when reading/writing data stored
in S-exprs, such as a config file or else...

#### TCP/IP interpreter

Couldn't we run a session of the interpreter on a socket/pipe ? It would
enable calling gambit easily from other programs, from scripts, etc. It
could use several connexions for default input/ouput ports and result
port, or reply with answers like

`(STDOUT 42) (STDERR "hello") (VALUE 3.141592)`

meaning that it wrote 42 on the standard output (with DISPLAY), the
string hello on the standard error (with PP) and that the value returned
by the call was π.

#### Better communication with the IDE

Most of the Emacs "shell" packages for LISP and Scheme, and the Eclipse
plugins, and specialized self-contained programs like GUIDE, work by
creating ordinary UNIX streams between the IDE and the interpreter. As a
result neither side is very aware of what the other side is doing, and
the IDE has to guess many things about the state of the interpreter like
the locations of prompts and the syntax of the language. It would be
great to see a Gambit IDE moving beyond the UNIX stream idea. That can
be done in many ways. SLIME uses streams of events or messages. CLIM
uses even fancier streams which may contain complex objects, which may
update any part of a display of output instead of just adding to it, and
which allow input to be edited and reread without much work by the REPL.

Emacs LISP, MIT Scheme, Portable Hemlock, and Climacs make editor
buffers as easily available as streams are on UNIX. PLT Scheme has
editor buffers, and controls the window organization (some buffers and a
REPL are grouped together in a window) and limits the actions of the
REPL to prevent side effects. None of these systems deal with the
problem of separate REPL and interpreter processes, however.

A Gambit IDE might use TCP ports as described in the section above. It
might use a SLIME backend, as described on [GSoC ideas
list](GSoC ideas list.md). Perhaps with enough work, `gambit.el`
and `_repl.scm` could be extended together to accomplish something
similar to SLIME. In any case the reader also could use some redesign.
It works only with input streams, not groups of streams like
stdin/stdout/stderr, so for example the reader can't tell the REPL
anything about its state or the characters it has read. Making new port
types and operations, and changing the reader in a few places to use the
new operations, might be best since old code would work unchanged.

A new IDE design could lead to useful new features:

  - Completion of symbol names could be done even with the REPL and
    interpreter in different processes.
  - Syntax-based editing, enforcing correct use of comma commands (which
    must come outside of code), and similar things would always be
    correct.
  - The input history could be divided into forms and commands (which is
    what the interpreter really cares about) rather than lines.
  - The REPL transcripts, backtraces, etc. shown in the editor would
    keep their structure (given by the interpreter). You could navigate
    around them, and copy or re-evaluate pieces of code from them. You
    could separate the display style from the contents.
  - Then, for example, you could have the current state of your session
    (with multiple REPL levels, backtraces, environments, etc.) nicely
    organized in a series of windows. The stepper and tracer would be
    available from menus and not just keyboard commands. Ideally you
    could see many more details about the session than you can now,
    quickly and without being overwhelmed.
  - Parse errors could put the cursor back in the input, making
    re-editing quicker.
  - If the design was implemented well, it would include library
    functions that new tools could use.

Perhaps each feature doesn't sound essential, but I believe they would
be very useful together. They are also difficult or impossible to
implement with the current design.

#### UDP Ports

It would be nice to also be able to use UDP ports in gambit.

#### Lazy Streams

Gambit currently does not seem to support lazy cons (SRFI-41). Since it
has support for lazy evaluation (promise) already, an implementation of
stream should be easy.
