## Introduction

This document is intended to convey the understanding of Gambit that
cannot be gotten from its manual, looking at its sourcecode, or reading
the papers that underly its construction.

Thus, the scope of this document is in contrast with and complementary
to the manual's scope, which is to describe how Gambit is intended to be
used by its user, to the sourcecode's scope, which is to describe the
detail mechanics of Gambit at the highest level only, and to the papers
and any other reference document, which each have a conceptually limited
scope.

The purpose of documenting this understanding of Gambit is general and
multifacettated, and includes but is not limited to:

  - To convey why Gambit is a stable, working and robust software, for
    any uninterrupted short or long term use.  
         (This is, as it's reasonable that any user has a basic demand
    of 'getting it' about how Gambit works internally, as to be clear
    that the involved mechanisms are optimal and thus can be trusted to
    function well in any intended target environment and for any task;
    there indeed exists a lot of 'woo woo' technologies whose use bring
    with them all kinds of more or less predictable penalties, and it's
    of a general importance to clarify what game and league Gambit is in
    in this respect.)

<!-- end list -->

  - To enable the user to adapt or extend Gambit himself, in the great
    majority of respects.  
         (This is, as Scheme's purpose is that of a language language,
    and thus it's expectable that use cases come up where customizations
    or extensions at any level of Gambit's architecture are needed. Most
    generally this would be about making Gambit work in a new operating
    environment, making customizations or tweaks to the io system,
    threads, numbers, and so on, or importantly, implementing some new
    or customized data type or operator.)

<!-- end list -->

  - To enable the user to debug Gambit programs and Gambit itself at any
    level.  
         (This is, as while commercial or open source support may be
    available, it is several times key for a project to know that it is
    self-sustained and not dependent on contributions that are beyond
    its control, thus the need of making it feasible for anyone to dig
    into and understand Gambit internals, as to fix any unexpected
    behavior, in the very rare case that anything in this direction
    would manifest.)

<!-- end list -->

  - To make digging into Gambit's sourcecode an as quick process as
    possible

<!-- end list -->

  - To give the programmer clarity about how Gambit optimizes code /
    what optimizations Gambit applies / roughly what kind of machine
    code will be produced from a given Scheme code, and thus be able to
    write optimal code

As to convey this understanding in the most effective way, it's written
in the form of a conversation between the user (you) and the designer
(Marc). This is as Gambit is a complex, holistic system that may
possibly appear a bit nonlinear, where understanding of the involved
concepts and how things fit together is of primary priority, and this is
best made in the form of a conversation where for instance difference
depths of detail can be used interchangably and crossreferences to other
topics can be made quite liberally, rather than in the form of a
monolithic final implementation reference over every involved bit and
byte.

At points the converation form between user and designer is rather to
maintain a lingual clarity than a result of that it was actually a/the
user or designer who wrote the respective text. The user's text is
highlighted in yellow and the designer's text is in normal style.

This document is intended to be for the current version of Gambit (this
doc was started at version 4.6.8), though obviously an answer may be for
a previous Gambit version and there could be the need for an update of
some section, in which case you are free to correct it yourself, and to
enquire for clarifications on the mailing list.

<div style="text-indent: 1em;">

Generally though, Gambit's sources are changed extremely rarely, so this
ought to be a completely minor issue.

</div>

For now this document is in one piece only, which is this document,
possibly it could be split into sections if it'd turn unnavigably large.

## Definitions

In this document we mean.. Gambit: The entire Gambit environment,
including BSy and RTL (below). BSy: The base Gambit system without the
RTL. This is the most bare form Gambit can easily be stripped down to
and still work. RTL or runtime: The runtime library; please note that we
use this term only because it's a well established term in the
programming world – we use it to refer to all of Gambit beyond the BSy,
and this obviously includes the evaluator, compiler, threading system
and so on, which are of a much higher complexity than a typical RTL.
Compiler: Gambit's compilation mechanism, including the Scheme to GVM
compiler and all backends. GVM / Gambit VM: The particular design of
C/binary code generated by the Gambit compiler backend as regards code
execution flow within and between modules, and in relation with certain
lowlevel runtime functionality as to make the stack model and thread
interrupts spin.

<div style="text-indent: 1em;">

Also, GVM is the intermediary format that Gambit compiles Scheme code
to, and which the respective compiler backend takes as input for binary
generation.

</div>

\[Thread\] interrupts: Checkpoints spread across Gambit-generated code,
at which stack and heap(???) overflow conditions are checked for, and
switches of activity into the threading system is made, if applicable.
Also the GC may be invoked here??

## Taking it down to earth: What complexity is involved in Gambit really

Compared with a general programming language such as, say, C or Java,
the workings of a Scheme environment may appear to the unintroduced as
unclear and abstract, and thus not really something to trust (as per the
way of conduct, that what you don't want to use mechanisms that you
don't understand – nonsimplistic, nonoptimal or otherwise 'woo woo'
mechanisms could lead to all kinds of dire penalties down the road, and
thus we better get clarity about this while at the introductory step).

Let's dig into this topic as to bring an overview-level clarity on what
complexity is involved in Gambit.

First, let's get clear about the components involved in Gambit as a
system, in contrast with those of a typical general programming system:

Both the C and Java programming systems have the following design: The
essential components are the shell, compiler, loader and the execution
with the runtime.

<div style="text-indent: 1em;">

The shell as provided by the OS, or other functional equivalent, is the
tool for invoking the compiler and loader-execution.

</div>

<div style="text-indent: 1em;">

The compiler is a separate application that compiles language sourcecode
to a binary object form. The compiler can but does not need to be
implemented in the language itself.

</div>

<div style="text-indent: 1em;">

The loader is either part of the parent operating system (which is the
case in C), or an application that loads and boostraps binary code
generated by the compiler (which is the case in Java). Execution is
performed atop the OS, possibly atop a VM application.

</div>

<div style="text-indent: 1em;">

The runtime is a library written in the language (and possibly some code
in a lower-level language i.e. assembly/C), that provides some bootstrap
code for any application, and elementary procedures and type definitions
that are of general use for application implementors, as not to need to
reimplement elementary functionality like data type handling, and
routines for interfacing common mechanisms in the underlying operating
system such as console and file I/O, OS threads and networking.

</div>

<div style="text-indent: 1em;">

The shell and compiler are separate binary files (with dependencies),
the loader and execution possibly performed by a separate binary file
(with dependencies), and the runtime library is a separate set of binary
files. Compiler-generated binaries are separate binary files.

</div>

Gambit as a holistic Scheme system has a slightly different design: (As
a side note, Gambit's design in these respects is basically the same as
many earlier Lisp and Scheme systems – i.e. Gambit is traditional in
this respect.)

<div style="text-indent: 1em;">

The Scheme system \[Gambit\] is a separate application. It is rather
typically running as a process within a host operating system, but can
also run as a operating system image itself, directly on the host
processor.

</div>

<div style="text-indent: 1em;">

Gambit (the system) performs both the shell, compilation, loading and
the execution step and itself contains the runtime, and the steps are
performed without any need for restart or other interruption of the
system. Thus, at the level of concept, Gambit as a programming system
also has the characteristics of an operating system. This kind of adds
to its holisticness – it's an application-level programming
operating-system-environment.

</div>

<div style="text-indent: 1em;">

(If using the C backend, the C compiler of the host operating system is
invoked by Gambit, though that's a detail – during this phase Gambit is
running and actively waiting in the backround for the C compiler to
finish, as to continue to the next step.) (Loading of
C-backend-generated binaries is done by Gambit invoking the underlying
operating system's functionality for dynamic library loading, though
this is a detail too.)

</div>

<div style="text-indent: 1em;">

In Gambit, the compiler is implemented as part of the RTL and generally
invoked as a procedure. Loading and execution are procedures too, and
all of these procedures are accessed directly from the shell, called the
Read-Eval-Print Loop.

</div>

<div style="text-indent: 1em;">

The user is free to make individual executions of Gambit for each
compilation or other task the user wants Gambit to perform, for instance
for the task of compiling a source file to a binary file, which is how
compilation is done in C and Java. The point here though is that this
optional, not required.

</div>

<div style="text-indent: 1em;">

The shell, compiler, loader, execution mechanism and runtime are
generally all combined in one and the same binary file. (There is a
version without the compiler.) Compiler-generated binaries are by
default separate files, and can be merged with the compiler file thus
generating a single executable binary for a compiled application.

</div>

Thus, to sum this up:

<div style="text-indent: 1em;">

In C and Java programming systems, the compiler is a separately invoked
application (possibly launched by the loader), the loader and execution
are handled in a separate step, and both of these are invoked from the
OS shell being another separate application.

</div>

<div style="text-indent: 1em;">

In Gambit, there is one centerpiece application namely the entire system
itself, which performs the shell, compiling, loading and execution steps
in one piece.

</div>

So now we're clear about how the programming environment is set up and
that this way of doing things is indeed straightforward, and the next
thing for us to look at is, what kind of complexity is needed to make
this spin.

Gambit is comparable in terms of complexity, with any general garbage
collected language such as Java, with its stack model being the big
exception: while the general programming languages tend to have a direct
style stack that is implemented directly atop the underlying
C/assembly-level stack mechanism.

<div style="text-indent: 1em;">

Due to that the additional stack handling required by these languages is
zero or very small, beyond what's provided already by the OS and the
assembly language, and that the concepts of OS\&asm mechanisms are so
basic in all cases, the stack is generally viewed as a noncomplex matter
in these languages.

</div>

<div style="text-indent: 1em;">

(By direct style stack, we mean that there's procedure calls stored on a
fixed-size stack, every call should conceptually be neutralized by by a
procedure return as for the app not to eventually run out of stack
space, and returns generally return to the stack level directly below it
in the stack, or in the case of exceptions, multiple steps, until the
place of the closest exception handler, or in the case of application
termination, the application terminates and the entire stack is
discarded.)

</div>

<div style="text-indent: 1em;">

Gambit is stackless. The stack functionality is performed through stack
frame objects, that are linked together in a tree (or web) that's
possibly cyclical. To make code in this environment execute run with an
optimal speed (the as that of C code doing approx the same thing),
extensive optimizations are applied to the stack handling.

</div>

<div style="text-indent: 1em;">

While the concepts of stacklessness and stack frame objects ought to be
straightforward enough, the details of how Gambit actually performs
this, may be a very complex matter, and therefore we will explore this
topic in detail below.

</div>

Gambit's threading and exception handling mechanisms are, given that the
Gambit stack is already in place, quite non-complex matters, they're
essentially simple applications of use of the stack model.

<div style="text-indent: 1em;">

The threading needs interrupt hooks at regular intervals in the
application code in order to function, which the compiler sugars the
code with – this is a delicate topic that we will explore further below.

</div>

Gambit's IO model is based on an event dispatcher loop centered around a
select() OS call and use interrrupt timer functionality from the host
OS. While Gambit does this in a particularly elegant way, in-application
central IO/event dispatcher loops have been in a quite wide use since
very long – since the inception of Unix systems, say – and has been
refined into simple to use API:s in libraries such as libevent and
libuv.

<div style="text-indent: 1em;">

Therefore, even while lots of effort and exactitude is required for
implementing this in a way that really spins uncompromisingly, we relate
to this functionality as noncomplex.

</div>

Beyond the stack model, making Scheme code execute at speeds comparable
with that of C, a careful design of the compiler – including extensive,
complex optimizations – and of the VM/runtime system (type and object
handling etc) are required. We discuss this in more detail below.

Thus, we can now sum up complexity in Gambit, beyond that of a general
programming environment such as that of Java, as being focalized to the
design of the stack handling and to how the matters of how very high
performance of code execution is achieved, which are dealed with by the
compiler and the tuning of the details of the VM/runtime system design.

## The C-level anatomy of Gambit and a Gambit-based application

<span style="background-color:lightyellow;">When having Gambit or a
Gambit-based application in sourcecode form only, what steps are
required to compile it?</span>  
(running the configure script – the configure script generates
./Makefile \*only\* or other files too? - the Makefile as for use by
make without parameters just as to compile the program, essentially only
invokes the C compiler and linker for the different C files in the
appropriate order? C files generated out of Gambit's runtime's scheme
files, required to make compilation out of C code only work. ./configure
and running make on the makefile is all needed to produce the C
binaries? For distributing an application implemented in Gambit, only
distributing the C files generated by Gambit for the application's
Scheme files, is sufficient. Thus for such an application, add to the
configure script/Makefile instructions to compile also the application's
bundled C files, and include those in the linking process. Any advice on
how to prepare such an Gambit application from C files distribution as
easily as possible, are there any examples anywhere available?)  
  
<span style="background-color:lightyellow;">Conceptually, what does the
configure script check for and what output files does it
produce?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">In what order are Gambit's
source files compiled and linked? This order is functionally significant
right?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">In what areas are there
differences in what C/asm code of Gambit is used, between processors and
platforms?</span>  
(the select loop and files and networking, how interrupt signals are
made, more? Native bit size of values of course.)

## The C-level anatomy of starting Gambit or a Gambit-based application

<span style="background-color:lightyellow;">When Gambit or a Gambit
application is started, what is approximately the code path of the
initiation all way up to that Scheme code starts to execute? (roughly
locations of the different functions in Gambit's C code, that are
invoked) Where is the main/WinMain procedure? What OS calls are
made/state for the Gambit OS process with the OS is set up, and what
information is acquired from the host OS? What code is run to initialize
the heap? What code is run to initialize the stack handling with its
first stack frame (perhaps this q should rather be put in the section
about stack handling)?</span>  
(Marc's answer here)

## Structures

<span style="background-color:lightyellow;">Internally, are structures
just special-type vectors?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Is there any inheritance
between structures, i.e. can I create a structure of type car and then
make a subtype structure of type volvo? If so, how does this inheritance
work – is it just that when making a volvo object, a vector is created
with slots for all of a car's properties and appended to that is slots
for all of volvo's properties too – how does the car property access
procedures typecheck for if it's a car or a volvo?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Where in Gambit's code is
the structure type handled, and what's the anotomy of this
code?</span>  
(Marc's answer here)

## The ports/IO system

<span style="background-color:lightyellow;">Gambit has a variety of port
types. Are the primary groupings/super-types of these, byte ports,
character ports, and object ports? Is there some kind of strict
inheritance between these, that each character ports is or contains a
byte port too, and that every object port is or contains a character
port too?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What is the anatomy of the
IO/ports system and its sourcecode? At what places in Gambit's code is
data sent/calls/mutations done to the OS as for Gambit to feed it with
data, at what places in Gambit's code are things for Gambit to listen
for events for (file handles, sockets, interrupt timeout?) inserted? How
is the core IO-time scheduling done (on all platforms), is it by a
select() or select()-equivalent call only, or is there any alternative
return path from the OS into Gambit, during wait for timer timeout or IO
input from the OS? (we discuss the reception and handling of timer
interrupts separately in the section on threading.)</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What is the anatomy of the
IO/ports system's sourcecode – which are the main procedures and code
sites, approximately how does it fit together?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What is the anatomy of a
port, it's a structure with approx what properties, it has a will so
it's shut down the right way when GC:ed?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the code
path for a |display| or |write| or |write-subu8vector| to a port, for
various port types, all the way up to the end destination for the
operation (the network device/OS file/target string
buffer/etc).</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">At what points is the
ports/IO system copying (both by function and by location in the
sourcecode)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">When select() has given an
event for a file handle/socket, what is the code path that is invoked to
propagate this event into the Scheme world?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Does Gambit support
select()-ing for more than 64 sockets on Windows? (this is a limit in
Windows' select)</span>  
(Marc's answer here)

## Console interaction and REPL

<span style="background-color:lightyellow;">Where is the sourcecode for
the console interaction (incl libreadline kind of functionality) and
REPL code, and what is the anatomy of this code?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What parts are in Scheme and
what in C (I understand this would all better have been done in C but
due to historic reasons right now some are in C)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">If one would want to pipe
REPL:s elsewhere than to the console, what hooks would be used? The
place that spawns a REPL for uncaught exceptions, where is it so that
one could direct those REPL:s to elsewhere than to the console
REPL?</span>  
(Marc's answer here)

## The threading system

<span style="background-color:lightyellow;">How and where is the
threading system bootstrapped? Where is the primordial thread
initialized, and what makes it be the code that is actually the first to
be run (except for, that at the time it's the only thread that
exists)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Is each thread a structure
only? Roughly what properties does this structure have? How many bytes
in size is this structure, on different architectures (32bit or 64bit)?
Does Gambit provide any global state where threads and thread groups are
stored, if so which is this structure and where is it declared, or does
the caller need to keep references for them as not to GC?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How does Gambit ensure that
interrupt checks are distributed in the code at such locations that
smooth execution across threads is guaranteed, while the overhead for
interrupt checking is kept low enough? How many % of code execution time
is taken up by interrupt checks? The mechanism that puts interrupt
checks in code is calibrated in such a way that there is no place in the
code, no loop and so on, that is exempted from interrupt checks, in such
a way that \>1-2ms of code execution would happen without any interrupt
check being made? So, (let loop ((at 0)) (if (\#\#fx\< at 1000000000)
(loop (\#\#fx+ at0)))) will never cause any issues with threading
smoothness, right?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What principle is applied by
Gambit when choosing what next thread to invoke? Where in Gambit's code
are these therad switches made? If there's any particular complexity to
the subject of making thread switches, please describe (such as,
invoking the right trampolines or leaving the C/asm stack in the right
condition or sth .. perhaps this is taken care of by the stack handling
and that's it)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What is the anatomy of the
thread switching mechanism: so first off, while not executing code but
waiting for IO or timeouts from the OS, Gambit has a timer interrupt
signal scheduled with the host OS (are these rescheduled all the time by
Gambit, or is the OS set to recurringly make such interrupts at a
certain interval forever)? Then, all Gambit-generated code is sugared
all over with interrupt checks, so the interrupt signal handler
procedure does something like mutating a global variable has\_interrupt
to true, and these interrupt checks do sth like if (has\_interrupt) goto
handle\_interrupt or handle\_interrupt(); depending on if the code is
single-host or multiple-host? Then, does this handle\_interrupt always
check for stack overflow? What about heap overflow, or trigging a GC?
How does it check if it's time to switch to another thread now? Does it
do anything more? In case of switch to another thread, how is the
current point of execution left in a way that maintains
application/stack/etc integrity (perhaps that's a stack handling-section
question)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Where/how is it configured
for how long a thread should run before a switch is made to the next
one? Is this a global or a per-thread configuration?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">While code can have (declare
(not interrupts-enabled)) as not to accept any interrupts, the RTL is
mostly compiled with interrupts enabled, so while inlined procedures
such as + fall within the same interrupts-enabled setting as the code
where it's used, non-inlined procedures such as assq do produce
interrupts, right?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What's the anatomy of
Gambit's threading system sourcecode, in what source files and locations
are the threading system and the threading interrupts represented (I
suppose the latter is in the compiler)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Does the threading system
schedule between threads based on the number of thread interrupts
passed, or based on the amount of wall clock time passed?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What is the dynamics of the
priority, quantum and priority boost parameters to the threads, perhaps
this is described completely enough in the specification document (don't
remember its name or url right now)? If I want one thread to be of high
priority and one of low, what parameters are needed? If I want one
thread to get double or half as much CPU time as another, what
parameters are needed?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Beyond what has been covered
above, is there any additional complexity to the threading system, or
notable details not obvious from looking at its sourcecode?</span>  
(Marc's answer here)

## Exceptions

<span style="background-color:lightyellow;">Is the basic anatomy of the
exceptions system, that first and foremost there is a |raise| procedure
that takes one argument which is the exception value and which can be of
any type, and, that in the dynamic environment there's a current
exception handler parameter, that is a procedure, that is invoked on
exception, and this is what with-exception-catcher and
with-exception-handler uses to implement its functionality? So, the
exception object type/-s is really a matter completely separated from
the basic exception raising and catching mechanisms, and are only used
as containers for conveying the content or message of each respective
raised exception?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Gambit has a number of
different exception types: os-exception,
wrong-number-of-arguments-exception etc. etc.. Are these arranged in any
kind of hierarchy? Are they all sub-structure-types of the exception
type? Is there any way to get any kind of group type out of
these?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Do any particular
precautions need to be taken in order for a heap overflow exception to
be handled 'safely', i.e. for the exception handling code not to
unintendedly trig a new heap overflow exception in turn, that would
terminate the program or cause otherwise unintended behavior?</span>  
(Marc's answer here)

## Memory handling

<span style="background-color:lightyellow;">Beyond freedom from bugs,
were any particular strategies assumed in making Gambit free of buffer
overflows and memory corruption bugs? (I'm clear this might be a
pointless question)</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Does Gambit have any quick
reclaim mechanism for quickly discarding (GC:ing) objects that are not
in use? Sth like, (define (a) (let ((b 1) (c 2.99999999999999) (d
(make-string 1))) (+ b c)) – right at the point when a returns, is the
memory for all of b, c and d immediately freed? Perhaps only b, because
the compiler knew it took space only within the current stack frame and
not otherwise on the heap so presuming the compiler knew to discard that
stack frame quickly, it did. Does it discard c too (even while it
occupies a little bit of heap space outside of the stack frame, no?) but
not d, because b it knows what type it is of, but d was generated by an
external procedure so quick freeing cannot be done but it will wait
until the next GC?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">There is no central index of
all objects on the heap, they're just allocated space for in the chunks
of system memory allocated by the memory handling mechanism
right?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Is any particular design of
the heap or stacks required, for there to be support for concurrent
garbage collection? (in same cpu core or multicore)</span>  
(Marc's answer here)

## GC (the default stop & copy implementation)

<span style="background-color:lightyellow;">Which are the variables used
for determining if it's time to perform a GC, and where is the code that
maintains those counters?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the anatomy
of a garbage collection, including what kind of state structures are
used during the process (for the markings and for keeping track of
finalizers). What state does the garbage collector keep between gc:s?
The state structures (for keeping track of finalizers for instance), are
they such that they expand dynamically during the GC, if so are those
just malloc/free:ed or is there any special design for their
allocation/freeing to be as fast as possible?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What is the entry point for
making GC iterations, the \_\_\_gc() C procedure? Does the garbage
collector have more entry points than this, if so what are they used
for?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How are finalizers handled?
Because, I suppose, the finalizer needs to finish before the object is
discarded. So, when an object with a finalizer ends up not marked by a
GC process, then the GC makes a note of that object in some kind of
list, and each such object has some kind of status flag that can be
either of “finalizer not invoked”, “finalizer running” and “finalizer
done”, and if it's “finalizer done” then the object is GC:ed, and after
each GC all entries with “finalizer not invoked” are invoked? Please
describe the possible states in here, where this state is stored, and
which the state changes are and when the changes take place.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">On a GC, does Gambit always
allocate memory for the target of the copy anew, and free() all
allocations for the old copy at the end of GC? Or is there some keeping
of memory allocations to not need to spend time on all new malloc()
calls on each GC call?</span>  
(Marc's answer here)

## Data types

<span style="background-color:lightyellow;">Every variable value in
Scheme is represented internally as an integer, and has a tag about what
fundamental data type the respective value is, right? What are the bit
patterns in use for describing datatypes here? Where in Gambit's code is
the basis for and use of those bit patterns implemented (as to know how
to add or edit a type)? (I'm aware that fixnum is described by the two
lowest bits being 0.)</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Whenever a variable value
has a payload – some kind of object contents – a pointer to the memory
address at which this payload is located, is included in every object
reference on the heap for that object, right?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What was the motivation for
using the lower bits in the variable values for the tag rather than the
upper ones?</span>  
(Marc's answer here)

## Hashtables

<span style="background-color:lightyellow;">The hashtable and there used
hashing algorithm, how does it work? Is there a paper anywhere that
describes it?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">In what components/elements
are hashtables stored internally (some kind of chain or tree I'd
suppose, but what)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">By what reason is it that a
table must not be mutated during table-for-each, what's the worstcase
outcome if one mutates a table during it?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Is it possible to implement
hashtables that fit together in a tree kind of shape, so that if I make
table-set\! on a parent then that one is visible to all child and
grandchild etc. hashtables but not the other way around?</span>  
(Marc's answer here)

## Numbers

<span style="background-color:lightyellow;">What are the rules for
automatic type changes of numbers on number operations? Say, fixnum +
flonum gives a flonum, that's obvious, but what about more complex cases
– when are bignums generated, when are bignums scaled down to fixnums,
and so on?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How are bignum structures
stored internally, each such value is an object reference to a “bignum
object”, and that object is a vector of integers that each contains a
couple of decimals of the bignum value? With what procedures can I
introspect and manipulate the element parts of a bignum value?</span>  
(Marc's answer here) <span style="background-color:lightyellow;">If one
would want to change the structure format for the bignums, for instance
for plugging in another bignum library, how would one go about for
that?</span>  
(Marc's answer here)

## The compiler

<span style="background-color:lightyellow;">What is the basic anatomy of
the compiler's sourcecode, and what is the basic code path that any
compilation process takes? In all cases, I'm clear already there's two
steps, a Scheme to GVM step, and a GVM to native code step (with the C
backend or the native backend).</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the
different phases that a compilation process takes (including any loops),
and what form the sourcecode is stored in and what information form the
compilation output is in, and what intermediary forms between sourcecode
and compilation output are there and what's the purpose of those, in the
different phases.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Approximately what
optimizations are made by the compiler in the Scheme to GVM step and the
GVM to C or native code steps respectively? (Let's define optimization
as any logics that make the output code neater or faster than if that
logics would not have been there, or if that logics would have been less
well designed/thought through.)</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Does the compiler look up
all call/cc:s, and make a CPS conversion of all the code, during the
compilation process? What is done with the CPS-converted code in order
to generate the fastest or otherwise slimmest resultant code (if this is
what the compiler does)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please list the academic
papers and algorithm names that describe /something like/ what Gambit
does during compilation.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">If I wanted to implement a
new primitive function that requires special (inlined) compiler output,
say \#\#sysmem-byteref , where in the Scheme to GVM compiler's code and
where in the C backend would a change need to be made, and approximately
what kind of change would need to be made? </span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">If I wanted to implement a
new primitive conditional that requires special (inlined) compiler
output, say a variant of |or| or |if| that we call |or/0| or |if/0| that
treats fixnum 0 as \#f, where in the Scheme to GVM compiler's code and
where in the C backend would a change need to be made, and approximately
what kind of change would need to be made? </span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Secondarily, I may want a
first-class variant of this primitive too, for use both in compiled code
and by the interpreter. What is a suitable place in Gambit's code to put
a “wrapper” of the compiled version of \#\#sysmem-byteref to a
first-class version of it, and how should that code look?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the general
nature of the GVM language, and more specifically what kind of
operations the GVM code language contains. Basically the GVM language
describes procedures and their execution flows (stack operations,
conditionals of the execution flow, jumps/invocations to procedures,
trampolines?), and other than that it's invocations of primitives (+
etc)?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the GVM code
for a closure.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the GVM code
for a conditional.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the GVM code
for an invocation of a procedure with one or more arguments, and for its
return.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What state structures are
needed to run a GVM (within C backend)? (both for the stack and to
maintain the execution state needed to handle the juggling of host
functions) Please describe with some detail – what's on the C stack,
what's the structure of the processor struct and stack structures and so
on.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the kind of
functionality/functions needed by a GVM. So for instance, it needs to
have a GC. What more? Some kind of stack handling machinery including
dynamic addition and removal of slots to stack frames? (I suppose the
entire concept of host procedures is within the C backend's architecture
only, the GVM design in itself has nothing to do with those?)</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">In many places, exceptions
are raised from a really low level point, say that + was applied to the
wrong data type and now there's a type exception. How does the GVM code
look for such handling, and how is this implemented in the C
backend?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Exactly, what is the extent
of (declare (not interrupts-enabled)), as in, if a piece of code is
compiled with this, then what Gambit forms, primitives and procedures
can it call in such a way that the thread scheduler to is \_guaranteed\_
not to switch running thread meanwhile?</span>  
(Marc's answer here)

## The \[GVM code to\] C backend and the resultant object file

<span style="background-color:lightyellow;">Please describe the general
anatomy of the C code output. (It is clear that each C file has some
kind of headers and information structures inlined as constants, that
are for somehow instructing the parent Gambit process what globals or
alike the object file contains, rather than just code)</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What is the basic execution
flow of the C code in an object file? Right when it's loaded by the OS,
which code in it is run? What is done? To feed the parent process with
globals would, I suppose, be one. It is the parent process that then
invokes an initialization routine in the object file, that invokes its
top level code, right?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">The C code seems to be a
pretty hardcore example of macro use. Is there any higher level of
understanding or structure in all the macros that, if understood, makes
it easier to understand the macro definitions and how the macros and
their use fit together? What is the anatomy of the files with macro
definitions?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">When the host procedore
returns (which happens basically between every evaluation step in code
compiled with the safe declare), to what code in Gambit's runtime does
it return then, what does that code do and in what condition does it
jump back into the host procedore?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the C code
for a closure.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the C code
for a conditional.</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">Please describe the C code
for an invocation of a procedure with one or more arguments, and for its
return.</span>  
(Marc's answer here)

## Stack handling and related code generation aspects (including for GC traversibility, safety of stack overflows and call/cc) and trampolines

<span style="background-color:lightyellow;">Are stack frames the highest
level of granularity that Gambit deals with stacks at, or do they have
subcomponents (except for the slots for the individual contained values
of course)? What about the code (define (a) (let ((b \[value\])) (let
((c \[value\])) \[code1\]) \[code2\])), what happens in the stack as
code1 completes and code2 is started to be executed and c thus is
disposed from the stack?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How many bytes does a stack
frame occupy?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">What contents does a
continuation value or a continuation object value have, beyond (being) a
reference to the stack frame to be executed on its invocation?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How is the dynamic
environment and parameter values implemented? When invoking a
continuation or there is a change of active thread, how is the switch of
dynamic environment done?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How is the stack layout
designed, as to be traversible by the GC? Were any particular
considerations needed for this, to maintain the platform independentness
of Gambit's C backend?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How is the stack layout
designed, as to suit call/cc? What is the full mechanism of a call/cc,
and what is the anatomy in site of a call/cc call, and, does it have any
dependencies otherwise in the RTL?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">For the Gambit-generated
native code to be safe for stack and heap overflows, it appears to me
that there is basically some handling code between each step of every
evaluation that involves the return of the host function. Why is this?
Say that there is a processing loop, (let loop ((i 0)) (if (\#\#fx\< i
1000) (begin (\#\#u8vector-set\! u i (+ (u8vector-ref u i) 1) (loop
(\#\#fxnum+ i 1))))) say, why can't it just be one solid piece of code
that executes through the loop just like that? What is the proof that
stack overflow will never happen unless malloc fails? Within Gambit, is
by stack overflow, always overflow of the C-level stack meant?</span>  
(Marc's answer here)  
  
<span style="background-color:lightyellow;">How much C stack space does
a Gambit process make use of? Can it be adjusted?</span>  
(Marc's answer here)

## Debugging

<span style="background-color:lightyellow;">If a lowlevel crash would
happen in a Gambit program, say somewhere outside the application's
object files, what are the steps you normally would take/recommend as to
determine the source of the error?</span>  
(Marc's answer here)
