Please check this page regularly as we will be adding new ideas as they
come up.

Note that this page is editable. If you have suggestions for projects
please add them to the second section ("project suggestions"). The first
section contains the projects that are officially proposed by the
organization. You can make corrections to the text but please don't make
substantial changes.

The Gambit [ wish list](Wish%20list.md) may also be a good source
of inspiration for projects. However the wish list contains projects
which require a very good knowledge of the Gambit system's structure and
internal operation, so they are not advisable for students with little
or no experience with Gambit.

## Official project proposals

### Documentation framework

Gambit's documentation is mainly in the form of a traditional user
manual. To increase its usefulness it needs to be easier to access while
developing code. Documentation needs to be integrated with the tools
including: from the text editor (pop-up documentation when a call to a
built-in function is typed, with information on parameters), from the
REPL, from the command-line. Moreover the documentation should be
automatically checked for consistency on each release (for instance by
checking that the code examples actually produce the result indicated).

Difficulty: easy

Required skills: Scheme, Gambit, and possibly LaTeX, HTML

### Gambit packages

There is an urgent need for easily reusable packages for miscellaneous
features including data-structures, message digests, encryption,
pattern-matching, regular expressions, etc. Portable Scheme code already
exists for many of these (e.g. SRFI's, Snow packages, Chicken eggs,
etc). The project consists in porting these codes to the Gambit system
and to write new packages when appropriate. We expect the student to
choose the set of packages to port/write based on their personal
interests. Due to the high number of packages that need to be worked on,
this project can accomodate up to 3 students. If more than one student
participates on this project, the mentor will help to coordinate the
work of the students.

Difficulty: easy to medium

Required skills: Scheme, Gambit, algorithms

### SLIME interface

SLIME (http://common-lisp.net/project/slime/) is a full-featured Emacs
mode for multithreaded Common Lisp development. A port of it exists for
Scheme48 ("SLIME48"). The project consists in adapting SLIME to Gambit
to allow multithreaded remote debugging from Emacs. The current
gambit.el emacs-lisp package could be used as a starting point.

Difficulty: medium

Required skills: emacs-lisp, Scheme, Gambit, multithreading

### Web framework

Thanks to Gambit's high-level networking functions, non-blocking I/O,
and efficient thread system, a simple high-performance web server can be
written in a few minutes. With Gambit's serializable closures and
continuations it is possible to implement cleanly a fancy web framework
with advanced features (client-side computation, task migration, load
balancing, persistent interactions, undoable transactions). The project
consists in constructing the framework to support these advanced
features, and to demonstrate a few of them).

Difficulty: hard

Required skills: HTML, HTTP, Scheme, Gambit

### Native Threading

Gambit (like most Scheme systems) implements its own lightweight
threading system which is not aware of native, operating system threads.
The so-called Gambit Virtual Machine (GVM) can only be bound to one
native thread. This makes it difficult to utilize true parallel
processing, which will be an important step in adapting to future
hardware. This also causes C code to block the entire GVM for the
duration of the C code's execution.

One solution is to use Termite and spawn multiple Gambit processes.
Although it is a valid solution, it is unacceptable for any real-time
system such as video games. This project aims to extend Gambit in some
fashion to utilize system threads. Various solutions and assumptions
will be explored: spawn multiple Gambit systems across threads in one
process and have a special shared memory buffer? Extend Gambit to
enforce pure functional programming?

Difficulty: hard

Required skills: Gambit, threading, memory allocation, garbage
collection

### GVM virtual-machine interpreter

The Gambit system is based on a virtual machine (the GVM). It is the
intermediate representation for the compiler. The size of the generated
code is an issue for very large programs (such as the Jedi IDE) and for
embedded systems. An interesting approach, to be explored by this
project, would be to design a compact bytecode representation for the
GVM and implement a bytecode interpreter.

Difficulty: hard

Required skills: Scheme, Gambit, compilation

### Module System Infrastructure

The Gambit system adhere's to the philosophy of providing basic, robust
and efficient infrastructure on top of which users can implement
higher-level abstractions. This project consists in designing and
implementing a module system infrastructure for Gambit with which more
sophisticated module systems can be implemented. In particular it should
be possible to use the result of this project to implement the R6RS
module system.

Difficulty: hard

Required skills: Gambit, functional programming, metaprogramming, OO
programming

### Automating FFI Bindings

Gambit's foreign-function-interface (FFI) can be tedious to use for
interfacing to C libraries with many functions (such as Xlib, OpenGL).
There exist some tools to parse header files and extract information
relevant to the creation of FFI bindings. For example, SWIG
(http://www.swig.org/) is an interface generator that supports several
high-level programming languages, including a few Schemes (Chicken,
MzScheme, Guile). There is also gcc-xml which is being used in the
Ctypes Python library. The goal would be to get one of these tools
working with Gambit. The Gambit FFI subsystem will also have to be
extended minimally to support features such as "const" types.

Difficulty: medium

Required skills: Gambit, C/C++

## Project suggestions

Add to this section any projects you want to suggest to the organizers.
Acceptable projects will be moved to the above section.

### Flash bytecode / ActionScript back-end

This project will explore ways to compile Scheme code into either
Bytecode targeted for the Flash Player, or alternatively ActionScript,
and write a prototype back-end for the Gambit compiler.

Difficulty: hard

Required skills: Scheme, Gambit, compilation

### LLVM back-end

A working prototype generating LLVM code (intermediate representation)
has been written by two students as part of a term project. Going from
this prototype, a back-end covering all of Scheme and Gambit's
extensions can be created. This will allow Gambit to take advantage of
the LLVM infrastructure, and also allow to compile code without
requiring a complete C compilation environment to be installed on the
user's machine.

Difficulty: hard

Required skills: Scheme, Gambit, compilation
