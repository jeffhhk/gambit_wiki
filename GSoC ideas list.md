Here is the ideas list for the 2008 GSoC program. Please check this page
regularly as we will be adding new ideas as they come up.

Note that this page is editable. If you have suggestions for projects
please add them to the second section ("project suggestions"). The first
section contains the projects that are officially proposed by the
organization. You can make corrections to the text but please don't make
substantial changes.

The Gambit [ wish list](Wish_list "wikilink") may also be a good source
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

### Termite package

Termite is a thin layer on top of Gambit which provides Erlang-like
features for concurrent and distributed computing. The current code's
integration with Gambit could be improved.

<s> The project consists in cleaning up the code and turning it into a
package that can easily be loaded into Gambit to extend the runtime
system. </s>

*This has already been done in a soon-to-be-released version of Termite.
Other projects related to Termite would be nice, though, and I would be
willing to help/supervise.* -- [Guillaume](User:Guillaume "wikilink")
16:05, 15 March 2008 (EDT)

Difficulty: medium to hard

Required skills: Scheme, Gambit, multithreading, continuations,
networking

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

### Module System

Gambit offers a simple namespace handling system for code
compartimentalization, and the Snow\! code compartimentalization and
distribution system improves on this. That shouldn't be the end of
modular programming in the Gambit world, though, and a project exploring
that space is underway here: [Module\_System](Module_System "wikilink").
The project could use one student as complemention of the team.
Depending on the skills of the student, he/she could tackle different
parts (help compiling all requirements onto a minimal set of features
(maybe formal description of the problem spaces, finding missing pieces
when coming from an OO background, ...), or implementing parts of the
system).

Difficulty: very hard

Required skills: Gambit, functional programming, metaprogramming, OO
programming

### SWIG interface

Gambit's foreign-function-interface (FFI) can be tedious to use for
interfacing to C libraries with many functions (such as Xlib, OpenGL).
SWIG (http://www.swig.org/) is an interface generator that supports
several high-level programming languages, including a few Schemes
(Chicken, MzScheme, Guile). The goal would be to get SWIG working with
Gambit.

Difficulty: medium

Required skills: Gambit, C/C++

## Project suggestions

Add to this section any projects you want to suggest to the organizers.
Acceptable projects will be moved to the above section.
