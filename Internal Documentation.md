People who want to [
contribute](Contributing%20Patches%20to%20Gambit%20Source%20Code.md) to
Gambit development will need to learn something about how the Gambit-C
runtime and compiler are organized. While we intend that source code
documentation be included in the source itself (currently there is very
little documentation), we intend that descriptions of program design or
algorithms used in the runtime and compiler could be included here.

## Namespace handling

See [Namespaces](Namespaces.md).

## Runtime Library

### Memory Management

General notes on internal object storage and memory consumption is on
the [Debugging](Debugging.md) page. Also see [Notes on Memory
Management](Notes%20on%20Memory%20Management.md).

### Thread System

#### Safely dumping a thread's state

Summary: Dumping thread state needs to be done with care as
\#\#thread-continuation-capture if run on threads where code execution
didn't start yet, causes SIGSEGV.

To safely dump a thread's state (for instance, one you got from
thread-group-\>thread-list), refer to the following routine, which reads
out the thread's state, taking into consideration any possible state.
(Thanks to Marc 16 dec 2011 while Gambit at v4.6.2.)

The macros are from define-type thread of lib/\_thread\#.scm .

XX While I think the following code is threadsafe, I have not got that
confirmed. If you by any reason suspect the contrary, please let the ML
know.

`(let* ((end-condvar (macro-thread-end-condvar thread))`  
`       (exception?  (macro-thread-exception?  thread))`  
`       (result      (macro-thread-result      thread)))`  
`  (cond ((not end-condvar)`  
`         ;; thread has terminated`  
`         (if exception?`  
`             (begin`  
`               ;; thread has terminated with exception`  
`               (display "Terminated with exception:\n")`  
`               (display-exception result))`  
`             (begin`  
`               ;; thread has terminated with result`  
`               (display "Terminated with result:\n")`  
`               (pretty-print result))))`  
`        (exception?`  
`         ;; thread has never run and is not terminated`  
`         (if (not result)`  
`             (begin`  
`               ;; thread is not yet started`  
`               (display "Not yet started\n"))`  
`             (begin`  
`               ;; thread is started but has not yet run`  
`               (display "Started but has not yet run\n"))))`  
`        (else`  
`         (let ((c (##thread-continuation-capture thread))) ; See note above`  
`           (cond ((and max-head max-tail)`  
`                  (display-continuation-backtrace c (current-output-port) #f display-environment max-head max-tail))`  
`                 (max-head`  
`                  (display-continuation-backtrace c (current-output-port) #f display-environment max-head))`  
`                 (else`  
`                  (display-continuation-backtrace c (current-output-port) #f display-environment)))`  
`            ))))`

### I/O System

### Arithmetic implementation

### Eval

#### Continuation manipulation

The manual lists `continuation-graft`, `continuation-capture`, and
`continuation-return` but doesn't describe them. The REPL debugger, and
possibly other things, use them. See Marc Feeley's paper *A Better API
for First-Class Continuations*.

### REPL

The REPL has some fairly interesting functions and variables, especially
for hackers.

#### Variables

  - `##repl-location-relative`  
    Should the REPL give relative or absolute pathnames. **Note:** When
    using emacs with gambit, it is useful to set it to \#f, especially
    if you change the current-directory.

#### Functions

  - `##cmd-`*x*  
    where *x* is a REPL command letter (typed after a comma from the
    REPL). Executes that command as if it was executed inside of the
    REPL. For instance `##cmd-b` displays a backtrace.

### Record system

That is, `define-type`. Based on SRFI-9, but extensions not documented.
This email provides the best explanation
[1](https://webmail.iro.umontreal.ca/pipermail/gambit-list/attachments/20090226/af2ee44c/attachment-0001.txt)

### Introspection

#### Symbol introspection

To get list of interned symbols:

`(define (symbol-table->list st)`  
  
`  (define (symbol-chain s syms)`  
`    (let loop ((s s) (syms syms))`  
`      (if (symbol? s)`  
`          (loop (##vector-ref s 2) (cons s syms))`  
`          syms)))`  
  
`  (let loop ((lst (vector->list st)) (syms '()))`  
`    (if (pair? lst)`  
`        (loop (cdr lst) (symbol-chain (car lst) syms))`  
`        (reverse syms))))`  
  
`(define (interned-symbols)`  
`  (symbol-table->list (##symbol-table)))`  
  
`(pp (length (interned-symbols)))`

(From Gambit ML 2009-03-22)

### Program startup

  - The entry point function (either main(), winmain()...) will be
    generated (in linker file) to call either `___main()`,
    `___main_UCS_2` or `___winmain`.
  - These functions do very basic initialization (setup `___base_mod`
    and `___program_startup_info`) then passes to `___main()` in main.c.
  - This function in turn calls `___setup()`, which does
      - sets up the VM
      - Call linker
      - Initialize tables (symbol, keyword, global variables,
        primitives)
      - Kick off the kernel

## Compiler

Script igsc.scm inside gsc directory can be used to get REPL of compiler
so you can inspect details.

### Frontend

The frontend entry point is **cf**, main function to do compilation is
**compile-parsed-program**, which generates GVM instructions. Some
optimization is done by frontend via function **normalize-program**.

TODO: Optimizations, program tree representation.

### Intermediate representation

The closet document to describe Gambit Virtual Machine is probably [A
Parallel Virtual Machine for Efficient Scheme
Compilation](http://www.iro.umontreal.ca/~feeley/papers/pvm.ps.gz).

#### Operands

There are 6 types of operands, described in \_gvmadt.scm. All operands
are encoded to a number. The following list is extracted from
\_gvmadt.scm:

` reg(n)       n*8 + 0`  
` stk(n)       n*8 + 1`  
` lbl(n)       n*8 + 2`  
` glo(name)    index_in_operand_table*8 + 3`  
` clo(opnd,n)  index_in_operand_table*8 + 4`  
` obj(x)       index_in_operand_table*8 + 5`

Global variables (glo), closed variables (clo) and objects are saved in
\*opnd-table\*. Reg, stk, lbl are respectively abbreviations of
register, stack and label. All these operands can be created by make-X,
where X is the abbreviation.

In .gvm output, registers are prefixed by "+", stack by "-", objects by
a single quote, labels by "\#". Global variables are displayed by
variable name, closed variables are enclosed in brackets, objects

#### Instructions

GVM instructions include apply, copy, close, ifjump, switch, jump,
comment and label, in \_gvm.scm, "Virtual machine instruction
representation" section.

Instructions in .gvm output are represented by their name. If it's a
poll jump, the instruction will be followed by a star. If it's a safe
jump, it is followed by a dollar.

#### Optimization

After GVM generation, dead code is removed by **bbs-purify\!**.

### Backend

Backend is selected by **target-select\!**. All backend functions start
with **target.**. The only supported backend is C, which explains the
"C" part in "Gambit-C", reside in \_t-c-\[1-3\].scm.

The initial state of a module includes global variables, symbols,
keywords, subtypes, number of used labels and procedures. All are
maintained during compilation (see "Object management" in \_t-c-1.scm)
and dumped out as C declaration. The entry point that does this is
targ-heap-dump. C output is written in C macros only. **gambit.h** will
actually produce C code suitable for each architecture.

#### Linking

Because each module contains each own global variables, symbols and
keywords. All must be combined to produce single tables for those.
Linking works by reading linker info in generated C files. Linker info
is actually sexp embedded in C files (and be never read by C compiler as
it is protected by \#ifdef). Structure of linker info can be found in
**targ-read-linker-info**, which is a list of

1.  Compiler version
2.  Module name
3.  Modules
4.  List of symbols
5.  List of keywords
6.  List of supplied and demanded globals
7.  List of supplied and not demanded globals
8.  List of not supplied globals
9.  Script line

Linking is started with **targ-linker**. When incremental link is
demanded, INCREMENTAL\_LINKFILE will be defined in generated C file.
gambit.h will handle the rest.

[Category: Internals](Category:%20Internals.md)
