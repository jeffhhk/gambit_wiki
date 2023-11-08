#### Namespaces

### Introduction

Gambit implements a namespace mechanism, which is used to separate
internals from user code and also to partition the Gambit internals into
several parts (e.g. the compiler parts use a different namespace than
the interpreter parts).

The namespace mechanism is not described in the official documentation,
but it is revealed by the examples bundled with the Gambit source code.
Marc maybe didn't officially document it since he didn't want to
sanction it as official mechanism that he would need to support into the
indefinite future, and he maybe originally just wrote it for his own
purposes (those are just speculations). In the meantime, as there is now
an "official" module system in the form of SNOW\!, users should make use
of the latter instead of using the namespace mechanism directly (the
writer of these lines also intends to continue to work on his
"chjmodule" module system to further explore dynamic programming and
other concepts like parametrization).

### How does it work?

There is a special rule for fully qualifying an identifier, and there is
a special form which can be used to (a) declare the currently active
default namespace, and (b) to declare a list of given identifiers to be
in a particular namespace. Both uses of the special form only affect
unqualified identifiers.

#### Full qualification

Gambit treats the \# character (`#\#` in Scheme syntax) in identifier
symbols as a special character separating the namespace from the local
name. Example:

`foo#bar`

is the `bar` identifier in the "foo\#" namespace (I include the \#
separator character here since that's also how the namespace form works
and because that allows to specify the empty namespace).

There are two special namespaces. One is the "\#\#" namespace which
Gambit uses for most of its internals, and the other is the "" (empty)
namespace. The empty namespace is the default namespace for user code
when the Gambit interpreter or compiler is started, and is also where
Gambit puts the many non-internal identifiers described in the manual.

The qualifier syntax handles "\#\#" like any other namespace. Similarly
to the example above:

` ##include`

is the `include` identifier in the "\#\#" namespace.

But there is no qualifier syntax for the "" namespace, so there is no
way to refer to identifiers in the "" namespace from outside it. (A
single \# at the beginning of a token might work, except that it's
already used for many other purposes by the Scheme standards.) If Gambit
puts a name in the "" namespace, it often puts the same name in the
"\#\#" namespace. But when the names refer to functions, the two
functions do not necessarily behave the same way; see
[Usage](#Usage.md) for the details.

#### Declaring an identifier as belonging to a particular namespace

The

`(namespace (STRING SYMBOL...))`

special form specifies that the list of the given identifiers (symbols)
are to be treated as belonging to the namespace given in STRING.
Example:

`(namespace ("foo#" bar))`

says that whenever `bar` is mentioned in the code that follows in the
same scope or file, it is to be implicitly treated as `foo#bar`.

#### Switching the default namespace in code

Identifiers which do not contain a \# character are treated as belonging
to the active namespace in the current scope (see below for what the
scope is). This is the empty namespace by default. This can be changed
by putting

`(namespace (STRING))`

before the rest of the code; e.g. using a namespace declaration without
a list of identifiers. See the note above about `##namespace`.

#### Scope of namespace declarations

A namespace declaration is in effect until the end of the current
`lambda` or `let` construct (note that `begin` doesn't open a new
scope), or, if specified at the top level of the file (or just inside a
`begin` form residing in the top level) until the end of that file.

#### Switching the default namespace in the repl

If entered in the repl, or run by the `eval` procedure, the default
namespace used by the repl (which is also the empty namespace by
default) is changed. This then remains active until the next namespace
declaration is entered or `eval`ed.

#### Accessing the `namespace` identifier

Note that the identifier `namespace` is part of the empty namespace.
Because of the exception to the qualifier syntax described
[above](#Full qualification.md), if you change the default
namespace, then you have to use `##namespace` instead. `##namespace` and
`namespace` behave the same.

### Usage

#### Modularization

The namespace mechanism can be used to partition user code in a style
similar to how C works, by writing a "header" file for a library which
declares all the exported values to be in the namespace that has been
chosen for that library. In the Gambit source code and the examples,
Marc uses the convention of adding the \# character to the name of the
module (e.g. if he writes a module "foo.scm" he will name the header
file "foo\#.scm").

#### Access to Gambit internals

Since the namespace feature doesn't prohibit access (it's just for the
sake of convenience, not for protection), Gambit is somewhat special in
that all of its internal procedures can be accessed by the user; this is
very useful to learn how the internals work and also to extend the
system without hacking the Gambit source code. It must be noted though,
that most of those procedures do not check their types, so you will
quickly get a segfault when feeding them data of the wrong type.
Sometimes this missing type check is also (mis)usable (or is even used
explicitly by other Gambit internal procedures) to access data
structures on a lower level:

`(define-structure foo a b c)`  
`(let ((v (make-foo 123 "hello" 'world)))`  
`  ;; (mis)treat the structure as vector:`  
`  (let ((len (##vector-length v)))`  
`    (let lp ((i 0))`  
`      (if (< i len)`  
`        (begin`  
`          (println "value at position " i ":" (##vector-ref v i))`  
`          (lp (+ i 1)))))))`

This prints

`value at position 0:#<type #2 foo>`  
`value at position 1:123`  
`value at position 2:hello`  
`value at position 3:world`

(Note that if you accessed elements behind the given length, you would
access unrelated memory contents or invalid memory addresses.)

#### Producing low-level unsafe code explicitly

It's also worthwhile knowing that the basic primitives (like memory
accesses, low-level arithmetic, etc.) which are mapped by the compiler
directly to C constructs (actually C macros as defined in the
include/gambit.h file) are a bunch of such identifiers in the "\#\#"
namespace. Like `(##car obj)` will be translated to `___CAR(obj)` which
is then expanded by the C compiler to basically a pointer dereference.
So those can be used in user code to let the compiler output those
low-level constructs, which do not do any type checking (and are simple
enough for the C compiler to enable optimizations on the C level) and
thus very fast, without declaring your whole Scheme code as being unsafe
by using the `declare` special form (which would achieve the same
thing).
