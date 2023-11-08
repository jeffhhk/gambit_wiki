**NOTE: the information below is outdated (from around 2008). You may
find more up-to-date information at page of the
[Black\_Hole](Black%20Hole.md) module system.**

**Older NOTE: Please note that there has been progress in module system
development during 2008-2009, and there is a sharp module system
developed by Per Eckerdal and partially by Christian Jaeger available.
For more info, see the separate Module system mailing list.**

This is a project to create a module system for Scheme. It does not have
a real name yet. For now, it will target the Gambit Scheme system.

The system is aiming to be complementary to the
[Snow](http://snow.iro.umontreal.ca/) package system: unlike Snow, this
project does *not* focus on package distribution, but instead it has the
following objectives:

  - good interactive development capabilities (runtime inspection and
    modification, included light-weight build system for the base case
    to make running code as easy as in any scripting language)
  - good code modularity features (parametrization, optional hygienic
    macros, infrastructure for generics, ..)
  - generality (run code written in different language styles, to some
    extent in different module philosophies, interface with object
    systems, host languages requiring translation like lazy-by-default
    variants of Scheme or maybe different languages like Arc or XSLT)
  - efficiency (employ cross-module inlining, parametrization
    specialization)
  - attempt portability by moving parts into or building parts on top of
    the Snow framework (possibly not achieving the same level of
    integration with the host system without implementing suggested
    changes of the latter, but at least make modules written by users
    portable to other Scheme systems)

The projects wants to deliver a base system usable for everyday use
quickly, and explore the more advanced objectives in the longer term.

The primary long-term aim is to be very open for extension, by offering
open access to the modularization mechanisms and finding the right
mechanisms, so as to make it possible to integrate code from different
language and modularization philosophies as cleanly as possible, and to
make it easier to contribute towards this goal. We are aware that this
is a noble aim and that the development will involve many steps of
learning and searching for solutions.

During this process, several systems may be developed which either help
daily code development on Gambit or contribute to the understandings and
code base for attaining the longer term goals.

Currently we have the following development steps in mind:

1.  during a first phase, the project will continue to develop within
    the code bases of the
    [chjmodule](http://scheme.mine.nu/gambit/chjmodule) and the
    [gambit-modules](http://scheme.mine.nu/gambit/jlongster/gambit-modules/)
    projects.
2.  implement a better interface language (like e.g. that of Scheme-48)
    on the chjmodule code, targetting parametrization
3.  investigate ideas like those from the CL
    [Lexicons](http://www.flownet.com/ron/lisp/Lexicons.pdf) project for
    making easier to write macros (without automatic hygiene)
4.  investigate what automatic hygiene systems exactly need, implement
    necessary infrastructure
5.  introduce hygienic macros, maybe by partial adaption of existing
    code bases (Abdulaziz Ghuloum's, psyntax, Riaxpander, Scheme-48's,
    Andre v. Tonder's)

Before, during or after steps 4-5 modules with a lazy-by-default variant
of Scheme will be implemented, as an additional play ground for working
on the right hooks for code transformations.

## Leading Contributors

  - [Christian Jaeger](User:Pflanze.md)
  - James Long

## How to contribute

One important part of the task involves getting to know the Gambit
internals well enough to be able to embed the system well. Partly, the
current leading contributors can help with this, partly this has to be
worked out (by reading code, asking Marc, and writing documentation).

Another essential one is laying a sound foundation especially for syntax
handling (macros), considering the limited knowledge of the current
contributors, this involves reading the right literature and discussing
with more experienced people.

Seeking contact with people in other module development projects is an
important duty; we're currently especially thinking of those working
with R6RS implementations, and those working on module systems for
Common Lisp ([Lexicons](http://www.flownet.com/ron/lisp/Lexicons.pdf),
PLT-alike module system on CL GSoC proposal by
[LispNYC](http://lispnyc.org/soc.clp)).

## Google Summer of Code

This project is attempting to participate in the Google Summer of Code
campaign under the hospice of [LispNYC](http://lispnyc.org/soc.clp) and
offers to mentor a student. There are several possibilities for
contribution, depending on the background of the student. One is to
tackle one or more of the above development steps. Another one could be
to work with the PLT-for-CL project and port it to our infrastructure.

## Timeline

Rough points of the planned timeline:

  - James Long is reading literature, and experimenting with several
    existing Scheme module systems
  - Christian Jaeger finishes up and announces the long-promised new
    chjmodule release
  - seeking contact with the other module projects, student selection if
    possible
  - seeking more input, continuing to work on chjmodule and
    gambit-modules to proceed with the above development step list

## Virtual Library

We are keeping a growing [list of
literature](Module%20System/Virtual%20Library.md) which we think is
relevant for our work. It contains items we've read or intend to read,
so it will be growing; we will not put anything there which we don't
think we will manage to read, so by looking at that list you can see
whether we are missing something you think we should know about. We hope
that you will notify us in this case.

## Log

There is a [logfile](Module%20System/Log.md) mentioning the events
in the project.

## License

The project intends, for the time being, to license all code under the
same licenses as Gambit, namely dual LGPL version 2.1 or Apache version
2.0 license (freely choosable). Requirements for making the code
portable may lead us to change the license in due course.
