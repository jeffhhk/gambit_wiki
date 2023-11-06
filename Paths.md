### Why do I not like the "\~\~" and "\~\~foo" path ideas

by Christian Jaeger, please paint over (with your own graffiti or
artwork) as you see fit.

  - they are a layer on top of the underlying OS; and you can't access a
    file named '\~\~' in the current directory anymore

<!-- end list -->

  - they are not lispy, in the sense that they are a language by
    themselves, a sublanguage inside lisp. Just like regex strings are a
    sublanguage; for the latter a number of lisp/Scheme regex packages
    provide sexpr based regular expressions.

<!-- end list -->

  - I'm missing a good specification for the sublanguage; it feels ad
    hoc, which is ok for "local" things but when it comes to files put
    out into the wild, it's bad if the spec changes and you need to
    adapt the files.

<!-- end list -->

  - I understand that "\~\~" means "use a file delivered with core
    Gambit". This may seem ok now, but it doesn't allow transparent
    shadowing by the user. This may not seem to be an issue now, but
    compare with Perl: there would frequently be a module Foo provided
    with the default installation, but it might be too old for your
    application, and thus you install a newer Foo from CPAN and because
    it's coming in the search path before the core Foo you'll be using
    the newly installed Foo now. Gambit already delivers some utility
    code like digest.scm, this is a candidate for overriding.

<!-- end list -->

  - Specifying "\~\~lib/" instead of "\~\~/lib" to solve the problem
    where distribution makers are tearing apart the traditional \~\~
    directory contents, is requiring adaption of the currently written
    files. If we're going to adapt, it would better be a solution that
    is meant to stay quite some time. Will this be the case?

<!-- end list -->

  - Specifying the "lib" part at all seems to be redundant: the
    |include| can only deal with library files anyway, right? Or, if
    include files are put to "\~\~include" and library files to
    "\~\~lib" consistently, then |load| could as well know by itself
    that a \~\~ path is meant to be a \~\~lib path and |include| could
    know that a \~\~ path is meant to be a \~\~include path.

<!-- end list -->

  - (\~\~ is not understood by other Scheme systems, it's only usable
    for unportable files; this means if digest.scm is portable, it would
    better not be delivered with Gambit (if this means it requires \~\~
    in the path to access it).)

The only thing I wonder is whether the solution at
<http://scheme.ch/gambit/experimental/paths/paths.scm> is too
complicated or otherwise has a problem that makes it unsuited.

That solution \*should\* be usable for non-Gambit-core files, too.
Should there be a hard distinction between core and non-core files?
Should all code loading except for Gambit core files be handled by a
module system which uses such a solution? But why, if the core
procedures like |compile-file| and |load| are good enough for many
things, also for non-core code. Is the above linked code really too much
of a module/user space feature to be included in the core?

Which tools are handling paths how?

  - Perl does it pretty much exactly as shown in
    <http://scheme.ch/gambit/experimental/paths/paths.scm>

<!-- end list -->

  - What about other interpreted languages?

<!-- end list -->

  - How does path resolving in gcc for -lfoo etc. exactly work? compare.

<!-- end list -->

  - other tools?
