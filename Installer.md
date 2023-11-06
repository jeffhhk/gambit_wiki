This (still empty) page describes the requirements to help rewriting
parts of the installation procedure of Gambit in order to make it easier
to install on various systems.

# Objectives

  - Have a standard process with portable defaults

# Constraints

  - Allow several versions of gambit to be installed concurrently on a
    single host
  - Avoid name clashes with other software (i.e. gsi/gsc is also default
    name of some ghostscript interpreters)
  - Do not mess up with host's hierachy

# Examples

## Default

` ./configure && make && make install`

installs in

` /usr/local/bin/gs{i,c}-$VERSION`  
` /usr/local/lib/gambit/$VERSION/syntax-case.scm`  
` /usr/local/share/doc/gambit/$VERSION`

and so on

## Parametrized

` ./configure --prefix=/tmp/test --bindir=/bin --gsi=gambi-VERSION --gsc=gambc  && make && make install`

gives

` /bin/gambi-4.2.9`  
` /bin/gambc`  
` /tmp/test/lib/gambit/4.2.9/syntax-case.scm`  
` /tmp/test/share/doc/gambit/4.2.9/gambit-doc.html`  
` ...`

# Add-ons

  - Provide some subsidiary package with everything that is lacking from
    the original source code
      - List manipulation: filter, fold-left, etc
      - Generators: iota, etc
      - Sorts: list-sort, vector-sort, etc
      - Editors tools: gambit.el for emacs, and comparable plugins for
        vim, eclipse, etc

<!-- end list -->

  - References: GHC's Prelude module provides a minimal yet reasonable
    set of features that we want...

# Join the dream team

  - fetch the 4.2.8 source code (our reference for this refactoring)
  - with mercurial, join the task force's branch at the address \*branch
    to be created somewhere\*
  - hack, hack, hack
  - test installation
  - test \*again\*
  - submit patch
  - ???
  - Profit\!

[Category: Installing](Category:_Installing "wikilink") [Category:
Internals](Category:_Internals "wikilink")
