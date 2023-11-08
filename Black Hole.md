**Black Hole** (“BH”) is a module system abstraction for
[Gambit](Documentation.md) providing recurring module file
dependency compilation and loading, including export of macros, for
regular R5RS Scheme.

Read further in the Introduction section of the documentation, found
below.

## Download

Its latest version is avilable via GIT, and can be downloaded using
[GIT](http://www.git-scm.org) by

  -   
    git clone git://github.com/pereckerdal/blackhole.git

There are also some libraries for Black Hole at

  -   
    git clone git://github.com/pereckerdal/blackhole-libs.git

or non-automatically, probably very old copy
[here](media:Black%20Hole.zip.md).

Make sure you put the blackhole-libs into a directory called "std" in
\~\~/lib/modules

## Documentation

Core documentation (2009): [PDF](media:Black%20Hole%20Core.pdf.md),
[OpenOffice](media:Black%20Hole%20Core.odf.md) and [Microsoft
Word](media:Black%20Hole%20Core.doc.md)

Bundled libraries documentation (2009):
[PDF](media:Black%20Hole%20Bundled%20libraries.pdf.md),
[OpenOffice](media:Black%20Hole%20Bundled%20libraries.odf.md) and
[Microsoft Word](media:Black%20Hole%20Bundled%20libraries.doc.md)

(The tutorial for the alpha release is no longer available. The old site
<http://mwaza.dyndns.org/apps/files/bh-tutorial.html> is now offline.)

There is a setup guide by Mikael for Black Hole from nov. 2012:
<https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-November/006188.html>

## Bundled libraries

BH is bundled with a set of general purpose libraries, including:

  - SRFI 1 (list processing), 13 (string), 14 (character sets), 16
    (case-lambda support), 19 (time data types and procedures), 95
    (sorting) and MORE (check the repository)
  - pregexp, hash digestion, base64 handling, UUID generator
  - HTTP client, server, URI and session variable handling
  - XML\<-\>SXML routines
  - FIFO queue, mailbox, weight balanced tree, erlang-style list matcher
  - String, u8vector, list, exception handling helper libraries
  - let-optionals support

BH's core is completely independent of the bundled libraries, and can be
separated from them without any modifications.

See the documentation for more information.

## Additional libraries

Termite: A version suited for usage in Black Hole is found at GitHub,
<http://github.com/pereckerdal/termite/tree/master>.

Blackhole-web: More extensions to BlackHole for use as a web framework
are here: <http://github.com/jonnay/Blackhole-web>. Currently it is just
a copy of SSAX-SXML, but more are planned. Check the Blackhole-web.org
file for more information on what is to be done, and what is planned.
