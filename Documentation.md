## User Documentation

There is a user manual for Gambit in
[HTML](http://www.iro.umontreal.ca/~gambit/doc/gambit.html) and
[PDF](http://www.iro.umontreal.ca/~gambit/doc/gambit.pdf) formats.

See the [Scheme](Scheme.md) page for documentation on Scheme in
general.

## Prebuilt Distributions

Some prebuilt distributions are [ available](Distributions.md).

## Building Gambit from Source

To build Gambit from source, one must first run the configure script,
which has a number of important [
options](Configure_script_options.md).

There are various [ make targets](Make_targets.md) of which you
should be aware.

## Packages, modules, libraries and driving systems

See the [Packages](Packages.md) page.

### SRFI support

The Scheme community has accumulated a set of common libraries and
conventions called Scheme Requests for Implementation, publicly
collected on [srfi.schemers.org](http://srfi.schemers.org/).

Gambit [ natively implements](SRFI:s.md) a number of SRFIs.
[Black Hole](Black_Hole.md) includes several popular SRFIs not
natively supported, while still others may be downloaded from
[Snow](http://snow.iro.umontreal.ca/?listcat=srfi).

## Editor/IDE Support

Gambit provides a powerful development environment through
[Emacs](http://www.iro.umontreal.ca/~gambit/doc/gambit.html#Emacs-interface).

For those running MS Windows you can [download Emacs
here](http://ftp.gnu.org/pub/gnu/emacs/windows/). You may want to read
the [Emacs FAQ for
Windows](http://www.gnu.org/software/emacs/windows/ntemacs.html) before
customizing Emacs for use with Gambit. In order to use Emacs' inferior
scheme mode on Windows, you may need to run gsi/gsc in raw stdin/stdout
mode (see section "Emacs interface" in the manual for details).

Editors such as [Eclipse](http://www.eclipse.org/) with the
[SchemeWay](http://schemeway.sourceforge.net/) extension, or just about
any text editor with Scheme syntax highlighting, such as [VIM or
GVIM](http://www.vim.org), may prove valuable as well.

## Termite

[Termite](http://code.google.com/p/termite/) is an Erlang-like
distributed programming system written in Scheme.

Distributed computing hot right now, and Termite has been noticed in
blogs and elsewhere.

Termite depends on specific features of Gambit, and at one time or
another the Termite source code has been distributed with Gambit, so we
point to the Termite web site from here.

## Tutorials

[A Tour of Scheme in Gambit](A_Tour_of_Scheme_in_Gambit.md)
gives a general introduction to Scheme and Gambit to people with
experiences of general programming languages. Available in
[PDF](media:A_Tour_of_Scheme_in_Gambit.pdf.md), [Open
Office](media:A_Tour_of_Scheme_in_Gambit.odf.md) and [Microsoft
Word](media:A_Tour_of_Scheme_in_Gambit.doc.md) formats.

## Internals Documentation

[Design guide](Design_guide.md)

People who want to [ contribute](How_to_Contribute.md) to Gambit
development will need to learn something about how the Gambit-C runtime
and compiler are organized. While we intend that source code
documentation be included in the source itself (currently there is very
little documentation), we intend that descriptions of program design or
algorithms used in the runtime and compiler could be included on the
[Internal Documentation](Internal_Documentation.md) page.

## Working with External Libraries

With Gambit's C FFI (Foreign Function Interface), one can easily use
standard C and C++ libraries with your code; this wiki has some examples
of and practices on using Gambit with external libraries. See [Using
Gambit with External
Libraries](Using_Gambit_with_External_Libraries.md).

## Debugging

See the [Debugging](Debugging.md) page.

## More

The [mailing list
archive](http://mailman.iro.umontreal.ca/pipermail/gambit-list) covers
many topics at depth. You can make Google search queries limited to the
mailing list by clicking "advanced settings" on www.google.com.

[Dumping Grounds](Dumping_Grounds.md)

[Compiling Gambit software for different target
environments](Compiling_Gambit_software_for_different_target_environments.md)

[Wish list](Wish_list.md)

[External links](External_links.md)
