\_\_NOTOC\_\_ \_\_NOEDITSECTION\_\_ The development of Gambit was
started in 1989 by [Marc Feeley](http://www.iro.umontreal.ca/~feeley/).
In order to maintain tight control over the quality of the
implementation, few other developers have been permitted to contribute
directly to the system. Now that Gambit has matured and that the core
system is unlikely to change much, we believe that it is important to
open the development process to invite contributions by the Gambit user
community.

Contributions are sought in at least three areas:

  - **Gambit's source code**
      - Locate and fix bugs
      - [ Suggest new features ](Wish_list "wikilink")
      - Implement new features
      - [ Port the system to new platforms ](Installer "wikilink")
      - Improve the performance of the system

<!-- end list -->

  - **Gambit's documentation**
      - Improve the user manual
      - Improve the Gambit web site
      - Write tutorials

<!-- end list -->

  - **Gambit's packages**
      - Create and maintain Gambit-specific packages
      - Port packages from other Scheme implementations

## Contributing to Gambit's source code

If you discover a problem while working with Gambit we advise that you
report this on the [issue tracker on
github](https://github.com/gambit/gambit/issues). You should do this
even if you know how to fix or work around the problem, so that others
can search the issue database to be aware of its existence and to see
the status of the issue. Suggestions for new features can also be
submitted to the github issue tracking system.

The latest sources are available in the [ source code
repository](Source_code_repository "wikilink"). It can be accessed using
the [git](http://git.or.cz/) distributed version control system. The
details are explained in the [ source code repository
instructions](Contributing_Patches_to_Gambit_Source_Code "wikilink").
With git you get a local copy of the source code repository which you
can use to develop a *patch* that fixes a bug or implements a new
feature. If you feel confident that the patch is correct and useful to
others, you can then submit this patch to the Gambit maintainers. The
patch will be reviewed to see how well it fits with the goals and
philosophy of Gambit. If the patch is accepted then it will be applied
to the source code repository and included in the official distribution
of Gambit. If you are planning to make a substantial change to the
source code it is best to discuss your plans on the [Gambit mailing
list](https://mailman.iro.umontreal.ca/cgi-bin/mailman/listinfo/gambit-list)
beforehand to improve the likelihood it will be accepted when it is
submitted.

## Contributing to Gambit's documentation

The Gambit manual's source is bundled with the source code. It is in the
**doc** subdirectory of the Gambit source code distribution. Patches to
the manual can be contributed like source code contributions as
explained above.

The Gambit web site is organized as a Wiki, powered by
[MediaWiki](http://www.mediawiki.org/) (the same system used by
Wikipedia). The core pages and the navigation sidebar can only be edited
by the Gambit maintainers. Pages which have an **edit** link or tab can
be edited by anyone using these [editing
rules](http://meta.wikimedia.org/wiki/MediaWiki_User%27s_Guide:_Editing_overview).

Suggestions for improving the Gambit web site or for adding new pages
that can be edited should be discussed on the [Gambit mailing
list](https://mailman.iro.umontreal.ca/cgi-bin/mailman/listinfo/gambit-list).

## Contributing to Gambit's packages

The procedure for contributing packages is still under development.
Please stay tuned.

[Category: Internals](Category:_Internals "wikilink")
