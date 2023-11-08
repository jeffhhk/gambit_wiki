\_\_NOTOC\_\_ \_\_NOEDITSECTION\_\_ The Gambit [ source code
repository](Source code repository.md) is managed with the
[git](http://git.or.cz/) distributed version control system.

Each developer uses a local copy of the central repository. The copies
can be edited independently using any tool (editors, utility programs,
etc). When a developer completes making a set of related changes they
are recorded as a *commit*. The developer can submit these changes to
the Gambit maintainers as a *patch*. If the patch is accepted then it
will be applied to the repository by the Gambit maintainers and will
become part of the central repository.

This page only gives the main procedures and git commands that are
needed to manage the local copy of the central repository. Please read
the [git
tutorial](http://www.kernel.org/pub/software/scm/git/docs/gittutorial.html)
for typical usage instructions.

#### Installing git

Git was created for the development of the Linux kernel, and is
sometimes preinstalled on Linux distributions. Prebuilt distributions of
git and a source code distribution are available
[here](http://git.or.cz/#download). Please follow the installation
instructions on the [git web site](http://git.or.cz/).

To simplify usage of git the git configuration file **$HOME/.gitconfig**
should be created and contain at least your username. There are many
settings that can be specified such as the editor to use for entering
the comment attached to a commit. For example, to use emacs:

    [user]
            name = Marc Feeley
            email = feeley@iro.umontreal.ca
    [core]
            editor = emacs -nw
    [color]
            ui = auto

#### Obtaining a local copy of the Gambit source code

The best way to obtain a local copy of the Gambit source code is to
download and unpack a recent source code release. You need the
**developer** version, which has a **-devel** in the tarball name.
Normally the most recent source code release should be downloaded and
you need at least v4.3.2 (the first version to support git). After that
you can use the **make update** command to update your local copy with
all the changes committed to the central source code repository.

Get the most recent tarball from the [main Gambit
page](http://dynamo.iro.umontreal.ca/~gambit/wiki/index.php/Main_Page).

    wget http://www.iro.umontreal.ca/~gambit/download/gambit/v4.3/source/gambc-v4_3_2-devel.tgz
    tar zxf gambc-v4_3_2-devel.tgz
    mv gambc-v4_3_2-devel gambit

Note that in anticipation of making changes to the source code the
directory has been renamed to **gambit** to avoid thinking it is a
pristine copy of a particular release.

The system should then be prepared for modifications of the Scheme
source code by creating a compiler for bootstrapping:

    cd gambit
    ./configure --enable-single-host
    make bootstrap

This will create the **gsc-comp** compiler, which is executed when a
Scheme source code file in the Gambit system must be compiled following
a change.

#### Synchronizing with the central source code repository

The local source code files can then be updated to contain the latest
commits on the central source code repository with the command:

    make update

This will call the **git fetch** and **git checkout** commands in a
loop, so that the Gambit compiler for each release between the local
copy's release and the most recent release are built in succession. This
is necessary because the Gambit system is bootstrapped with itself, and
to compile the runtime system for a specific version the Gambit compiler
for that version must be used. If for some reason you need to do this
manually the following procedure should be used. Assuming the local copy
is currently at release v1.2.3 and that v1.2.5 is the most recent
release, do this:

    git checkout v1.2.4-bootstrap
    make bootstrap
    git checkout v1.2.4
    make bootclean bootstrap
    git checkout v1.2.5-bootstrap
    make bootstrap
    git checkout v1.2.5
    make bootclean bootstrap
    git checkout master
    make bootstrap

Note that the latest source code may be unstable. It is wise to check
that it passes basic consistency checks by running the command **make
check**.

The local source code files can be updated to a specific version with
the **git checkout** command.

    git checkout v4.2.1

After this command it is not possible to build Gambit with a **make**
because the local **gsc-comp** is not the appropriate version to compile
the runtime system. One way around this problem is to download the
release for that version in a different directory, do a **make
bootstrap** in that directory, and then copy **gsc-comp** back here.

#### Creating a commit

A source code change may simply be modifications of existing files,
deletions, and addition of new files. If a new file has been created git
should be informed by entering:

    git add newfile

After making a set of related changes it is customary to review which
files have changed by executing the command:

    make status

If all checks out, a commit is created with the command:

    make commit

This command will pop up an editor to compose a comment describing the
nature of the patch. Please use a short but descriptive comment on the
first line of text entered.

To get a list of the commits (starting with the latest, i.e. the
**HEAD**) use:

    make log

#### Contributing a patch

A patch file can be created using the **git diff** command. To include
only the most recent commit use:

    git format-patch -1

A range of patches can be specified like this:

    git format-patch -4 HEAD~3

The patch file(s) can then be submitted to the Gambit maintainers by
sending the file by email to **gambit@iro.umontreal.ca**. For example:

    mail -s "[PATCH] ,b command fix" gambit@iro.umontreal.ca < my-patch

#### Releasing a new version of Gambit

This section contains some notes for the Gambit maintainers. It explains
the steps to follow to release a new version of Gambit. **The steps must
be followed carefully or you may end up with a wedged system\!**

##### Manual procedure

Assume the latest release of Gambit is v1.2.3 and the new release will
be v4.5.6. Because Gambit is bootstrapped with itself, releasing a new
version of Gambit is a two step process. The Gambit compiler must first
be modified so that it generates C code which references the
**gambit.h** file version v4.5.6. Then the **gambit.h** file,
**configure.ac** file, and other files with an embedded version number
must be updated so they refer to version v4.5.6.

**1. Creating a Gambit compiler for version v4.5.6**

Make sure you have a working bootstrap compiler (**gsc-comp**) and save
a copy just in case:

    make check
    make bootstrap
    cp gsc-comp gsc-comp.old

Now modify the definition of **compiler-version** in the file
**gsc/\_parms.scm** so that it refers to v4.5.6 (using a single integer
encoding of the version number):

    (define (compiler-version) 405006) ;; 100000*major + 1000*minor + revision

Now create the new bootstrap compiler and commit the change with an
easily identifiable comment:

    make bootstrap
    git commit -a -m "[COMPILER CHANGES NEEDED FOR v4.5.6] Changed version in compiler"
    git tag v4.5.6-bootstrap

Note that at this very point the new compiler will not pass the tests
because the runtime still expects the old version number.

**2. Upgrading the runtime files from version v1.2.3 to v4.5.6**

For the C files generated from Scheme files the version numbers can
easily be upgraded by compiling them from scratch using the new Gambit
compiler. This will be done automatically by a **make** after a **make
bootclean**. The other changes to the files are made by running the
script **misc/changev** (this must be done first because it changes the
file **configure.ac** which produces the **configure** script which must
be run again).

    misc/changev 102003 405006
    make bootclean
    ./configure
    make
    make check   # will fail test5 because version numbers have changed
    mv tests/mix.c tests/test5.ok
    make check   # this time all tests should pass
    git commit -a -m "[RUNTIME CHANGES NEEDED FOR v4.5.6] Changed version of runtime using misc/changev"
    git tag v4.5.6

**3. Creating prebuilt installers for version v4.5.6**

To create prebuilt installers for Mac OS and Windows you need a Mac with
Xcode and Parallels workstation with Windows and the MinGW and Microsoft
Visual C Express environments. The following command will build all
variants of the installers (it takes about 2 hours in total):

    make release

If no errors were reported then you can upload the new release to the
Gambit repository using:

    make publish-release

This will automatically send an announcement on the Gambit mailing list.
You must manually update the main page of the Gambit Wiki so that it
refers to the latest version.

##### Automated procedure

Step 1 and 2 in the above instructions are executed with the
**change-version** make target:

    make NEW_VERSION=v4.5.6 change-version

Alternatively, the **new-revision**, **new-minor**, or **new-major**
make targets can be used to compute the new version number from the
current version by incrementing one of the version number fields:

    make new-revision

#### Troubleshooting

A user was having trouble using trouble building an up-to-date version
of Gambit starting from v4.3.2. **make update** was failing. Marc Feeley
responded with an explanation and a solution.

The solution is to download the latest version of Gambit and use that to
run the **make update** in the git repository.

Here's the
[reply](http://article.gmane.org/gmane.lisp.scheme.gambit/4399):

> Let me explain the problem. If you're only interested in a solution:
> get v4.6.0 and a make update from that.

> Gambit is a self-hosting compiler. That means that it is bootstrapped
> using itself. Most of the system is implemented in Scheme (not just
> the libraries, but the compiler itself). So when a change is made in
> the compiler's source code, it must be compiled with the current
> version of the compiler. Roughly speaking, version 2 must be compiled
> with version 1. Then when changes are made (to get the source code of
> version 3), then version 3 must be compiled with version 2. And so on.
> When you "make update" the build process executes the chain of self
> compilations that led to the most recent version of Gambit. In other
> words, if the most recent Gambit is v4 and you currently have v1
> installed, then the "make update" will: compile the v2 source with the
> v1 executable to get the v2 executable, then it will compile the v3
> source with the v2 executable to get the v3 executable, and finally
> compile the v4 source with the v3 executable to get the v4 executable.
> This chain must be followed because features that are used in the
> source code of the most recent version of the compiler may have
> appeared (i.e. were implemented) in an intermediate version of the
> compiler.

> This self dependency is not easy to handle in the build process and
> makefiles, and currently there is a bug. In your particular case,
> Gambit v4.3.2 does not support the same command line options as more
> recent version of Gambit. So somewhere in this chain of commits, the
> makefiles were not consistent with the then current compiler. But
> because the commits can't be "taken back" the self compilation
> sequence that "make update" executes has some version transitions that
> will forever be broken. I haven't looked into ways (automatic tool, or
> better version changing habits) to avoid this problem in the future.

[Category: Development](Category: Development.md)
