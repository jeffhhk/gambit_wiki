Using Andre van Tonder's [syntax-case & library
system](http://www.het.brown.edu/people/andre/macros) on Gambit

The latest tarball is available at:

[r6gambit.tar.gz](http://smyles.com/projects/r6gambit.tar.gz)

The project homepage is now

<http://smyles.com/projects/r6gambit>

The darcs2 repository is available at

<http://smyles.com/projects/r6gambit/darcs>

General Discussion mailing list:

<http://smyles.com/mailman/listinfo/r6gambit>

The err5rs records implementation can be independently used for your
projects. Just copy the files:

srfi-99.scm

Questions:

` * Can you add the declarations (declare (standard-binding)(extended-bindings)(block)) to the top of the file?  That would decrease the size of the .c file passed to gcc.`  
` *`*`I've``   ``added``   ``what``   ``you``   ``asked``   ``in`` 
 ``the``   ``latest``   ``version.`*` `

I'm having trouble getting things installed. Could you please add a
couple of paragraphs of user guide here? I followed the installation
instructions (modifying my .gambcini); then I did gsc -i compile. At
this point, everything got compiled and copied to my \~/.gambit/lib
directory. I then went to that directory, and did gsc -i, followed by
(load "r6rs"). No joy, it did not recognize the library or import
special forms. Any suggestions? Thanks\! (This is a very cool project,
though, and I'm sure I'm just doing something dumb.) Oh yes: Gambit-C
4.4.1 on OS X Leopard.

''Sorry for the late reply. I just noticed your message now. To answer
your question. There is a function called (r6rs) which will load the
base library for you. Then you can use the (program ...) special form.
Also, when you use the repl it is still the gambit repl. You can't use
import or library. I sometimes lurk on \#gambit (I'm atsmyles) irc
channel so if you see me we can work through it.

[Category: Code](Category:_Code.md) [Category:
Languages](Category:_Languages.md)
