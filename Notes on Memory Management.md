*This content is currently just some notes by me without any connection
to the Gambit-C project - they may not be completely accurate.
--[PhilDawes](User:PhilDawes "wikilink") 17:04, 9 April 2007 (EDT)*

## Creating Gambit Scheme objects from C

Gambit scheme objects have the type \_\_\_SCMOBJ in c. 'gambit.h'
contains functions to create scheme objects. Most of the C types have
conversion functions e.g. \_\_\_U32\_to\_SCMOBJ,
\_\_\_UTF\_8STRING\_to\_SCMOBJ. Lists/cons cells can be created using
the '\_\_\_make\_pair' constructor. The end of the list is a \_\_\_NUL.

To create homogenious vectors (e.g S32vector, U8vector) useful for
passing and manipulating binary data from within scheme, you need to use
the '\_\_\_alloc\_scmobj' function passing a the subtype tag (e.g.
\_\_\_sS32VECTOR, \_\_\_sU8VECTOR).

Before using these functions to create scheme objects from C code, you
need to understand reference counting and the various allocation
strategies - see below:

## Reference Counting

Gambit uses reference counting to keep track of references to scheme
objects from C code. If the reference count drops to 0 \*and\* the
object is not reachable by any other scheme objects then the object will
be collected by the garbage collector. This means that objects passed
back to scheme code must have their references decremented to 0 or
they'll be leaked. E.g. this function must release the scheme string
object before returning it to it's scheme caller.

    <nowiki>
    (define test-return-string
        (c-lambda () scheme-object
    #<<c-lambda-end
       ___SCMOBJ s;
       char *p = "hello";
       ___UTF_8STRING_to_SCMOBJ(p,&s,0);  // s has a refcount of 1
       ___release_scmobj(s);        // s has refcount of 0
       ___result = s;
    c-lambda-end
    ))
    </nowiki>

Note that the reachability also applies to other scheme objects created
in C code. E.g. if you create a string and then put it in a list (e.g.
using \_\_\_make\_pair()) then you must release it or it will remain
uncollected even if the pair is released and subsequently goes out of
scope. Here's some code which creates a list of strings:

    <nowiki>
    (define test-return-list
        (c-lambda () scheme-object
    #<<c-lambda-end
         ___SCMOBJ s,list,tmp;
         int i;
         char *p = "hello";
         ___EXT(___UTF_8STRING_to_SCMOBJ)(p,&s,___STILL);
     
         list = ___NUL;
     
         for (i=0; i<1000000; i++){
              tmp  = ___EXT(___make_pair) (s,list, ___STILL);
              ___EXT(___release_scmobj) (list);
              list = tmp;
         }
     
         /* deref remaining ref counts */
         ___EXT(___release_scmobj) (list);
         ___EXT(___release_scmobj) (s);
     
         ___result = list;
     
    c-lambda-end
    ))
    </nowiki>

Personally, I found keeping track of refcounting difficult when writing
C code. The best way I found of keeping on top of it was to create the
following function which allows you to look at the reference count of an
object:

    <nowiki>
    int _peek_refcount(___SCMOBJ s){
    #define ___STILL_BODY_OFS 6
    #define ___STILL_REFCOUNT_OFS 1
        return ___UNTAG(s)[___BODY_OFS - ___STILL_BODY_OFS + 
    ___STILL_REFCOUNT_OFS];
    }
    </nowiki>

I then littered the code with assert calls. e.g:

    assert(1 == _peek_refcount(myobj));

## Permanent, Still and Moveable objects

Gambit objects created from within scheme code can be moved around
memory by the garbage collector during a collect cycle. This is
transparent for scheme code, but would cause problems if you held a
reference to that memory from C. Thus gambit provides different
management strategies. From 'mem.c':

    <nowiki>
     * Memory allocated Scheme objects can be allocated using one of three
     * allocation strategies:
     * 
     *    Permanently allocated:
     *      These objects, called 'permanent objects' for short, are never
     *      moved or reclaimed, and all pointers to memory allocated
     *      objects they contain must point to permanent objects.  As a
     *      consequence, the GC does not have to scan permanent objects.
     *      Permanent objects can be allocated on the C heap, but they are
     *      typically allocated in C global variables and structures that
     *      are set up when the program starts up or when a module is
     *      dynamically loaded.
     * 
     *    Still dynamically allocated:
     *      These objects, called 'still objects' for short, are allocated
     *      on the C heap.  Still objects are never moved but they can be
     *      reclaimed by the GC.  A mark-and-sweep GC is used to
     *      garbage-collect still objects.
     * 
     *    Movable dynamically allocated:
     *      These objects, called 'movable objects' for short, are allocated
     *      in an area of memory that is managed by a compacting GC.  The GC
     *      can move and reclaim movable objects.
    </nowiki>

Note that the garbage collector may be invoked by the scheme runtime
within gambit C calls (e.g. during calls to
\_\_\_UTF\_8STRING\_to\_SCMOBJ) so you need to be aware of this
possibility.

By way of example, here's some code that creates a 'still' u8vector and
passes it to the (scheme) caller. The memory associated with the
u8vector will still be collected by the GC when it goes out of scope,
but it won't be moved.

    <nowiki>
    (define create-still-u8vector
       (c-lambda (int) scheme-object
    #<<c-lambda-end
       ___result = ___alloc_scmobj(___sU8VECTOR, ___arg1, ___STILL);
       ___EXT(___release_scmobj) (___result);
    c-lambda-end
    ))
    </nowiki>

For more information, check out the extensive comments in mem.c.

\--[PhilDawes](User:PhilDawes "wikilink") 16:54, 9 April 2007 (EDT)
