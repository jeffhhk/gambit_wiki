Comparison of Watcom to mingw-prebuilt for Gambit-v4.1.2. Note that the
benchmarks supplied with the distribution were not intended to be run
standalone so I have bypassed the prefixes. Clearly, GCC is
significantly faster than Watcom.

    gsi puzzle.scm  ; watcom
    (time (start))
        4567 ms real time
        4567 ms cpu time (4567 user, 0 system)
        61 collections accounting for 230 ms real time (230 user, 0 system)
        30940760 bytes allocated
        no minor faults
        no major faults
    
    Microsoft Windows XP [Version 5.1.2600]
    (C) Copyright 1985-2001 Microsoft Corp.
    
    C:\gt>gsi puzzle ; (mingw)
    (time (start))
        2995 ms real time
        2994 ms cpu time (2994 user, 0 system)
        61 collections accounting for 220 ms real time (220 user, 0 system)
        30940760 bytes allocated
        no minor faults
        no major faults
    
    C:\gt>gsi graphs ;mingw
    (time (run 5))
        1161 ms real time
        1162 ms cpu time (1162 user, 0 system)
        98 collections accounting for 380 ms real time (381 user, 0 system)
        41479048 bytes allocated
    
    C:\gt>gsi graphs.scm ;watcom
    (time (run 5))
        1593 ms real time
        1592 ms cpu time (1592 user, 0 system)
        98 collections accounting for 541 ms real time (541 user, 0 system)
        41479896 bytes allocated
