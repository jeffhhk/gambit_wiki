    ;;; -*- Scheme -*-   wmaker.scm  ..bm2008-jan-27
    ;;; prints OpenWatcom 1.7a makefile for Gambit-C v4.1.2
    ;;; - unpack into c:/Gambit-W ..(change name later as needed)
    ;;; - copy wmaker.scm to c:/Gambit-W/misc
    ;;; - gsi wmaker
    ;;; - wmake -f Makefile.wcc
    
    (define (print-title)
      (display "# OpenWatcom 1.7a Makefile for Gambit-C v4.1.2\n\n"))
    
    ;; matches targets to compilation rules given list of associations,pathname
    (define (wcc-target-rule pair-list folder)
      ;; pair-list .. associated targets and compile rule
      ;; folder .. pathname prefix
      (let ([s (length pair-list)])
        (let loop ([i 0])
          (if (< i s)
          (begin
            (display 
             (string-append
              folder (list-ref pair-list i) 
              ".obj: " folder (list-ref pair-list i) ".c\n"
              "\t" (list-ref pair-list (+ i 1))
              " -bt=$(NTO) $*.c -fo=$*.obj\n\n"))
            (newline)
            (loop (+ i 2)))))))
    
    
    (define (gambitc-dir)
      ;; modified name avoids clash with possible c:/Gambit-C/
      (display "GAMBCDIR=\"C:/Gambit-W")
      ;;trailing slashes are seen in Gambit batch files so..
      (let loop1 ((i 0))
        (if (< i 75)
        (begin
          (display (string #\/ #\. ))
          (loop1 (+ i 1)))))
      (display (string #\"))
      (newline))
    
    (define (dll-enable)
      ;; not using 'nt_dll' yet
      (display "\nNTO=nt \nNTDLL=nt\n"))
    
    (define (folders)
      (display 
       (string-append
        "INCDIRS = -Ic:\\WATCOM\\H -Ic:\\WATCOM\\H\\NT -I..\\include\n"
        "BINDIR  = ..\\bin\n"
        "GSCDIR  = ..\\gsc\n"
        "GSIDIR  = ..\\gsi\n"
        "LIBDIR  = ..\\lib\n\n"
        "LIBNAME = gambc.lib\n"
        "WINLIBS = ws2_32.lib\n"
        "WINGUILIBS = $(WINLIBS) kernel32.lib user32.lib gdi32.lib\n\n")))
    
    (define (comp-gen)
      ;; copied from "misc/openwatcom.bat" 
      ;; -3r for 386, was changed to -5r for pentium
      (display
       (string-append
        "\nCOMP_GEN=wcc386 -w0 -zp4 -zq -obetir -bm -5r -mf &\n"
        "\t$(INCDIRS) &\n"
        "\t-D___GAMBCDIR=$(GAMBCDIR) -D___SYS_TYPE_CPU=\"i686\" &\n"
        "\t-D___SYS_TYPE_VENDOR=\"pc\" -D___SYS_TYPE_OS=\"openwatcom\" &\n"
        "\t-D___IMPORTED_ID_SUFFIX=\"_\" \n\n"
        "COMP_LIB_MH\t=$(COMP_GEN) -D___LIBRARY\n"
        "COMP_LIB_PR_MH\t=$(COMP_LIB_MH) -D___PRIMAL\n"
        "COMP_LIB\t=$(COMP_LIB_MH) -D___SINGLE_HOST\n"
        "COMP_LIB_PR\t=$(COMP_LIB_PR_MH) -D___SINGLE_HOST\n"
        "COMP_APP\t=$(COMP_GEN) -D___SINGLE_HOST\n\n")))
    
    (define (target-all)
      ;; default rule to compile library, gsi, and gsc
      (display 
       (string-append 
        "all: $(LIBDIR)\\gambc.lib $(BINDIR)\\gsi.exe "
        "$(BINDIR)\\gsc.exe\n\n")))
    
    ;; --------------- gsi section --------------------------
    (define gsi-files
      (list "_gsilib" "_gambcgsi" "_gsi" "_gsi_" ))
    
    (define (gsi-target-list gsi-files)
      ;; tell wcc the dependents to compile and the rule for executable
      (define (gsi-target-item  name )
        (display (string-append "GSIOBJS += $(GSIDIR)\\" name ".obj\n")))
      (display "GSIOBJS=\n")
      (for-each gsi-target-item gsi-files)
      (newline)
      (display 
       (string-append
        "\n$(BINDIR)\\gsi.exe: $(GSIOBJS)\n"
        "\twlink name $*.exe op quiet op stack=16384 system nt &\n"
        "\t\tfile {$(GSIOBJS)} &\n"
        "\t\tlibrary { $(LIBDIR)\\$(LIBNAME) $(WINLIBS)}\n\n")))
    
    ;; -- target-name ..... rule ---
    (define gsi-pairs
      (list "_gsilib"    "$(COMP_LIB)"
        "_gambcgsi"  "$(COMP_LIB)"
        "_gsi"       "$(COMP_APP)"
        "_gsi_"      "$(COMP_APP)"))
    
    ;; --------------- gsc section --------------------------
    
    (define gsc-pairs
      (list 
       "_host"     "$(COMP_LIB)"
       "_utils"    "$(COMP_LIB)"
       "_source"   "$(COMP_LIB)"
       "_parms"    "$(COMP_LIB)"
       "_env"      "$(COMP_LIB)"
       "_ptree1"   "$(COMP_LIB)"
       "_ptree2"   "$(COMP_LIB)"
       "_gvm"      "$(COMP_LIB)"
       "_back"     "$(COMP_LIB)"
       "_front"    "$(COMP_LIB)"
       "_prims"    "$(COMP_LIB)"
       "_t-c-1"    "$(COMP_LIB)"
       "_t-c-2"    "$(COMP_LIB)"
       "_t-c-3"    "$(COMP_LIB)"
       "_gsclib"   "$(COMP_LIB)"
       "_gambcgsc" "$(COMP_LIB)" 
       "_gsc"      "$(COMP_APP)"
       "_gsc_"     "$(COMP_APP)"
    ))
    
    (define (gsc-target-list gsc-pairs)
    
      ;; tell wcc the dependents to compile and the rule for executable
      (define (gsc-target-item  name )
        (display (string-append "GSCOBJS += $(GSCDIR)\\" name ".obj\n")))
    
      (display "GSCOBJS=\n")
      ;;(for-each gsc-target-item gsc-files)
      
      (let ([s (length gsc-pairs)])
        (let loop ([i 0])
          (if (< i s)
          (begin
            (gsc-target-item (list-ref gsc-pairs i))
            (loop (+ i 2))))))
      
      (display 
       (string-append
        "\n$(BINDIR)\\gsc.exe: $(GSCOBJS)\n"
        "\twlink name $*.exe op quiet op stack=16384 system nt &\n"
        "\t\tfile {$(GSCOBJS)} &\n"
        "\t\tlibrary { $(LIBDIR)\\$(LIBNAME) $(WINLIBS)}\n\n")))
    
    ;; --------------- lib section --------------------------
    (define lib-pairs
      (list 
     "main"       "$(COMP_LIB_PR)"
     "setup"      "$(COMP_LIB_PR)"
     "mem"        "$(COMP_LIB_PR)"
     "os"         "$(COMP_LIB_PR)"
     "os_base"    "$(COMP_LIB_PR)"
     "os_time"    "$(COMP_LIB_PR)"
     "os_shell"   "$(COMP_LIB_PR)"
     "os_files"   "$(COMP_LIB_PR)"
     "os_dyn"     "$(COMP_LIB_PR)"
     "os_tty"     "$(COMP_LIB_PR)"
     "os_io"      "$(COMP_LIB_PR)"
     "c_intf"     "$(COMP_LIB_PR)"
     "_kernel"    "$(COMP_LIB_PR)"
     "_system"    "$(COMP_LIB_PR)"
     "_num"       "$(COMP_LIB_PR_MH)"
     "_std"       "$(COMP_LIB_PR)"
     "_eval"      "$(COMP_LIB_PR)"
     "_io"        "$(COMP_LIB_PR_MH)"
     "_nonstd"    "$(COMP_LIB_PR)"
     "_thread"    "$(COMP_LIB_PR)"
     "_repl"      "$(COMP_LIB_PR)"
     "_gambc"     "$(COMP_LIB_PR)"
    
    ))
    
    (define (lib-target-list lib-pairs)
      
      ;; tell wcc the dependents to compile and the rule for executable
      (define (lib-target-item  name )
        (display (string-append "LIBOBJS += $(LIBDIR)\\" name ".obj\n")))
      
      (display "LIBOBJS=\n")
      ;;(for-each gsc-target-item gsc-files)
      
      (let ([s (length lib-pairs)])
        (let loop ([i 0])
          (if (< i s)
          (begin
            (lib-target-item (list-ref lib-pairs i))
            (loop (+ i 2))))))
      
      (display 
       (string-append
        "\n$(LIBDIR)\\gambc.lib: $(LIBOBJS)\n"
        "\twlib /q /n /t $*.lib $(LIBOBJS)\n\n")))
    
    
    (define (prepare-header)
      (let ([ OUT #f] [INP #f] [inchar #f] [hatch (integer->char 35)] )
        (set! OUT (open-output-file "../include/gambit.h"))
        (set! INP (open-input-file  "../include/gambit.h.in"))
    
        (display
         (string-append 
          (string hatch) " ifndef ___VOIDSTAR_WIDTH\n"
          (string hatch) " define ___VOIDSTAR_WIDTH ___LONG_WIDTH\n"
          (string hatch) " endif\n"
          (string hatch) " ifndef ___MAX_CHR\n"
          (string hatch) " define ___MAX_CHR 0x10ffff\n"
          (string hatch) " endif\n\n")
         OUT)
        (let reader ()
          (set! inchar (read-char INP ))
          (if (eof-object? inchar)
          #f
          (begin
            (write-char inchar OUT)
            (reader))))
        
        (close-input-port INP)
        (close-output-port OUT)))
    
    (define (maker)
      (print-title)
      (gambitc-dir)
      (newline)
      (folders)
      (dll-enable)
      (comp-gen)
      (target-all)
      
      (lib-target-list lib-pairs )
      (wcc-target-rule lib-pairs "$(LIBDIR)\\")
      
      (gsi-target-list gsi-files )
      (wcc-target-rule gsi-pairs "$(GSIDIR)\\")
      
      (gsc-target-list gsc-pairs )
      (wcc-target-rule gsc-pairs "$(GSCDIR)\\")
      
      ;;(display "..done\n")
      )
    
    (prepare-header)
    (with-output-to-file "Makefile.wcc" maker )
    
     ------------- snip ---------------------------
    # -------------------------------------------------------------------
    # Example Application Makefile
    # Compiler:  OpenWatcom C++ v1.7a
    
    WINLIBS = ws2_32.lib
    #WINGUILIBS = kernel32.lib user32.lib gdi32.lib
    
    GAMBITROOT = c:\Gambit-W
    WATCOM  = c:\WATCOM
    INCDIRS  = -I$(WATCOM)\H
    INCDIRS += -I$(WATCOM)\H\NT 
    INCDIRS += -I$(GAMBITROOT)\include 
    INCDIRS += -I..\clib
    
    LIBDIR  = $(GAMBITROOT)\lib
    
    SCM = ..\scheme
    CLIB    = ..\clib
    
    PGM = hoi
    OBJS    = model.obj screen.obj page0.obj page1.obj menubar.obj sk_tty.obj &
        start.obj start_.obj
    
    WCC_OPTS= -w0 -zp4 -zq -obetir -bm -5r -mf $(INCDIRS) &
        -D___SYS_TYPE_CPU="i686" -D___SINGLE_HOST &
        -D___SYS_TYPE_VENDOR="pc" -D___SYS_TYPE_OS="openwatcom" &
        -D___IMPORTED_ID_SUFFIX="_"
    
    all:    start_.obj
            wlink name hoi.exe op quiet op stack=16384 system nt &
            file {$(OBJS)} &
        library { $(LIBDIR)\gambc.lib $(WINLIBS) $(CLIB)\pdcurses.lib}
        
    start_.obj: .always
        gsc -c $(SCM)\model.scm
        gsc -c $(SCM)\screen.scm
        gsc -c $(SCM)\page0.scm
        gsc -c $(SCM)\page1.scm
        gsc -c $(SCM)\menubar.scm
        gsc -c $(SCM)\start.scm
        gsc -link &
            $(SCM)\model.c &
            $(SCM)\screen.c &
            $(SCM)\page0.c &
            $(SCM)\page1.c &
            $(SCM)\menubar.c &
            $(SCM)\start.c
        wcc386 $(WCC_OPTS) $(CLIB)\sk_tty.c
        wcc386 $(WCC_OPTS) $(SCM)\model.c
        wcc386 $(WCC_OPTS) $(SCM)\screen.c
        wcc386 $(WCC_OPTS) $(SCM)\page0.c
        wcc386 $(WCC_OPTS) $(SCM)\page1.c
        wcc386 $(WCC_OPTS) $(SCM)\menubar.c
        wcc386 $(WCC_OPTS) $(SCM)\start.c
        wcc386 $(WCC_OPTS) $(SCM)\start_.c
    
    ## end
