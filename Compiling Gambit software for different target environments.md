### How to build GUI apps for Win32

The misc\\vcexpress.bat file, used as described in INSTALL, both found
in the source repo file are good sources of inspiration on how to build
Gambit apps using MSVC.

Here's a good GUI example app for win32, call it test.scm:

  -   
    (c-declare "\#include \<windows.h\>")
    (define message-box (c-lambda (UTF-8-string) void
    "MessageBox(0,\_\_\_arg1,\\"Dbg\\",0);"))
    (message-box "Hi\!")

In order to compile it, first run misc\\vcexpress to get
lib\\libgambc.lib . Then do:

  -   
    C:\\Program Files\\Microsoft Visual Studio 8\\VC\\vcvarsall
    gsc -link test
    cl.exe -Fetest.exe -nologo -Oityb1 -MT
    -D\_CRT\_SECURE\_NO\_DEPRECATE -c
    -I"C:\\gambc-v4\_3\_2-devel\\include"
    -I"C:\\gambc-v4\_3\_2-devel\\lib" -D\_\_\_SINGLE\_HOST -D\_WINDOWS
    "test\_.c" "test.c"
    cl -Fetest.exe test.obj test\_.obj
    C:\\gambc-v4\_3\_2-devel\\lib\\libgambc.lib Kernel32.Lib User32.Lib
    Gdi32.Lib WS2\_32.Lib /link /subsystem:windows

Voila\! The key recipe in here is to pass -D\_WINDOWS to cl.exe, and
/subsystem:windows to link.exe .

Upon any (display) (force-output) you do, a console window will be
allocated. This can be remedied by doing (current-output-port
(open-dummy)) (current-input-port (open-dummy)) though.

Pass -dr:- to force the app open a console.

For more info see the ml thread around
[here](https://mercure.iro.umontreal.ca/pipermail/gambit-list/2012-April/005879.html).
