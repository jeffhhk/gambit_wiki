## Overview

Work in progress. See also [User:dpeschel](User:dpeschel.md).

The hand-written C files in the Gambit `lib/` directory contain many
small functions, so it would be useful to have a list. The GNU `etags`
program can create lists in a format that Emacs can use and in a
different format that vi and Vim can use. These lists support
interactive browsing within Emacs, vi, and Vim. With your cursor on a
symbol, you jump to its definition. Unfortunately the lsits are not very
useful to read by themselves (to get a quick overview of functions and
the order they are defined in, or to get sublists of symbols sorted by
type).

Another program, Exuberant Ctags, has some features `etags` doesn't, but
I haven't gotten Exuberant working yet.

I don't think the original UNIX `ctags` is flexible enough to parse the
Gambit library. I haven't tried yet.

There is no point in parsing C files that were compiled from Scheme, if
I can parse the original Scheme.

The extensive use of preprocessor macros in the library code means
`etags`'s default parser can do very little with the files. Luckily you
can extend the lines recognized by `etags` through its `--regex` option.
The regular expressions used must:

  - Match the entire text of a line or group of lines (this is not
    documented in the `etags` manual page).
  - Contain a subexpression that matches the name of the function being
    defined.
  - Handle enough C syntax to match all and only the correct uses of the
    macros.
      - `etags` doesn't tag forward declarations, so these expressions
        are designed not to match forward declarations.
      - You can't give `etags` an expression and say "treat matches as
        new return types" or "treat new matches as function signatures".
        Your expression must match everything needed to find any
        function definition -- return type, linkage, function name,
        signature, and opening brace. However, `etags` does not need to
        know where those parts are within the matched text, except the
        function name as described above.

Because the regular expressions may match more than one line, a recent
version of `etags` is required. Earlier versions don't allow multiline
matches. The version distributed with Emacs 22.1 works, the version
distributed with Emacs 21.4a doesn't work.

## Library files

`lib/c_intf.c`  
`lib/main.c`  
`lib/mem.c`  
`lib/os.c`  
`lib/os_base.c `  
`lib/os_dyn.c`  
`lib/os_files.c`  
`lib/os_io.c`  
`lib/os_shell.c`  
`lib/os_time.c`  
`lib/os_tty.c`  
`lib/setup.c`

## Parsing `___PVOID`

This expression handles the \_\_\_PVOID macro:

`{c}/.*`\([ \t]\|\n\)`+`\([A-Za-z_][A-Za-z_0-9]*\)\([ \t]\|\n\)`+___PVOID`\([ \t]\|\n\)`*{/\2/m`

Unfortunately, following a tag will land on the line with the brace.

This expression doesn't match the opening brace. It will tag forward
declarations by mistake, but following a tag will land on the line with
the tag.

`{c}/.*`\([ \t]\|\n\)`+`\([A-Za-z_][A-Za-z_0-9]*\)\([ \t]\|\n\)`+___PVOID/\2/m`

## Parsing `___P`

The \_\_\_P macro takes two arguments, each of which usually contains
parentheses. No regular expression can match text containing parentheses
nested to an unknown depth. Howver, if you have a known depth, you can
create a regular expression that matches text containing parentheses
nested to that depth or less. As the known depth gets bigger, the
expressions get longer, so it's wise to use the lowest depth that will
work.

### Finding the maximum depth of parentheses used

The maximum depth after any \_\_\_P invocation in any of the library
files is four. See [this page](Lalr_example.md) for the script
used to find that.

### Generating a regexp

### Using the regexp

[Category: development](Category:_development.md) [Category:
internals](Category:_internals.md)
