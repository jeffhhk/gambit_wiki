Dominique Boucher ported the algorithms used in the
[Bison](http://www.gnu.org/software/bison/bison.html) parser generator
to several Scheme dialects including Gambit. His project, Lalr, is
available [here](http://schemeway.dyndns.org/Lalr/lalr.html).

Lalr does the "behind-the-scenes" work of creating programs that
recognize input described by a context-free grammar. You only need to
give the grammar and some other things. All your input to Lalr has a
very Scheme-like syntax. Lalr creates a Scheme function (the parser)
which can call other Scheme functions at certain points in the parsing.

Unfortunately Lalr version 2.1.0 only comes wth one example. This page
gives another example (although a special-purpose one) and describes
using Lalr with Gambit.

See [Creating tags from Gambit library C
files](Creating tags from Gambit library C files.md) for why
this example was written, and [User:dpeschel](User:dpeschel.md).

I ran the example using these steps. I used Mac OS 10.3.7, Lalr 2.1.0,
and Gambit 4.2.4. Lalr 2.3.0 is a [Snow](http://snow.iro.umontreal.ca/)
package so the installation step is different, and other steps may need
to be changed too.

  - Download and unpack the Lalr sources.

<!-- end list -->

  - Change to the directory containing the unpacked sources. Run `make
    gambit`. Lalr is implemented in one file, `lalr.scm`.

<!-- end list -->

  - Put this code in a file, say `paren-depth.scm`.

`;;; Read C source code, breaking it into the following types of tokens:`  
`;;; the identifier ___P, other identifiers, left and right parentheses,`  
`;;; and any other non-spacing character.  White space (space, tab, and`  
`;;; newline characters) is never a token and may come between any two`  
`;;; tokens, before the first, or after the last.`  
  
`;;; Whenever the identifier ___P is seen, read a left parenthesis`  
`;;; followed by a body (zero or more tokens) followed by a right`  
`;;; parenthesis.  If the body contains parentheses they must be properly`  
`;;; paired.  Other tokens in the body, including ___P, have no effect.`  
`;;; Count the deepest nesting level used in the body.  Count the maximum`  
`;;; deepest level (of all the bodies seen so far).`  
  
`;;; At the end of the file, print the maximum deepest level, or 0 if no`  
`;;; bodies were found.`  
  
  
`;;; Global variables used by lexical analyzer and parser.`  
`;;; The lexical analyzer needs them to print the maximum level at the`  
`;;; end of the file.`  
  
`(define depth 0)`  
`(define max-depth 0)`  
  
`;;; Lexical analyzer.  Passes tokens to the parser.`  
  
`(define (paren-depth-lexer errorp)`  
`  (lambda ()`  
  
`    ;; Utility functions, for identifying characters, skipping any`  
`    ;; amount of white space, or reading multicharacter tokens.`  
  
`    (letrec ((char-whitespace?`  
`              (lambda (c)`  
`                (or (char=? c #\space)`  
`                    (char=? c #\tab)`  
`                    (char=? c #\newline))))`  
`             (skip-whitespace`  
`              (lambda ()`  
`                (let loop ((c (peek-char)))`  
`                  (if (and (not (eof-object? c))`  
`                           (char-whitespace? c))`  
`                      (begin (read-char)`  
`                             (loop (peek-char)))))))`  
  
`             (char-in-id?`  
`              (lambda (c)`  
`                (or (char-alphabetic? c)`  
`                    (char=? c #\_))))`  
`             (read-___P-or-other-id`  
`              (lambda (l)`  
`                (let ((c (peek-char)))`  
`                  (if (char-in-id? c)`  
`                      (read-___P-or-other-id (cons (read-char) l))`  
`                      ;; else`  
`                      (if (equal? l '(#\P #\_ #\_ #\_))`  
`                          '___P`  
`                          ;; else`  
`                          'ID))))))`  
  
`      ;; The lexer function.`  
  
`      (skip-whitespace)`  
`      (let loop ((c (read-char)))`  
`        (cond`  
`         ((eof-object? c)      (begin (display "max depth ")`  
`                                      (display max-depth)`  
`                                      (newline)`  
`                                      '*eoi*))`  
`         ((char-whitespace? c) (begin (errorp "didn't expect whitespace " c)`  
`                                      (loop (read-char))))`  
`         ((char-in-id? c)      (read-___P-or-other-id (list c)))`  
`         ((char=? c #\()       'LPAREN)`  
`         ((char=? c #\))       'RPAREN)`  
`         (else                 'CHAR))))))`  
  
`;;; Parser.`  
  
`(define paren-depth-parser`  
`  (lalr-parser`  
  
`   ;; Options.`  
  
`   (expect: 0) ;; even one conflict is an error`  
  
`   ;; List of terminal tokens.`  
  
`   (CHAR LPAREN RPAREN ID ___P)`  
  
`   ;; Grammar rules.`  
  
`   (file       (newfile tokens))`  
`   (newfile    ()                      : (begin (set! depth 0)`  
`                                                (set! max-depth 0)))`  
  
`   (tokens     (tokens token)`  
`                (token))`  
  
`   ;; When not after a ___P, the structure of the file is unimportant.`  
`   (token      (CHAR)`  
`                (LPAREN)`  
`                (RPAREN)`  
`                (ID)`  
  
`   ;; But after a ___P, we start counting parentheses.`  
`                (___P newexpr in LPAREN exprs RPAREN out)`  
`                (___P newexpr in LPAREN       RPAREN out))`  
`   (newexpr    ()                      : (set! depth 0))`  
  
`   ;; Inside an expression, ___P is treated like all other identifiers.`  
`   ;; Only parentheses do anything very interesting.  I'm assuming Lalr`  
`   ;; will enforce the pairing of parentheses, so my in and out actions`  
`   ;; don't check for too many or too few closing parens.`  
  
`   (exprs      (exprs expr)`  
`                (expr))`  
  
`   (expr       (CHAR)`  
`                (in LPAREN exprs RPAREN out)`  
`                (in LPAREN       RPAREN out)`  
`                (ID)`  
`                (___P))`  
`   (in         ()                      : (begin (set! depth (+ depth 1))`  
`                                                (if (> depth max-depth)`  
`                                                  (set! max-depth depth))))`  
`   (out        ()                      : (set! depth (- depth 1)))))`  
  
`;;; Main program.`  
  
`(define paren-depth`  
`  (let ((errorp`  
`          (lambda args`  
`            (for-each display args)`  
`            (newline))))`  
`    (lambda ()`  
`      (paren-depth-parser (paren-depth-lexer errorp) errorp))))`  
  
`(paren-depth)`

  - Gambit's `load` is useless on files containing macros, like
    `lalr.scm`. Instead you must `include` the files. This shell command
    generates and runs the parser:

`gsi -e "(include \"lalr.scm\")" paren-depth.scm < input.c`

and this shell command generates and runs the parser on a series of
input files:

`for x in gambc-v4_2_5/lib/[cmos]*.c; do`  
`  echo $x`  
`  gsi -e "(include \"lalr.scm\")" paren-depth.scm < $x`  
`done`
