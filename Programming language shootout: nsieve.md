This is a Gambit implementation of the
[nsieve](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=nsieve&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming_language_shootout "wikilink").

## The program

    #!gsi-script
    ;; $Id: nsieve.scm,v 1.3 2008/02/17 23:43:53 lucier Exp lucier $
    ;; The Great Computer Language Shootout
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; nsieve benchmark for The Computer Language Shootout
    ;; Written by Dima Dorfman, 2004
    ;; Converted to MzScheme by Brent Fulgham
    ;; Converted to Gambit by Bradley Lucier
    
    (declare (standard-bindings)(extended-bindings)(block)(fixnum)(not safe))
    
    (define (nsieve m)
    
      ;; vectors with #t or #f are too big to be allocated
      ;; on 32-bit machines for the size of arguments in the test
      
      (define (make-bvector n val)
        (make-u8vector n (if val 1 0)))
      
      (define (bvector-true? a i)
        (= (u8vector-ref a i) 1))
      
      (define (bvector-clear! a i)
        (u8vector-set! a i 0))
      
      (define (bvector-set! a i)
        (u8vector-set! a i 1))
      
      (let ((a (make-bvector m #t)))
        (let loop ((i 2)
               (n 0))
          (if (< i m)
          (if (bvector-true? a i)
              (let clear ((j (+ i i)))
            (if (< j m)
                (begin
                  (bvector-clear! a j)
                  (clear (+ j i)))
                (loop (+ 1 i)
                  (+ 1 n))))
              (loop (+ 1 i) n))
          n))))
    
    (define (test n)
      
      (define (string-pad s n)
        (string-append (make-string (- n (string-length s)) #\space)
               s))
      
      (let* ((m (* (expt 2 n) 10000))
         (count (nsieve m)))
        (display (list "Primes up to "
               (string-pad (number->string m) 8)
               " "
               (string-pad (number->string count) 8)
               #\newline))))
    
    
    (define (main arg)
      (let ((n (string->number arg)))
        (if (not n)
        (begin
          (display "An integer is required") (newline) 2)
        (begin
          (if (>= n 0) (test n))
          (if (>= n 1) (test (- n 1)))
          (if (>= n 2) (test (- n 2)))
          0))))

## Compiling

    gsc nsieve

## Running

    gsi nsieve 9
