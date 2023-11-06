This is a Gambit implementation of the
[nsieve-bits](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=nsievebits&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming_language_shootout "wikilink").

## The program

    #!gsi-script
    ;; $Id: nsieve-bits.scm,v 1.2 2008/02/17 23:44:02 lucier Exp lucier $
    ;; The Great Computer Language Shootout
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; nsieve benchmark for The Computer Language Shootout
    ;; Written by Dima Dorfman, 2004
    ;; Converted to MzScheme by Brent Fulgham
    ;; Converted to Gambit by Bradley Lucier
    
    (declare (standard-bindings)(extended-bindings)(block)(fixnum)(not safe))
    
    (define (nsieve m)
      
      (define (make-bvector n val)
        (make-u8vector (quotient (+ n 7) 8)
               (if val 255 0)))
      
      (define (bvector-true? a i)
        (not (zero? (fxand (u8vector-ref a (quotient i 8))
                   (fxarithmetic-shift-left 1 (remainder i 8))))))
      
      (define (bvector-clear! a i)
        (let* ((index (quotient i 8))
           (byte (u8vector-ref a index))
           (mask (fxarithmetic-shift-left 1 (remainder i 8))))
          (u8vector-set! a index (fxand byte (fxnot mask)))))
      
      (define (bvector-set! a i)
        (let* ((index (quotient i 8))
           (byte (u8vector-ref a index))
           (mask (fxarithmetic-shift-left 1 (remainder i 8))))
          (u8vector-set! a index (fxior byte mask))))
      
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

    gsc nsieve-bits

## Running

    gsi nsieve-bits 11
