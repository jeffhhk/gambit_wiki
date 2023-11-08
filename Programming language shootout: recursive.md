This is a Gambit implementation of the
[recursive](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=recursive&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming%20language%20shootout.md).

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the MzScheme variant
    
    (declare (standard-bindings)(extended-bindings)(block)(not safe))
    
    ;; -------------------------------
    
    (define (ack m n)
      (cond ((fxzero? m) (fx+ n 1))
            ((fxzero? n) (ack (fx- m 1) 1))
            (else (ack (fx- m 1) (ack m (fx- n 1))))))
    
    ;; --------------
    
    (define (fib n)
      (cond ((fx< n 2) 1)
            (else (fx+ (fib (fx- n 2)) (fib (fx- n 1))))))
    
    (define (fibflt n)
      (cond ((fl< n 2.0) 1.0)
            (else (fl+ (fibflt (fl- n 2.0)) (fibflt (fl- n 1.0))))))
    
    ;; --------------
    
    (define (tak x y z)
      (cond ((not (fx< y x)) z)
            (else (tak (tak (fx- x 1) y z) (tak (fx- y 1) z x) (tak (fx- z 1) x y)))))
    
    (define (takflt x y z)
      (cond ((not (fl< y x)) z)
            (else (takflt (takflt (fl- x 1.0) y z) (takflt (fl- y 1.0) z x) (takflt (fl- z 1.0) x y)))))
    
    ;; -------------------------------
    
    ;;; Boiler-plate for formatting floating point values
    (define (roundto digits n)
      (let* ([e (expt 10 digits)]
             [num (round (abs (* e (inexact->exact n))))]
             [str (number->string (remainder num e))])
        (string-append
         (if (negative? n) "-" "")
         (number->string (quotient num e))
         "."
         (make-string (- digits (string-length str)) #\0)
         str)))
    
    (define (main . args)
      (let ((n (string->number (car args))))
    
        (display "Ack(3,") (display n) (display "): ") (display (ack 3 n)) (newline)
    
        (display "Fib(") (display (roundto 1 (+ 27.0 n))) (display "): ")
        (display (roundto 1 (fibflt (+ 27.0 n)))) (newline)
    
        (set! n (- n 1))
    
        (display "Tak(") (display (* n 3))
        (display ",") (display (* n 2))
        (display ",") (display n) (display "): ")
        (display (tak (* n 3) (* n 2) n)) (newline)
    
        (display "Fib(3): ") (display (fib 3)) (newline)
    
        (display "Tak(3.0,2.0,1.0): ") (display (roundto 1 (takflt 3.0 2.0 1.0)))
        (newline)))
    
    ;; -------------------------------

## Compiling

    gsc recursive

## Running

    gsi -:m10000 recursive 11

Here we give a minimum heap of 10MB.
