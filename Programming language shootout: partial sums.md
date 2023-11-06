This is a Gambit implementation of the
[partial-sums](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=partialsums&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming_language_shootout.md).

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the Chicken variant
    
    (declare (standard-bindings)(extended-bindings)(block)(not safe))
    
    ;;; Stupid boiler-plate for formatting floating point values
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
      (let ([n (exact->inexact (string->number (car args)))]
            [fl2/3 (fl/ 2.0 3.0)]
            [format-result
             (lambda (str n)
               (display (roundto 9 n))
               (display str))])
        (let ((sums (f64vector 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
                   1.0 1.0)))
          (let loop ()
        
        (define-macro (with-sums . body)
          `(let ((s0 (f64vector-ref sums 0))
             (s1 (f64vector-ref sums 1))
             (s2 (f64vector-ref sums 2))
             (s3 (f64vector-ref sums 3))
             (s4 (f64vector-ref sums 4))
             (s5 (f64vector-ref sums 5))
             (s6 (f64vector-ref sums 6))
             (s7 (f64vector-ref sums 7))
             (s8 (f64vector-ref sums 8))
             (d  (f64vector-ref sums 9))
             (alt (f64vector-ref sums 10)))
             ,@body))
        
        (define (s0-set! val) (f64vector-set! sums 0 val))
        (define (s1-set! val) (f64vector-set! sums 1 val))
        (define (s2-set! val) (f64vector-set! sums 2 val))
        (define (s3-set! val) (f64vector-set! sums 3 val))
        (define (s4-set! val) (f64vector-set! sums 4 val))
        (define (s5-set! val) (f64vector-set! sums 5 val))
        (define (s6-set! val) (f64vector-set! sums 6 val))
        (define (s7-set! val) (f64vector-set! sums 7 val))
        (define (s8-set! val) (f64vector-set! sums 8 val))
        (define (d-set! val) (f64vector-set! sums 9 val))
        (define (alt-set! val) (f64vector-set! sums 10 val))
        
        (if (with-sums (fl> d n))
            (with-sums
             (format-result "\t(2/3)^k\n" s0)
             (format-result "\tk^-0.5\n" s1)
             (format-result "\t1/k(k+1)\n" s2)
             (format-result "\tFlint Hills\n" s3)
             (format-result "\tCookson Hills\n" s4)
             (format-result "\tHarmonic\n" s5)
             (format-result "\tRiemann Zeta\n" s6)
             (format-result "\tAlternating Harmonic\n" s7)
             (format-result "\tGregory\n" s8))
            
            (with-sums
             (let* ((d2 (fl* d d))
                (d3 (fl* d2 d))
                (ds (flsin d))
                (dc (flcos d)))
               (s0-set! (fl+ s0 (flexpt fl2/3 (fl- d 1.0))))
               (s1-set! (fl+ s1 (fl/ 1.0 (flsqrt d))))
               (s2-set! (fl+ s2 (fl/ 1.0 (fl* d (fl+ d 1.0)))))
               (s3-set! (fl+ s3 (fl/ 1.0 (fl* d3 (fl* ds ds)))))
               (s4-set! (fl+ s4 (fl/ 1.0 (fl* d3 (fl* dc dc)))))
               (s5-set! (fl+ s5 (fl/ 1.0 d)))
               (s6-set! (fl+ s6 (fl/ 1.0 d2)))
               (s7-set! (fl+ s7 (fl/ alt d)))
               (s8-set! (fl+ s8 (fl/ alt (fl- (fl* 2.0 d) 1.0))))
               (d-set! (fl+ d 1.))
               (alt-set! (fl- alt))
               (loop))))))))

## Compiling

    gsc partial-sums

## Running

    gsi partial-sums 2500000
