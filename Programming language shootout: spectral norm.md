This is a Gambit implementation of the
[spectral-norm](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=spectralnorm&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming language shootout.md).

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the MzScheme variant
    
    (declare (standard-bindings)(extended-bindings)(block)(not safe))
    
    ;;; Stupid boiler-plate for formatting floating point value
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
    
    ;; -------------------------------
    
    (define (approximate n)
      (let ((u (make-f64vector n 1.0))
            (v (make-f64vector n 0.0)))
        (do ((i 0 (fx+ i 1)))
            ((fx= 10 i))
          (mulAtAv n u v)
          (mulAtAv n v u))
        (let loop ([i 0] [vBv 0.0] [vV 0.0])
          (if (fx< i n)
              (loop (fx+ i 1)
                    (fl+ vBv (fl* (f64vector-ref u i) (f64vector-ref v i)))
                    (fl+ vV (fl* (f64vector-ref v i) (f64vector-ref v i))))
              (flsqrt (fl/ vBv vV))))))
    
    ;; --------------
    
    (define (A i j)
      (fl/ 1.0 (fixnum->flonum (fx+ (fx+ (fxquotient (fx* (fx+ i j) (fx+ (fx+ i j) 1)) 2) i) 1))))
    
    ;; --------------
    
    (define (mulAv n v av)
      (do ((i 0 (fx+ i 1)))
          ((fx= n i))
        (f64vector-set! av i 0.0)
        (do ((j 0 (fx+ j 1)))
            ((fx= n j))
          (let ()
            (declare (inlining-limit 10000))
            (f64vector-set! av i (fl+ (f64vector-ref av i) (fl* (A i j) (f64vector-ref v j))))))))
    
    ;; --------------
    
    (define (mulAtV n v atv)
      (do ((i 0 (fx+ i 1)))
          ((= n i))
        (f64vector-set! atv i 0.0)
        (do ((j 0 (fx+ j 1)))
            ((fx= n j))
          (let ()
            (declare (inlining-limit 10000))
            (f64vector-set! atv i (fl+ (f64vector-ref atv i) (fl* (A j i) (f64vector-ref v j))))))))
    
    ;; --------------
    
    (define (mulAtAv n v atav) 
      (let ((u (make-f64vector n 0.0)))
        (mulAv n v u)
        (mulAtV n u atav)))
    
    ;; -------------------------------
    
    (define (main . args)
      (let ((n (if (< (length args) 1)
                   100
                   (string->number (car args)))))
        (display (roundto 9 (approximate n))) (newline)))
    
    ; -------------------------------

## Compiling

    gsc spectral-norm

## Running

    gsi spectral-norm 5500
