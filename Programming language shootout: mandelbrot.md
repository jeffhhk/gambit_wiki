This is a Gambit implementation of the
[mandelbrot](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=mandelbrot&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming language shootout.md).

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the Chicken variant by Anthony Borla
    
    
    (declare (standard-bindings)(extended-bindings)(block)(not safe))
    
    (define-macro (when test . body)
      `(if ,test
           (begin
         ,@body)))
    
    ;; -------------------------------
    
    (define *limit-sqr* 4.0)
    
    (define *iterations* 50)
    
    ;; -------------------------------
    
    (define (mandelbrot iterations x frac2/n ci)
      (let ((cr (fl- (fl* (exact->inexact x) frac2/n) 1.5))
        (z (f64vector 0. 0.)))
    
        (define-macro (with-z . body)
          `(let ((zr (f64vector-ref z 0))
             (zi (f64vector-ref z 1)))
         ,@body))
    
        (define (zr-set! val)
          (f64vector-set! z 0 val))
    
        (define (zi-set! val)
          (f64vector-set! z 1 val))
        
        (let loop ((i 0))
          (cond ((fx> i iterations)
             1)
            ((with-z
              (fl> (fl+ (fl* zr zr)
                (fl* zi zi))
               *limit-sqr*))
             0)
            (else
             (with-z
              (zr-set! (fl+ (fl- (fl* zr zr)
                     (fl* zi zi))
                    cr))
              (zi-set! (fl+ (fl* 2.0 zr zi)
                    ci))
              (loop (fx+ 1 i))))))))
    
    ;; -------------------------------
    
    (define (main arg)
      (let* ((n (string->number arg))
         (frac2/n (/ 2.0 n))
         (out (current-output-port)))
        (display "P4") (newline)
        (display n) (display " ") (display n) (newline)
        (let loop-y ((y 0))
          (when (< y n)
                (let ([ci (fl- (fl* (exact->inexact y) frac2/n) 1.0)])
                  (let loop-x ((x 0) (bitnum 0) (byteacc 0))
                    (if (< x n)
                        (let ([bitnum (fx+ 1 bitnum)]
                              [byteacc (fx+ (fxarithmetic-shift-left byteacc 1)
                                            (mandelbrot *iterations* x frac2/n ci))])
                          (cond
                           [(= bitnum 8)
                            (write-u8 byteacc out)
                            (loop-x (fx+ 1 x) 0 0)]
                           [else (loop-x (fx+ 1 x) bitnum byteacc)]))
                        (begin
                          (when (positive? bitnum)
                                (write-u8 (fxarithmetic-shift-left byteacc (- 8 (fxand n #x7)))
                          out))
                          (loop-y (fx+ 1 y))))))))
        (force-output out)))
    
    ;; -------------------------------

## Compiling

    gsc mandelbrot

## Running

    gsi -:m10000 mandelbrot 3000 > output.pbm

Here I give it a 10MB minimum heap.
