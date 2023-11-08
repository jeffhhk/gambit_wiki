This is a Gambit implementation of the
[reverse-complement](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=revcomp&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming%20language%20shootout.md).

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the MzScheme variant
    
    
    (declare (standard-bindings)(extended-bindings)(block)(fixnum)(not safe))
    
    (define-macro (unless test . body)
      `(if (not ,test)
           (begin
         ,@body)))
    
    (define translation (make-vector 128))
    
    (for-each
     (lambda (from-to)
       (let* ([char (lambda (sym) (string-ref (symbol->string sym) 0))]
              [from (char (car from-to))]
              [to (char-upcase (char (cadr from-to)))])
         (vector-set! translation (char->integer from) to)
         (vector-set! translation (char->integer (char-upcase from)) to)))
     '([a t]
       [c g]
       [g c]
       [t a]
       [u a]
       [m k]
       [r y]
       [w w]
       [s s]
       [y R]
       [k M]
       [v b]
       [h d]
       [d h]
       [b v]
       [n n]))
    
    (define (put-whole-string s) (write-substring s 0 (string-length s)))
    
    (define (output lines)
      (if (> (length lines) 2)
          (let* ([pos (- (string-length (cadr lines)) (string-length (car lines)))]
                 [put-first-half
                  (lambda (s i) (write-substring s 0 i))]
                 [put-second-half
                  (lambda (s i) (write-substring s i (string-length s)))])
            (put-whole-string (car lines))
            (put-first-half (cadr lines) pos)
            (newline)
            (let loop ([l (cdr lines)])
              (cond
               [(null? (cdr l))
                (put-second-half (car l) pos)
                (newline)]
               [else
                (put-second-half (car l) pos)
                (put-first-half (cadr l) pos)
                (newline)
                (loop (cdr l))])))))
    
    (define (main . args)
      (let loop ([accum '()])
        (let ([l (read-line)])
          (if (eof-object? l)
          (output accum)
          (cond
           [(and (not (zero? (string-length l)))
             (eqv? #\> (string-ref l 0)))
            (output accum)
            (put-whole-string l)
            (newline)
            (loop '())]
           [else
            (let* ([len (string-length l)]
               [dest (make-string len)])
              (let loop ([i 0][j (- len 1)])
            (unless (= i len)
                (string-set! dest j
                         (vector-ref translation
                             (char->integer (string-ref l i))))
                (loop (+ i 1) (- j 1))))
              (loop (cons dest accum)))]))))
      (force-output))

## Compiling

    gsc reverse-complement

## Running

    gsi reverse-complement < fasta.in > fasta.out

fasta.in is computed by

    gsi fasta 2500000 > fasta.in
