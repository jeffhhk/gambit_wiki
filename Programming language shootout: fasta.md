This is a Gambit implementation of the
[fasta](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=fasta&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming_language_shootout "wikilink").

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the Chicken variant by Anthony Borla
    
    (declare (standard-bindings)(extended-bindings)(block)(not safe))
    
    (define-macro (unless test . body)
      `(if (not ,test)
           (begin
         ,@body)))
    
    (define *alu*
      (string-append
       "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
       "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
       "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
       "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
       "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
       "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
       "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))
    
    (define *iub*
      (list
       '(#\a . 0.27) '(#\c . 0.12) '(#\g . 0.12) '(#\t . 0.27) '(#\B . 0.02)
       '(#\D . 0.02) '(#\H . 0.02) '(#\K . 0.02) '(#\M . 0.02) '(#\N . 0.02)
       '(#\R . 0.02) '(#\S . 0.02) '(#\V . 0.02) '(#\W . 0.02) '(#\Y . 0.02)))
    
    (define *homosapien*
      (list
       '(#\a . 0.3029549426680) '(#\c . 0.1979883004921)
       '(#\g . 0.1975473066391) '(#\t . 0.3015094502008)))
    
    ;; -------------
    
    (define *line-size* 60)
    
    ;; -------------------------------
    
    (define (make-random seed)
      (let ((ia 3877.0)
        (ic 29573.)
        (im 139968.)
        (last (f64vector (exact->inexact seed)))
        (result (f64vector 0.)))
        (lambda ()
          (let* ((advance (fl+ ic
                (fl* (f64vector-ref last 0)
                     ia)))
             (next (fl- advance
                (fl* im
                     (fltruncate (fl/ advance im))))))
        (f64vector-set! last 0 next)
        (f64vector-set! result 0 (fl/ next im))))))
    
    ;; -------------------------------
    
    (define (make-cumulative-table frequency-table)
      (let ((cumulative 0.0))
        (map
         (lambda (x)
           (set! cumulative (fl+ cumulative (cdr x)))
           (cons (car x) cumulative))
           frequency-table)))
    
    ;; -------------
    
    (define random-next (make-random 42))
    (define *segmarker* ">")
    
    ;; -------------
    
    (define (select-random cumulative-table)
      (select-over-threshold (random-next) cumulative-table))
    
    (define (select-over-threshold rvalue table)
      (if (fl<= (f64vector-ref rvalue 0) (cdar table))
          (caar table)
          (select-over-threshold rvalue (cdr table))))
    
    ;; -------------
    
    (define (repeat-fasta id desc n sequence line-length)
      (let ((seqlen (string-length sequence))
            (out (current-output-port))
        (buffer (make-string line-length)))
        (display (string-append *segmarker* id " " desc "\n") out)
        (let loop-o ((n n) (k 0))
          (unless (fx<= n 0)
                  (let ((m (fxmin n line-length)))
                    (let loop-i ((i 0) (k k))
                      (if (fx>= i m)
                          (begin
                (write-substring buffer 0 m)
                            (write-char #\newline out)
                            (loop-o (fx- n line-length) k))
                          (let ([k (if (fx= k seqlen) 0 k)])
                (string-set! buffer i (string-ref sequence k))
                            (loop-i (fx+ 1 i) (fx+ 1 k))))))))
        (force-output out)))
    
    ;; -------------
    
    (define (random-fasta id desc n cumulative-table line-length)
      (let ((out (current-output-port))
        (buffer (make-string line-length)))
        (display (string-append *segmarker* id " " desc "\n") out)
        (let loop-o ((n n))
          (unless (<= n 0)
                  (let ((m (min n line-length)))
                    (let loop-i ((i 0))
                      (unless (>= i m)
                  (string-set! buffer i (select-random cumulative-table))
                              (loop-i (fx+ 1 i))))
            (write-substring buffer 0 m)
                    (write-char #\newline out)
                    (loop-o (fx- n line-length)))))
        (force-output out)))
    
    ;; -------------------------------
    
    (define (main . args)
      (let ((n (string->number (car args))))
        (repeat-fasta "ONE" "Homo sapiens alu" (* n 2) *alu* *line-size*)
        (random-fasta "TWO" "IUB ambiguity codes" (* n 3)
                      (make-cumulative-table *iub*) *line-size*)
        (random-fasta "THREE" "Homo sapiens frequency" (* n 5)
                      (make-cumulative-table *homosapien*) *line-size*) ))
    
    ;; -------------------------------

## Compiling

    gsc fasta

## Running

    gsi fasta 25000000 > /dev/null

## Building the input for [ k-nucleotide](Programming_language_shootout:_k-nucleotide "wikilink")

    gsi fasta 1000000 > knucleotide-input1000000.txt

The file knucleotide-input1000000.txt is used by [
k-nucleotide](Programming_language_shootout:_k-nucleotide "wikilink").
