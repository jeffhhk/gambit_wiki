This is a Gambit implementation of the
[fannkuch](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=fannkuch&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming%20language%20shootout.md).

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the O'Caml variant, by Christophe Papazian
    
    (declare (standard-bindings)(extended-bindings)(block)(not safe))
    
    (define (write-permutation perm)
      (for-each write (vector->list perm))
      (newline))
    
    ;; Find first value where r[i] != i
    ;; and increment (mod j+2) all values up to that (i.e. set them to j+1)
    ;; returning i+2
    (define (choose-next r)
      (let loop ((i 0))
        (if (eq? (vector-ref r i) i)
            (loop (fx+ 1 i))
            (let loop ((j 0))
              (if (eq? j i)
                  (begin (vector-set! r j (fxremainder (fx+ 1 (vector-ref r j)) (fx+ j 2)))
                         (fx+ j 2))
                  (begin (vector-set! r j (fx+ j 1))
                         (loop (fx+ j 1))))))))
    
    ;; Count number of "pancake flips" it takes to get p[0]=1
    ;; Does not side-effect p, uses s as a temporary
    ;; "Pancake flip" = Reverse first p[0]-1 elements
    (define (count-flips n p s)
      ;; Check if all p[i] != i+1
      (define (check i)
        (or (eq? i n)
            (and (not (eq? (vector-ref p i) (fx+ i 1)))
                 (check (fx+ i 1)))))
      ;; If all p[i] != i+1
      (if (check 0)
          (begin
            ;; set all s[i] = p[i]
            (do ((i 0 (fx+ i 1)))
                ((eq? i n))
              (vector-set! s i (vector-ref p i)))
            ;; keep reversing the first s[0]-1 values of s until s[0] = 1
            ;; and return number of flippings
            (do ((flips 0 (fx+ flips 1))
                 (s0 (fx- (vector-ref s 0) 1)
                     (fx- (vector-ref s 0) 1)))
                ((eq? s0 0)
                 flips)
              ;; reverse the first s[0]-1 values of s
              (do ((i 0  (fx+ i 1))
                   (j s0 (fx- j 1)))
                  ((fx>= i j))
                (let ((tmp (vector-ref s i)))
                  (vector-set! s i (vector-ref s j))
                  (vector-set! s j tmp)))))
          0))
    
    ;; Build a new permutation by "braiding":
    ;;  For every i in [1..n-1]:
    ;;    Shift each p[j<i] down by one
    ;;    and put p[0] at p[i]
    (define (braid n p)
      ;; For every i in [1..n-1]
      (do ((i 1 (fx+ i 1)))
          ((eq? i n))
        ;; Shift each p[j] down by one
        ;; and put p[0] at p[i]
        (let ((t (vector-ref p 0)))
          (do ((j 0 (fx+ j 1)))
              ((eq? j i))
            (vector-set! p j (vector-ref p (fx+ j 1))))
          (vector-set! p i t))))
    
    (define (fannkuch n r p s m z)
      (let ((i (choose-next r)))
        (if (fx> i n)
            m
            (begin
              (if (fx> z 0) (write-permutation p))
              (braid i p)
              (fannkuch n r p s (max m (count-flips n p s)) (fx- z 1))))))
    
    (define (main . args)
      (let ((n (string->number (car args))))
        (let ((r (make-vector n))
              (p (make-vector n))
              (s (make-vector n 0)))
    
          ;; Init r and p to [1,...,n]
          (do ((i 0 (fx+ 1 i)))
              ((eq? i n))
            (vector-set! r i (fx+ i 1))
            (vector-set! p i (fx+ i 1)))
    
          ;; No need to init s; it is only used as a temporary
    
          (let ((x (fannkuch n r p s 0 30)))
            (display "Pfannkuchen(")(display n)(display ") = ")
            (display x)(newline)))))

## Compiling

    gsc fannkuch

## Running

    gsi fannkuch 11
