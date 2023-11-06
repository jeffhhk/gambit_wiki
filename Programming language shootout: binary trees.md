This is a Gambit implementation of the [binary
trees](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=binarytrees&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming_language_shootout "wikilink").

## The program

    #!gsi-script
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Derived by Bradley Lucier from the Ikarus variant, which was
    ;; Derived by Michael D. Adams from the MzScheme variant, which was
    ;; Derived from the Chicken variant by Sven Hartrumpf
    
    (declare (standard-bindings)(extended-bindings)(not safe)(fixnum))
    
    (define-structure node left val right)
    (define-structure leaf val)
    
    (define (make item d)
      (if (= d 0)
          (make-leaf item)
          (let ((item2 (* item 2))
                (d2 (- d 1)))
            (make-node (make (- item2 1) d2) item (make item2 d2)))))
    
    (define (check t)
      (if (leaf? t)
          (leaf-val t)
          (+ (node-val t) (- (check (node-left t)) (check (node-right t))))))
    
    (define (main . argv)
      (let* ((min-depth 4)
             (max-depth (max (+ min-depth 2) (string->number (car argv)))))
        (let ((stretch-depth (+ max-depth 1)))
          (display "stretch tree of depth ")
          (display stretch-depth)
          (display "\t check: ")
          (display (check (make 0 stretch-depth)))
          (display "\n"))
        (let ((long-lived-tree (make 0 max-depth)))
          (do ((d 4 (+ d 2)))
              ((> d max-depth))
            (let ((iterations
                   (fxarithmetic-shift-left 1 (+ (- max-depth d) min-depth))))
              (do ((i 0 (+ i 1))
               (c 0 (+ c (check (make i d)) (check (make (- i) d)))))
              ((>= i iterations)
               (display (* 2 iterations))
               (display "\t trees of depth ")
               (display d)
               (display "\t check: ")
               (display c)
               (display "\n")))))
          (display "long lived tree of depth ")
          (display max-depth)
          (display "\t check: ")
          (display (check long-lived-tree))
          (display "\n"))))

## Compiling

    gsc binary-trees

## Running

    gsi -:m100000 binary-trees 16

Here we specify a minimum heap size of 100MB.
