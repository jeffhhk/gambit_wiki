This is a Gambit implementation of the
[sum-file](http://shootout.alioth.debian.org/gp4sandbox/benchmark.php?test=sumcol&lang=all)
benchmark of the [Computer Language Benchmarks
Game](Programming_language_shootout.md).

I have no idea what the real data is supposed to be.

## The program

    #!gsi-script
    
    ;; The Computer Language Benchmarks Game
    ;; http://shootout.alioth.debian.org/
    ;;
    ;; Derived by Bradley Lucier from the Ikarus variant
    ;; derived by Michael D. Adams from the MzScheme variant
    
    (declare (standard-bindings)(extended-bindings)(block)(not safe))
    
    
    (define (main)
      (time (let ([in (current-input-port)])
        (let loop ([acc 0])
          (let ([n (read-line in)])
        (if (eof-object? n)
            (begin (display acc) (newline))
            (loop (+ acc (string->number n)))))))))

## Compiling

    gsc sum-file

## Running

    gsi sum-file < sum-file.in
