#lang racket/base
(require tests/eli-tester
         racket/list
         racket/file
         file/sha1
         "compiler.rkt")

(define (compare-bytes got expected)
  (for/fold ([diff-addr #f])
    ([g (in-bytes got)]
     [e (in-bytes expected)]
     [addr (in-naturals)])
    (cond
      [(and (= g e) diff-addr)
       (printf "~a: expected: ~a\n~a:      got: ~a\n\n"
               (number->string diff-addr 16)
               (bytes->hex-string (subbytes got diff-addr addr))
               (number->string diff-addr 16)
               (bytes->hex-string (subbytes expected diff-addr addr)))
       #f]
      [(= g e)
       #f]
      [diff-addr
       diff-addr]
      [else
       addr])))

(define (compare-image-after-compile rkt-pth orig)
  (define rom-pth (path-replace-suffix rkt-pth #".sfc"))
  (test
   (compile-rom rom-pth (dynamic-require rkt-pth 'ROM))
   (let ()
     (define rom-bs (file->bytes rom-pth))
     (define orig-bs (file->bytes orig))
     (test
      (bytes-length rom-bs) => (bytes-length orig-bs)
      rom-bs => orig-bs
      (compare-bytes rom-bs orig-bs) => #f))))

(test
 (compare-image-after-compile "tic-tac-toe-tutorial/done.rkt" "tic-tac-toe-tutorial/original.smc"))