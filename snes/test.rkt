#lang racket/base
(require tests/eli-tester
         racket/file
         "compiler.rkt")

(define (compare-image-after-compile rkt-pth orig)
  (test
   (compile (dynamic-require rkt-pth 'Main)) => (file->bytes orig)))

(test
 (compare-image-after-compile "tic-tac-toe-tutorial/done.rkt" "tic-tac-toe-tutorial/original.smc"))