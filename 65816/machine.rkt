#lang racket/base
(require racket/file 
         racket/cmdline
         "cpu.rkt")

(define rom-start 0)
(define ram-start 0)
(define ram-size 0)

(define (load&run pth)
  (simulate rom-start (file->bytes pth)
            ram-start ram-size))

(command-line #:program "65816"
              #:once-each
              ["--rom-start" addr "addr that the rom starts at"
                             (set! rom-start (string->number addr))]
              ["--ram-start" addr "addr that the ram starts at"
                             (set! ram-start (string->number addr))]
              ["--ram-size" size "how much ram is available"
                            (set! ram-size (string->number size))]
              #:args (path) (load&run rom-path))

