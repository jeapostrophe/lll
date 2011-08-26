#lang racket/base
(require racket/list
         racket/port)

(struct section (name bank slot org force? semi-free? thunk)
        #:transparent)

(define current-section-box (make-parameter #f))
(define (record-section! name 
                         #:bank bank #:slot slot #:org org
                         #:force? force? #:semi-free? semi-free?
                         thunk)
  (define b (current-section-box))
  (set-box! 
   b
   (list* (section name bank slot org force? semi-free? thunk)
          (unbox b))))

(define (compile make-code)
  (define this-section-box (box empty))
  (parameterize ([current-section-box this-section-box])
    (make-code))
  (define these-sections (unbox this-section-box))
  (with-output-to-bytes
      (λ ()
        (port-count-lines! (current-output-port))
        (for ([s (in-list these-sections)])
          ((section-thunk s))))))

(provide record-section!
         compile)