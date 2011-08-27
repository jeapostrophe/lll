#lang racket/base
(require racket/list
         racket/match
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

(define current-labels (make-parameter #f))
(struct label (actual-address references)
        #:transparent
        #:mutable)
(struct label-reference (use-address kind)
        #:transparent)

; Addresses should be 24 bits
(define HIGH-BITS #b111111110000000000000000)
(define (format-addr addr use-addr kind)
  (match kind
    ; The two addresses share the first 8 bits
    ['absolute
     (cond
       [(= (bitwise-and addr HIGH-BITS)
           (bitwise-and use-addr HIGH-BITS))
        (integer->integer-bytes 
         (bitwise-and (bitwise-not HIGH-BITS) addr)
         2 #f)]
       [else
        (error 
         'format-addr
         "Absolute addr references do not share upper 8-bits: ~e and ~e"
         addr use-addr)])]
    ; The two addresses are within 128 bytes
    ['relative
     (define diff (- use-addr addr))
     (cond 
       [(= 0 diff)
        (bytes diff)]
       [(< 0 diff (add1 128))
        (bytes diff)]
       [(< (sub1 -128) diff 0)
        (bytes
         (+ (abs diff)
            #b10000000))]
       [else
        (error 'format-addr
               "Distance too far for relative addr: ~e from ~e" 
               addr use-addr)])]
    [x
     (error 'format-addr "Can't format ~v to ~e from ~v" 
            addr x use-addr)]))

(define (label-lookup! the-label use-addr kind)
  (match-define
   (and l (label actual refs))
   (hash-ref! (current-labels) the-label
              (λ () (label #f empty))))
  (cond
    [actual
     (format-addr actual use-addr kind)]
    [else
     (set-label-references! l
                            (list* (label-reference use-addr kind)
                                   refs))
     (format-addr 0 0 kind)]))
(define (label-define! the-label actual-addr)
  (match
   (hash-ref! (current-labels) the-label
              (λ () (label #f empty)))
    [(and l (label #f refs))
     (set-label-actual-address! l actual-addr)]
    [_
     (error 'label-define!
            "Label ~e has already been defined"
            the-label)]))

(define (compile make-code)
  (define labels (make-hasheq))
  (parameterize ([current-labels labels])
    ; Discover sections
    (define this-section-box (box empty))
    (parameterize ([current-section-box this-section-box])
      (make-code))
    (define these-sections (unbox this-section-box))
    ; Write sections
    (define the-bytes
      (with-output-to-bytes
          (λ ()
            (port-count-lines! (current-output-port))
            (for ([s (in-list these-sections)])
              ((section-thunk s))))))
    ; Rewrite labels
    (for ([(label-name l) (in-hash labels)])
      (match-define (label label-addr refs) l)
      (unless label-addr
        (error 'compile 
               "Label ~e was never defined"
               label-name))
      (for ([r (in-list refs)])
        (match-define (label-reference use-addr kind) r)
        (bytes-copy! the-bytes use-addr
                     (format-addr label-addr kind))))
    the-bytes))

(provide (all-defined-out))