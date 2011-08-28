#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
         racket/match
         racket/list
         "compiler.rkt")

(define (16bit-number? n)
  (and (number? n)
       (<= (integer-length n) 16)))
(define (8bit-number? n)
  (and (number? n)
       (<= (integer-length n) 8)))

(define-syntax-rule (define-unimplemented id)
  (begin
    (define-syntax-rule (id . e)
      (eprintf "Unimplemented ~e\n" 'id))
    (provide id)))
(define-syntax-rule (define-unimplemented* id ...)
  (begin (define-unimplemented id) ...))

(define (current-address)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  pos)

(struct label-use (name kind))
(struct addr (constant))

(define anonymous-labels (make-hasheq))
(define (*label-use n [kind 'default])
  (define effective-n
    (match n
      ['+
       (define new (gensym '+))
       (hash-set! anonymous-labels '+ new)
       new]
      ['-
       (hash-ref anonymous-labels '-
                 (λ () 
                   (error 'label-ref
                          "Use of - without previous definition")))]
      [(? symbol?) n]))
  (label-use effective-n kind))
(define (*label-define! n a)
  (define effective-n
    (match n
      ['-
       (define new (gensym '-))
       (hash-set! anonymous-labels '- new)
       new]
      ['+
       (hash-ref anonymous-labels '+
                 (λ () 
                   (error 'label-ref
                          "Definition of + without previous use")))]
      [(? symbol?) n]))
  (label-define! effective-n a))

(define-syntax (label-ref stx)
  (syntax-parse
   stx
   [(_ name:id . args)
    (syntax/loc stx
      (*label-use 'name . args))]))
(define-syntax (snes-label stx)
  (syntax-parse
   stx
   [(_ name:id)
    (syntax/loc stx
      (*label-define! 'name (current-address)))]))

(define-unimplemented* 
  define-memory-map  
  define-rom-bank-size
  define-rom-banks
  define-snes-header
  define-native-vector
  define-emulation-vector
  define-empty-fill
  data
  ;; op codes
  sei ldx txs jsl sep asl eor inc 
  clc tcd stx sta/X
  xce 
  rep jsr dex cli 
  OP:and cmp beq adc tax dec lda.l ldy sty
  xba wai
  )

(define-syntax (define-opcode stx)
  (syntax-parse
   stx
   [(_ (op:id arg:id ...) opcode:expr cycles:nat
       e:expr ...)
    (syntax/loc stx
      (begin 
        ; XXX record cycles
        (define (op arg ...)
          (write-byte opcode)
          e ...)
        (provide op)))]))

(define (write-absolute-label-use label)
  (define addr 
    (label-lookup! (label-use-name label)
                   (current-address)
                   'absolute))
  (write-bytes addr))
(define (write-absolute-label-or-const arg)
  (match arg
    [(addr (? 16bit-number? ad))
     (write-bytes (integer->integer-bytes ad 2 #f))]
    [(? label-use? lab)
     (write-absolute-label-use lab)]))

; XXX Some of these may be long, or otherwise exotic
(define (lda arg)
  (cond
    ; XXX This may be wrong?
    [(16bit-number? arg)
     (write-byte #xA9)
     (write-bytes (integer->integer-bytes arg 2 #f))]
    [(number? arg)
     (error 'lda "Argument too big: ~e/~e" 
            arg
            (number->string arg 16))]
    [else
     (write-byte #xAD)
     (write-absolute-label-or-const arg)]))
(provide lda)

(define-opcode (lda/X arg) #xBD 4
  (write-absolute-label-or-const arg))
(define-opcode (sta arg) #x8D 4
  (write-absolute-label-or-const arg))
(define-opcode (stz arg) #x9C 4
  (write-absolute-label-or-const arg))
(define-opcode (stz/X arg) #x9E 5
  (write-absolute-label-or-const arg))
(define-opcode (inx) #xE8 2)
(define-opcode (iny) #xC8 2)
; XXX cycle/byte note
; XXX can use addresses
(define-opcode (cpx constant) #xE0 2
  (write-byte constant))
; XXX Some uses of this in the code may actually be JML
(define-opcode (jmp label) #x4C 3
  (write-absolute-label-use label))
(define-opcode (bne near-label) #xD0 2
  (define near-addr 
    (label-lookup! (label-use-name near-label)
                   (current-address)
                   'relative))  
  (write-bytes near-addr))
(define-opcode (pha) #x48 3)
(define-opcode (phb) #x8B 3)
(define-opcode (phd) #x0B 4)
(define-opcode (phk) #x4B 3)
(define-opcode (php) #x08 3)
(define-opcode (phx) #xDA 3)
(define-opcode (phy) #x4A 3)
(define-opcode (pla) #x68 4)
(define-opcode (plb) #xAB 4)
(define-opcode (pld) #x2B 5)
(define-opcode (plp) #x28 4)
(define-opcode (plx) #xFA 4)
(define-opcode (ply) #x7A 4)
(define-opcode (rti) #x40 6)
(define-opcode (rtl) #x6B 6)
(define-opcode (rts) #x60 6)

(define-syntax-rule (repeat n . e)
  (for ([i (in-range n)]) . e))

(define current-back-references (make-parameter #f))
(define current-forward-references (make-parameter #f))
(define-syntax (define-section stx)
  (syntax-parse
   stx
   [(_ name:id
       (~or
        (~optional (~seq #:bank bank:expr)
                   #:defaults ([bank #'0]))
        (~optional (~seq #:slot slot:expr)
                   #:defaults ([slot #'0]))
        (~optional (~seq #:org org:expr)
                   #:defaults ([org #'0]))
        (~optional (~seq #:force
                         (~bind [force? #'#t]))
                   #:defaults ([force? #'#f]))
        (~optional (~seq #:semi-free
                         (~bind [semi-free? #'#t]))
                   #:defaults ([semi-free? #'#f]))
        (~optional (~seq #:links
                         (other-section:expr ...))
                   #:defaults ([(other-section 1) empty]))) 
       ...
       . e)
    (syntax/loc stx
      (define name
        (λ ()
          (other-section) ...
          (record-section! 
           'name 
           #:bank bank
           #:slot slot
           #:org org
           #:force? force?
           #:semi-free? semi-free?
           (λ () 
             (parameterize
                 ([current-back-references (box empty)]
                  [current-forward-references (box empty)])
               . e))))))]))

(provide 
 ; From racket/base
 #%module-begin require provide define #%datum #%app
 all-defined-out quote
 ; From this
 define-section repeat label-ref addr
 (rename-out [OP:and and]
             [snes-label label]))