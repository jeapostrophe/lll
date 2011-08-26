#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/list
         "compiler.rkt")

(struct *label (addr))

(define-syntax-rule (define-unimplemented id)
  (begin
    (define-syntax-rule (id . e)
      (eprintf "Unimplemented ~e\n" 'id))
    (provide id)))
(define-syntax-rule (define-unimplemented* id ...)
  (begin (define-unimplemented id) ...))

(define-unimplemented* 
  define-memory-map  
  define-rom-bank-size
  define-rom-banks
  define-snes-header
  define-native-vector
  define-emulation-vector
  define-empty-fill
  data
  macro-invoke
  ;; op codes
  sei ldx txs jsl sep asl eor inc 
  clc lda tcd stx sta stz 
  xce inx cpx 
  rep jsr dex cli 
  OP:and cmp beq adc tax jmp dec lda.l ldy sty
  xba wai
  )

(define (current-address)
  (define-values (line col pos)
    (port-next-location (current-output-port)))
  pos)

(define-syntax (snes:- stx)
  (syntax-parse
   stx
   [x:id
    (syntax/loc stx
      (first (unbox (current-back-references))))]))

(define-syntax (label stx)
  (syntax-parse
   stx
   #:literals (snes:- snes:+ snes:++)
   [(_ snes:-)
    (syntax/loc stx
      (set-box! (current-back-references)
                (list* (current-address)
                       (current-back-references))))]
   [(_ snes:+)
    (syntax/loc stx
      (rewrite-forward-references! (current-address) 1))]
   [(_ snes:++)
    (syntax/loc stx
      (rewrite-forward-references! (current-address) 2))]
   [(_ name:id)
    (syntax/loc stx
      (define name (current-address)))]))

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

(define-opcode (bne near-label) #xD0 2
  (write-byte (*label-addr near-label)))
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
                   #:defaults ([semi-free? #'#f]))) 
       ...
       . e)
    (syntax/loc stx
      (record-section! 
       'name 
       #:bank bank
       #:slot slot
       #:org org
       #:force? force?
       #:semi-free? semi-free?
       (λ () 
         (parameterize ([current-back-references (box empty)]
                        [current-forward-references (box empty)])
           . e))))]))

(define-syntax (snes-require stx)
  (syntax-parse
   stx
   [(_ pth)
    (with-syntax ([make-code (format-id stx "make-code")]
                  [this-make-code (generate-temporary)]) 
      (syntax/loc stx
        (begin
          (local-require (rename-in pth 
                                    [make-code this-make-code]))
          ; XXX attach to environment
          (this-make-code))))]))

(define-syntax (snes-#%module-begin stx)
  (syntax-parse 
   stx
   [(_ . e)
    (with-syntax ([make-code (format-id stx "make-code")]) 
      (syntax/loc stx
        (#%module-begin
         (define (make-code) . e)
         (provide make-code))))]))

(provide 
 ; From racket/base
 define #%datum #%app
 ; From this
 define-section repeat label
 (rename-out [OP:and and]
             [snes:- -]
             [snes:+ +]
             [snes:++ ++]
             [snes-#%module-begin #%module-begin]
             [snes-require require]))