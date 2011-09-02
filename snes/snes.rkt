#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
         racket/match
         racket/list
         "compiler.rkt")

(define ((make-nbit-number? n) x)
  (and (number? x)
       (< x (expt 2 n))))
(define 24bit-number? (make-nbit-number? 24))
(define 16bit-number? (make-nbit-number? 16))
(define 8bit-number? (make-nbit-number? 8))

(struct addr (constant))

(define anonymous-labels (make-hasheq))
(define (*label-use n [kind 'default])
  (define effective-n
    (match n
      ['+
       (hash-ref! anonymous-labels '+ (lambda () (gensym '+)))]
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
       (define next (gensym '+))
       (begin0
         (hash-ref anonymous-labels '+
                   (λ () 
                      (error 'label-ref
                             "Definition of + without previous use")))
         (hash-set! anonymous-labels '+ next))]
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

(define (data bs)
  (write-bytes bs))

(define-syntax (define-opcode* stx)
  (syntax-parse
   stx
   [(_ (op:id arg:id ...) body:expr ...)
    (syntax/loc stx
      (define-opcode* op (lambda (arg ...) body ...)))]
   [(_ op:id val:expr)
    (syntax/loc stx
      (define-opcode* op op val))]
   [(_ op:id out-op:id val:expr)
    (syntax/loc stx
      (begin
        (define inner-op val)
        (define (wrap-op stx . args)
          (debug-opcode stx)
          (apply inner-op args))
        (define-syntax (op stx)
          (syntax-case stx ()
            [(_ . args)
             (quasisyntax/loc stx
               (wrap-op #'#,stx . args))]))
        (provide (rename-out [op out-op]))))]))

(define-syntax (define-opcode stx)
  (syntax-parse
   stx
   [(_ (op:id arg:id ...) opcode:expr cycles:nat
       e:expr ...)
    (syntax/loc stx
      ;; XXX record cycles
      (define-opcode* (op arg ...)
        (write-byte opcode)
        e ...))]))

(define (write-absolute-label-or-const arg)
  (match arg
    [(addr (? 16bit-number? ad))
     (write-bytes (integer->integer-bytes ad 2 #f))]
    [(? label-use? lab)
     (write-label-use lab 'absolute)]))
(define (write-long-label-or-const arg)
  (match arg
    [(addr (? 24bit-number? ad))
     (write-bytes
      (bytes (bitwise-bit-field ad 0 8)
             (bitwise-bit-field ad 8 16)
             (bitwise-bit-field ad 16 24)))]
    [(? label-use? lab)
     (write-label-use lab 'long)]))
(define (write-label-or-const arg)
  (match arg
    [(addr (? 16bit-number? ad))
     (write-absolute-label-or-const arg)]
    [(addr (? 24bit-number? ad))
     (write-long-label-or-const arg)]
    [(? label-use? lab)
     (write-label-use lab (label-use-kind lab))]))

; XXX Some of these may be long, or otherwise exotic
(define (make-lda-like const-opcode addr-opcode long-opcode #:8bit? [8bit? #f])
  (λ (arg)
    (cond
      ; XXX This may be wrong?
      [(and 8bit? (8bit-number? arg))
       (write-byte const-opcode)
       (write-byte arg)]
      [(16bit-number? arg)
       (write-byte const-opcode)
       (write-bytes (integer->integer-bytes arg 2 #f))]
      [(number? arg)
       (error 'lda "Argument too big: ~e/~e" 
              arg
              (number->string arg 16))]
      [(and (label-use? arg) (eq? 'bank (label-use-kind arg)))
       (write-byte const-opcode)
       (write-label-use arg 'bank)]
      [(and (label-use? arg) (eq? '& (label-use-kind arg)))
       (write-byte const-opcode)
       (write-label-use arg '&)]
      [(and long-opcode (label-use? arg) (eq? 'long (label-use-kind arg)))
       (write-byte long-opcode)
       (write-label-use arg 'long)]
      [else
       (write-byte addr-opcode)
       (write-absolute-label-or-const arg)])))
(define (write-near-label near-label)
  (define near-addr 
    (label-lookup! (label-use-name near-label)
                   (current-address)
                   'relative))  
  (write-bytes near-addr))

(define-opcode* adc (make-lda-like #x69 #x6D #x6F))
(define-opcode* OP:and and (make-lda-like #x29 #x2D #x2F #:8bit? #t))
(define-opcode (asl/A) #x0A 2)
(define-opcode (asl arg) #x0E 6
  (write-absolute-label-or-const arg))
(define-opcode (beq near-label) #xF0 2
  (write-near-label near-label))
(define-opcode (bne near-label) #xD0 2
  (write-near-label near-label))
(define-opcode (clc) #x18 2)
(define-opcode (cld) #xD8 2)
(define-opcode (cli) #x58 2)
(define-opcode (clv) #xB8 2)
(define-opcode* cmp (make-lda-like #xC9 #xCD #xCF #:8bit? #t))
; XXX cycle/byte note
; XXX can use addresses
(define-opcode (cpx constant) #xE0 2
  (write-byte constant))
(define-opcode (cpx.l constant) #xE0 2
  (write-bytes (integer->integer-bytes constant 2 #f)))
(define-opcode (dec arg) #xCE 6
  (write-absolute-label-or-const arg))
(define-opcode (dex) #xCA 2)
(define-opcode (dey) #x88 2)
(define-opcode* eor (make-lda-like #x49 #x4D #x4F))
(define-opcode (ina) #x1A 2)
(define-opcode (inc arg) #xEE 6
  (write-absolute-label-or-const arg))
(define-opcode (inx) #xE8 2)
(define-opcode (iny) #xC8 2)
; XXX Some uses of this in the code may actually be JML
(define-opcode (jmp label) #x4C 3
  (write-label-use label 'absolute))
(define-opcode (jsl arg) #x22 8
  (write-long-label-or-const arg))
(define-opcode (jsr arg) #x20 6
  (write-absolute-label-or-const arg))
(define-opcode* lda (make-lda-like #xA9 #xAD #xAF #:8bit? #t))
(define-opcode (lda.w arg) #xA9 4
  (write-bytes (integer->integer-bytes arg 2 #f)))
(define-opcode (lda/DP/X arg) #xB5 4
  (write-byte arg))
(define-opcode (lda/X arg) #xBD 4
  (write-label-or-const arg))
(define-opcode (lda.l/X arg) #xBF 4
  (write-long-label-or-const arg))
(define-opcode* ldx (make-lda-like #xA2 #xAE #f))
(define-opcode* ldy (make-lda-like #xA0 #xAC #f))
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
(define-opcode (rep const) #xC2 3
  (write-byte const))
(define-opcode (rti) #x40 6)
(define-opcode (rtl) #x6B 6)
(define-opcode (rts) #x60 6)
(define-opcode (sei) #x78 2)
(define-opcode (sep imm) #xE2 3
  (write-byte imm))
(define-opcode (sta arg) #x8D 4
  (write-absolute-label-or-const arg))
(define-opcode (sta/DP/X arg) #x95 4
  (write-byte arg))
(define-opcode (sta/X arg) #x9D 5
  (write-absolute-label-or-const arg))
(define-opcode (stx arg) #x8E 4
  (write-absolute-label-or-const arg))
(define-opcode (stx/DP arg) #x86 3
  (write-byte arg))
(define-opcode (sty arg) #x8C 4
  (write-absolute-label-or-const arg))
(define-opcode (stz arg) #x9C 4
  (write-absolute-label-or-const arg))
(define-opcode (stz/DP/X arg) #x74 5
  (write-byte arg))
(define-opcode (stz/X arg) #x9E 5
  (write-absolute-label-or-const arg))
(define-opcode (tax) #xAA 2)
(define-opcode (tcd) #x5B 2)
(define-opcode (txs) #x9A 2)
; XXX cycle notess
(define-opcode (wai) #xCB 3)
(define-opcode (xba) #xEB 3)
(define-opcode (xce) #xFB 2)

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
        (~optional (~seq #:force
                         (~bind [force? #'#t]))
                   #:defaults ([force? #'#f]))
        (~optional (~seq #:semi-free
                         (~bind [semi-free? #'#t]))
                   #:defaults ([semi-free? #'#f]))) 
       ...
       . e)
    (syntax/loc stx
      (define name
        (make-section 
         'name 
         #:bank bank
         #:force? force?
         #:semi-free? semi-free?
         (λ () 
           (parameterize
               ([current-back-references (box empty)]
                [current-forward-references (box empty)])
             . e)))))]))


(provide 
 ; From racket/base
 #%module-begin require provide define #%datum #%app
 all-defined-out quote bytes * +
 quasiquote unquote empty list hasheq
 ; From this
 define-section repeat label-ref addr data make-rom
 (rename-out [snes-label label]))
