#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
         racket/stxparam
         racket/splicing
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

(define (*label-use n [kind 'default])
  (label-use n kind))

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
      (label-define! 'name (current-address)))]))

(define (data* stx bs)
  (debug-opcode stx)
  (write-bytes bs))
(define-syntax (data stx)
  (syntax-case stx ()
   [(_ e)
    (quasisyntax/loc stx
      (data* #'#,stx e))]))

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

(define-opcode* ADC (make-lda-like #x69 #x6D #x6F))
(define-opcode* AND (make-lda-like #x29 #x2D #x2F #:8bit? #t))
(define-opcode (ASL/A) #x0A 2)
(define-opcode (ASL arg) #x0E 6
  (write-absolute-label-or-const arg))
(define-opcode (BEQ near-label) #xF0 2
  (write-near-label near-label))
(define-opcode (BNE near-label) #xD0 2
  (write-near-label near-label))
(define-opcode (CLC) #x18 2)
(define-opcode (CLD) #xD8 2)
(define-opcode (CLI) #x58 2)
(define-opcode (CLV) #xB8 2)
(define-opcode* CMP (make-lda-like #xC9 #xCD #xCF #:8bit? #t))
; XXX cycle/byte note
; XXX can use addresses
(define-opcode (CPX constant) #xE0 2
  (write-byte constant))
(define-opcode (CPX.l constant) #xE0 2
  (write-bytes (integer->integer-bytes constant 2 #f)))
(define-opcode (DEC arg) #xCE 6
  (write-absolute-label-or-const arg))
(define-opcode (DEX) #xCA 2)
(define-opcode (DEY) #x88 2)
(define-opcode* EOR (make-lda-like #x49 #x4D #x4F))
(define-opcode (INA) #x1A 2)
(define-opcode (INC arg) #xEE 6
  (write-absolute-label-or-const arg))
(define-opcode (INX) #xE8 2)
(define-opcode (INY) #xC8 2)
; XXX Some uses of this in the code may actually be JML
(define-opcode (JMP label) #x4C 3
  (write-label-use label 'absolute))
(define-opcode (JSL arg) #x22 8
  (write-long-label-or-const arg))
(define-opcode (JSR arg) #x20 6
  (write-absolute-label-or-const arg))
(define-opcode* LDA (make-lda-like #xA9 #xAD #xAF #:8bit? #t))
(define-opcode (LDA.w arg) #xA9 4
  (write-bytes (integer->integer-bytes arg 2 #f)))
(define-opcode (LDA/DP/X arg) #xB5 4
  (write-byte arg))
(define-opcode (LDA/X arg) #xBD 4
  (write-label-or-const arg))
(define-opcode (LDA.l/X arg) #xBF 4
  (write-long-label-or-const arg))
(define-opcode* LDX (make-lda-like #xA2 #xAE #f))
(define-opcode* LDY (make-lda-like #xA0 #xAC #f))
(define-opcode (PHA) #x48 3)
(define-opcode (PHB) #x8B 3)
(define-opcode (PHD) #x0B 4)
(define-opcode (PHK) #x4B 3)
(define-opcode (PHP) #x08 3)
(define-opcode (PHX) #xDA 3)
(define-opcode (PHY) #x4A 3)
(define-opcode (PLA) #x68 4)
(define-opcode (PLB) #xAB 4)
(define-opcode (PLD) #x2B 5)
(define-opcode (PLP) #x28 4)
(define-opcode (PLX) #xFA 4)
(define-opcode (PLY) #x7A 4)
(define-opcode (REP const) #xC2 3
  (write-byte const))
(define-opcode (RTI) #x40 6)
(define-opcode (RTL) #x6B 6)
(define-opcode (RTS) #x60 6)
(define-opcode (SEI) #x78 2)
(define-opcode (SEP imm) #xE2 3
  (write-byte imm))
(define-opcode (STA arg) #x8D 4
  (write-absolute-label-or-const arg))
(define-opcode (STA/DP/X arg) #x95 4
  (write-byte arg))
(define-opcode (STA/X arg) #x9D 5
  (write-absolute-label-or-const arg))
(define-opcode (STX arg) #x8E 4
  (write-absolute-label-or-const arg))
(define-opcode (STX/DP arg) #x86 3
  (write-byte arg))
(define-opcode (STY arg) #x8C 4
  (write-absolute-label-or-const arg))
(define-opcode (STZ arg) #x9C 4
  (write-absolute-label-or-const arg))
(define-opcode (STZ/DP/X arg) #x74 5
  (write-byte arg))
(define-opcode (STZ/X arg) #x9E 5
  (write-absolute-label-or-const arg))
(define-opcode (TAX) #xAA 2)
(define-opcode (TCD) #x5B 2)
(define-opcode (TXS) #x9A 2)
; XXX cycle notess
(define-opcode (WAI) #xCB 3)
(define-opcode (XBA) #xEB 3)
(define-opcode (XCE) #xFB 2)

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
           . e))))]))

;; XXX gensym -> generate-temporary
(define-syntax (DO-WHILE stx)
  (syntax-parse
   stx
   [(_ body:expr ... branch:id)
    (with-syntax ([lab (gensym)])
      (syntax/loc stx
        (begin (snes-label lab)
               body ...
               (branch (label-ref lab)))))]))

(define-syntax (UNLESS stx)
  (syntax-parse
   stx
   [(_ branch:id body:expr ...)
    (with-syntax ([lab (gensym)])
      (syntax/loc stx
        (begin (branch (label-ref lab))
               body ...
               (snes-label lab))))]))

(define-syntax-parameter BREAK
  (lambda (stx) (raise-syntax-error 'BREAK "Only allowed inside with-break" stx))) 
(define-syntax (with-break stx)
  (syntax-parse
   stx
   [(_ body:expr ...)
    (with-syntax ([lab (gensym)])
      (syntax/loc stx
        (splicing-syntax-parameterize
         ([BREAK
           (syntax-rules ()
             [(_) (JMP (label-ref lab))])])
         (begin body ...
                (snes-label lab)))))]))

(provide 
 define-section label-ref addr data make-rom
 (rename-out [snes-label label])
 with-break BREAK
 DO-WHILE UNLESS)
