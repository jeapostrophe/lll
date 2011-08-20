#lang racket/base
(require (for-syntax syntax/parse
                     racket/syntax
                     racket/base)
         racket/contract
         racket/match)

;; This is an implementation of a machine similar to the 65816
(define (16-bit? x)
  (and (exact-nonnegative-integer? x)
       ((integer-length x) . <= . 16)))

(define (bytes-of-16-bit-length? bs)
  (and (bytes? bs)
       (16-bit? (bytes-length bs))))

(provide/contract
 [simulate (-> bytes-of-16-bit-length?
               16-bit?
               16-bit?
               16-bit?
               void)])

;; Library
(define (make-within start end)
  (λ (addr)
    (<= start addr end)))

(define-syntax-rule (forever e ...)
  (let loop ()
    e ...
    (loop)))

(define (clamp-+ limit)
  (λ (x y)
    (modulo (+ x y) limit)))

;; Machine  
(define (simulate rom-bytes rom-start
                  ram-start ram-size)
  (let/ec esc
    ;; XXX Not implementing banking
    ;; Setup memory
    (define rom-size (bytes-length rom-bytes))
    (define rom-end (+ rom-start rom-size))
    (define in-rom? (make-within rom-start rom-end))
    (define ram-end (+ ram-start ram-size))
    (define in-ram? (make-within ram-start ram-end))
    (define ram-bytes (make-bytes ram-size))
    (define (mem-ref addr)
      (match addr
        [(? in-ram?)
         (bytes-ref ram-bytes (- addr ram-start))]
        [(? in-rom?)
         (bytes-ref rom-bytes (- addr rom-start))]))
    (define (mem-set! addr b)
      (match addr
        [(? in-ram?)
         (bytes-set! ram-bytes (- addr ram-start) b)]
        [(? in-rom?)
         (error 'mem-set! "ROM is read-only")]))
    
    ;; Ensure we implement 8-bit register, 16-bit semantics correctly
    (define addr-add (clamp-+ (expt 2 16)))
    (define byte-add (clamp-+ (expt 2 8)))

    ;; Let's keep track of how many cycles have run
    (define CLOCK 0)

    ;; Setup registers
    (define A 0)
    (define X 0)
    (define Y 0)
    (define S 0)
    (define DB 0)
    (define DP 0)
    (define PB 0)
    (define P 0)
    (define PC rom-start)
    
    ;; Define opcodes
    (define *opcodes* (make-vector 256 NOP))
    (define (do-opcode c) ((vector-ref *opcodes* c)))
    (define (install-opcode! c t) (vector-set! *opcodes* c t))
    
    (define-syntax (define-opcode stx)
      (syntax-parse 
       stx
       [(_ mnemonic:id opcode:nat desc:str args:nat cycles:nat
           e:expr ...)
        (with-syntax 
            ([($arg ...)
              (for/list ([i (in-range (syntax->datum #'args))])
                (format-id #'mnemonic "$~a" i #:source #'args))]
             [(argn ...)
              (for/list ([i (in-range (syntax->datum #'args))])
                i)])
          (syntax/loc stx
            (begin
              (define (mnemonic)
                (define $arg (mem-ref (addr-add PC argn)))
                ...
                (set! PC (addr-add PC args))
                e ...
                (set! CLOCK (+ CLOCK cycles)))
              (install-opcode! opcode mnemonic))))]))
    
    ;; And here they are...   
    (define-opcode ADC/immediate #x69
      "Add with Carry (Immediate)" 2 2
      ; XXX Do the carrying
      (set! A (byte-add A $1)))
    (define-opcode STP #xDB
      "Stop Processor" 1 0
      (esc (void)))
    (define-opcode NOP #xEA
      "No Operation" 1 2
      (void))
    
    ;; Loop
    (forever
     (printf "~a: A: ~a X: ~a Y: ~a S: ~a PC: ~a\n"
             CLOCK A X Y S PC)
     (do-opcode (mem-ref PC)))))

(simulate
 (bytes #x69 5
        #x69 5
        #x69 10
        #x69 200
        #x69 25
        #x69 25
        #xEA
        #xEA
        #xEA
        #xEA
        #xDB)
 0 
 #x800000 0)