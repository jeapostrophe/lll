#lang racket/base
(require (for-syntax syntax/parse
                     racket/syntax
                     racket/base)
         racket/list
         racket/contract
         racket/match)

;; This is an implementation of a machine similar to the 65816
;;; At the moment I don't plan on implementing the emulation modes

(define ((make-n-bit? n) x)
  (and (exact-nonnegative-integer? x)
       ((integer-length x) . <= . n)))

(define 16-bit? (make-n-bit? 16))
(define 24-bit? (make-n-bit? 24))

(define (bytes-of-24-bit-length? bs)
  (and (bytes? bs)
       (24-bit? (bytes-length bs))))

(provide/contract
 [simulate (-> bytes-of-24-bit-length?
               24-bit?
               24-bit?
               void)])

;; Library
(define (make-within start end)
  (λ (addr)
    (and (start . <= . addr)
         (addr . < . end))))

(define-syntax-rule (forever e ...)
  (let loop ()
    e ...
    (loop)))

(define (clamp-+ limit)
  (λ (x y)
    (modulo (+ x y) limit)))

(define 24bit+ (clamp-+ (expt 2 24)))
(define 16bit+ (clamp-+ (expt 2 16)))
(define 8bit+ (clamp-+ (expt 2 8)))

(define (8bit-8bit->16bit high low)
  (bitwise-ior (arithmetic-shift high 8) low))

;; Machine  
(define (simulate rom-bytes
                  ram-start ram-size)
  (let/ec esc
    ;; Setup memory
    (define rom-start 0)
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
    
    ;; Let's keep track of how many cycles have run
    (define CLOCK 0)
    
    ;; Setup registers
    (define A 0) ; Accumulator (16-bit, when not P.M)
    (define X 0) ; Index (16-bit, when not P.X)
    (define Y 0) ; Index (16-bit, when not P.Y)
    (define S 0) ; Stack Pointer (16-bit)
    ; XXX Unused right now
    (define DB 0) ; Data Bank (8-bit)
    ; XXX Unused right now
    (define DP 0) ; Direct Page (16-bit)
    ; XXX Unused right now
    (define PB 0) ; Program Bank (8-bit)
    ;;; These are pieces of P -- the Processor Status
    (define P.N #f) ; Negative
    (define P.V #f) ; Overflow
    (define P.Z #f) ; Zero
    (define P.C #f) ; Carry
    (define P.D #f) ; Decimal
    (define P.I #f) ; IRQ Disable
    (define P.X #f) ; Index register size [#f = 16bit]
    (define P.M #f) ; Accumulator register size [#f = 16bit]
    (define P.E #f) ; 6502 emulation mode
    (define P.B #f) ; Break
    (define PC rom-start) ; Program Counter (16-bit)
    
    ;;
    (define (show-state include-memory?)
      (list*
       (cons 'A A)
       (cons 'X X)
       (cons 'Y Y)
       (cons 'S S)
       (cons 'DB DB)
       (cons 'DP DP)
       (cons 'PB PB)
       (cons 'P (vector P.N P.V P.Z P.C P.D P.I P.X P.M P.E P.B))
       (cons 'PC PC)
       (if include-memory?
           (list (cons 'RAM ram-bytes))
           empty)))
     
    ;; Define opcodes
    ;;; XXX This is probably slow
    (define (do-opcode c)
      (error 'opcode "Unimplemented: ~v\n" c))
    (define-syntax-rule (install-opcode! c ? t)
      (let ([old-do-opcode do-opcode])
        (set! do-opcode
              (λ (some-c)
                (if (and (= c some-c) ?)
                    (t)
                    (old-do-opcode some-c))))))
    
    (define-syntax (define-opcode stx)
      (syntax-parse 
       stx
       [(_ mnemonic:id opcode:nat desc:str args:nat cycles:nat
           (~optional (~seq #:side-condition side-condition:expr)
                      #:defaults ([side-condition #'#t]))
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
                ; XXX fix PC manipulation
                (define $arg (mem-ref (16bit+ PC argn)))
                ...
                (set! PC (16bit+ PC args))
                e ...
                (set! CLOCK (+ CLOCK cycles)))
              (install-opcode! opcode side-condition mnemonic))))]))
    
    ;; Define a set of opcodes that differ only in addressing mode
    (define-syntax (define-opcode-set stx)
      (syntax-parse
       stx
       [(_ mnemonic:id desc:str
           ([addressing-mode:id 
             opcode:nat args:nat cycles:nat
             (~optional side-condition:expr
                        #:defaults ([side-condition #'#t]))]
            ...)
           e:expr ...)
        (with-syntax
            ([core:mnemonic (generate-temporary)]
             [$value (format-id #'mnemonic "$value")]
             [$addr (format-id #'mnemonic "$addr")]
             [(mnemonic:addressing-mode ...) 
              (for/list 
                  ([am (in-list (syntax->list
                                 #'(addressing-mode ...)))])
                (format-id am "~a:~a" #'mnemonic am
                           #:source am))]
             [(($i ...) ...)
              (for/list 
                  ([am (in-list (syntax->list
                                 #'(addressing-mode ...)))]
                   [args (in-list (syntax->list
                                   #'(args ...)))])
                (for/list ([i (in-range (syntax->datum args))])
                  (format-id #'mnemonic "$~a" i #:source #'args)))])
          (syntax/loc stx
            (begin 
              (define (core:mnemonic $addr $value)
                e ...)
              (define-opcode mnemonic:addressing-mode opcode 
                desc args cycles
                #:side-condition side-condition
                (call-with-values (λ () (addressing-mode $i ...))
                                  core:mnemonic))
              ...)))]))
    
    ;; Define addressing modes
    ;;; addressing-mode : opcode args -> addr x value
    (define-syntax-rule (define-unimplemented-mode id)
      (define (id . args) (error 'id "Unimplemented: ~v" args)))
    (define-syntax-rule (define-unimplemented-modes id ...)
      (begin (define-unimplemented-mode id) ...))
    (define-unimplemented-modes 
      direct-page:indirect-indexed:x
      stack-relative
      direct-page
      direct-page:indirect-long
      absolute
      absolute-long
      direct-page:index-indexed:y
      direct-page:indirect
      stack-relative:indirect-indexed:y
      direct-page:indexed:x
      direct-page:indirect-long-indexed:y
      absolute-indexed:y
      absolute-indexed:x
      absolute-long-indexed:x)
    
    (define (accumulator $0)
      (values #f A))
    (define (immediate-long $0 $1 $2)
      (values #f (8bit-8bit->16bit $1 $2)))
    (define (immediate-short $0 $1)
      (values #f $1))
    
    ;; And here are the opcodes ...   
    (define-opcode-set ADC "Add With Carry"
      ([direct-page:indirect-indexed:x #x61 2 6]
       [stack-relative #x63 2 4]
       [direct-page #x65 2 3]
       [direct-page:indirect-long #x67 2 6]
       [immediate-short #x69 2 2 P.M]
       [immediate-long #x69 3 2 (not P.M)]
       [absolute #x6D 3 4]
       [absolute-long #x6F 4 5]
       [direct-page:index-indexed:y #x71 2 5] ; XXX cycle note
       [direct-page:indirect #x72 2 5]
       [stack-relative:indirect-indexed:y #x73 2 7]
       [direct-page:indexed:x #x75 2 4]
       [direct-page:indirect-long-indexed:y #x77 2 6]
       [absolute-indexed:y #x79 3 4]
       [absolute-indexed:x #x7D 3 4]
       [absolute-long-indexed:x #x7F 4 5])
      ; XXX Do the carrying
      (set! A (16bit+ A $value)))
    
    (define-opcode-set AND "AND Accumulator With Memory"
      ([direct-page:indirect-indexed:x #x21 2 6]
       [stack-relative #x23 2 4]
       [direct-page #x25 2 3]
       [direct-page:indirect-long #x27 2 6]
       [immediate-short #x29 2 2 P.M]
       [immediate-long #x29 3 2 (not P.M)]
       [absolute #x2D 3 4]
       [absolute-long #x2F 4 5]
       [direct-page:index-indexed:y #x31 2 5]
       [direct-page:indirect #x32 2 5]
       [stack-relative:indirect-indexed:y #x33 2 7]
       [direct-page:indexed:x #x35 2 4]
       [direct-page:indirect-long-indexed:y #x37 2 6]
       [absolute-indexed:y #x39 3 4]
       [absolute-indexed:x #x3D 3 4]
       [absolute-long-indexed:x #x3F 4 5])
      (set! A (bitwise-and A $value)))
    
    (define-opcode-set ASL "Arithmetic Shift Left"
      ([direct-page #x06 2 5] ; XXX cycle note
       [accumulator #x0A 1 2]
       [absolute #x0E 3 6]
       [direct-page:indexed:x #x16 2 6]
       [absolute-indexed:x #x1E 3 7])
      (if $addr
          (mem-set! $addr (arithmetic-shift $value 1))
          (set! A (arithmetic-shift A 1))))
    
    (define-opcode STP #xDB
      "Stop Processor" 1 0
      (esc (show-state #t)))
    (define-opcode NOP #xEA
      "No Operation" 1 2
      (void))
    
    ;; Loop
    (forever
     (printf "~v\n" (show-state #f))
     (do-opcode (mem-ref PC)))))

; XXX turn into tests
(printf "ADC\n")
(simulate
 (bytes #x69 0 5
        #x69 0 5
        #x69 0 10
        #x69 0 200
        #x69 0 25
        #x69 0 25
        #x69 #xff #xff
        #xEA
        #xEA
        #xEA
        #xEA
        #xDB)
 #x800000 0)

(printf "AND\n")
(simulate
 (bytes #x69 0 1
        #x29 0 1
        #x69 0 2
        #x29 0 1
        #xDB)
 #x800000 0)

(printf "ASL\n")
(simulate
 (bytes #x69 0 1
        #x0A
        #x0A
        #xDB)
 #x800000 0)