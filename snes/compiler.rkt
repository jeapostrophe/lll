#lang racket/base
(require racket/list
         racket/match
         racket/path
         racket/port)

(struct rom-layout (rom-bank-size rom-banks props sections))
(define (make-rom #:rom-bank-size rom-bank-size
                  #:rom-banks rom-banks
                  #:id id ; XXX 1-4 letter string
                  #:name name ; XXX 21 bytes, exactly
                  #:slow-rom? [slow-rom? #f]
                  #:lo-rom? [lo-rom? #f]
                  #:cartridge-type [cartridge-type #x00]
                  #:rom-size rom-size
                  #:sram-size sram-size
                  #:country country
                  #:license-code license-code
                  #:version version
                  #:native-interrupts native-interrupt->label
                  #:emulation-interrupts emulation-interrupt->label
                  #:empty-fill [empty-fill #x00]
                  #:sections [sections empty])
  (rom-layout 
   rom-bank-size rom-banks
   (hasheq 'id id
           'name name
           'slow-rom? slow-rom?
           'lo-rom? lo-rom?
           'cartridge-type cartridge-type
           'rom-size rom-size
           'sram-size sram-size
           'country country
           'license-code license-code
           'version version
           'native-interrupts native-interrupt->label
           'emulation-interrupts emulation-interrupt->label
           'empty-fill empty-fill)
   sections))

(struct section (name bank force? semi-free? thunk)
        #:transparent)

(define (make-section name 
                      #:bank bank #:force? force? #:semi-free? semi-free?
                      thunk)
  (section name bank force? semi-free? thunk))

(define current-labels (make-parameter #f))
(struct label (actual-address references)
        #:transparent
        #:mutable)
(struct label-reference (use-address kind)
        #:transparent)

; Addresses should be 24 bits
(define HIGH-BITS #b111111110000000000000000)
(define MIDL-BITS #b000000001111111100000000)
(define LOWR-BITS #b000000000000000011111111)
(define (format-addr addr use-addr kind)
  (match kind
    ['long
     (bytes (bitwise-bit-field addr 16 24)
            (bitwise-bit-field addr 8 16)
            (bitwise-bit-field addr 0 8))]
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

(define (current-address)
  (file-position (current-output-port)))

(define (compile-rom pth rl)
  (match-define (rom-layout rom-bank-size rom-banks props these-sections) rl)
  (define total-rom-size
    (* rom-bank-size 
       rom-banks))
  (define bank->start (make-hasheq))
  (for ([bank (in-range rom-banks)])
    (hash-set! bank->start bank (* bank rom-bank-size)))
    
  (define labels (make-hasheq))
  (call-with-output-file pth
    #:mode 'binary #:exists 'replace
    (λ (out)
      (for ([i (in-range total-rom-size)])
        (write-byte (hash-ref props 'empty-fill) out))
      
      (parameterize ([current-labels labels])
        ; Write sections
        (parameterize ([current-output-port out])
          (for ([s (in-list these-sections)])
            (match-define (section name bank force? semi-free? thunk) s)
            (define bank-start (hash-ref bank->start bank))
            (file-position out bank-start)
            (thunk)
            (hash-set! bank->start bank (current-address))))
        ; Rewrite labels
        (for ([(label-name l) (in-hash labels)])
          (match-define (label label-addr refs) l)
          (unless label-addr
            (error 'compile 
                   "Label ~e was never defined"
                   label-name))
          (for ([r (in-list refs)])
            (match-define (label-reference use-addr kind) r)
            (file-position out use-addr)
            (write-bytes (format-addr label-addr use-addr kind) out))))
      
      (flush-output out))))

(provide (all-defined-out))
