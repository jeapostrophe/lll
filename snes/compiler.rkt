#lang racket/base
(require racket/list
         racket/match
         file/sha1
         racket/path
         racket/port)

(struct rom-layout (lo-rom? id name cartridge-type rom-size sram-size country-code license-code version slow-rom?
                             native-interrupt->label
                             rom-bank-size rom-banks empty-fill sections))
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
   lo-rom? id name cartridge-type rom-size sram-size country license-code version slow-rom?
   native-interrupt->label
   rom-bank-size rom-banks empty-fill
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
(define (hex v) (format "#x~a" (number->string v 16)))
(define (format-addr name addr use-addr kind)
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
         "~a: Absolute addr references do not share upper 8-bits: addr(~a:~a) and use(~a)"
         (hex (current-address)) name (hex addr) (hex use-addr))])]
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
               "Distance too far for relative addr: addr(~a:~a) from use(~a)" 
               name (hex addr) (hex use-addr))])]
    [x
     (error 'format-addr "Can't format addr(~a:~a) to ~e from use(~a)" 
            name (hex addr) x (hex use-addr))]))

(define (label-lookup! the-label use-addr kind)
  (match-define
   (and l (label actual refs))
   (hash-ref! (current-labels) the-label
              (λ () (label #f empty))))
  (cond
    [actual
     (format-addr the-label actual use-addr kind)]
    [else
     (set-label-references! l
                            (list* (label-reference use-addr kind)
                                   refs))
     (format-addr the-label 0 0 kind)]))
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

(struct label-use (name kind))
(define (write-label-use label kind)
  (define addr 
    (label-lookup! (label-use-name label)
                   (current-address)
                   kind))
  (write-bytes addr))

(define (current-address)
  (file-position (current-output-port)))

(define (write-bytes@ out pos bs)
  (file-position out pos)
  (write-bytes bs out))
(define (read-byte@ in pos)
  (file-position in pos)
  (read-byte in))

(define (16bit+ x y)
  (modulo (+ x y) (expt 2 16)))

(define current-debug (make-parameter #f))
(define (debug-opcode src)
  (fprintf (current-debug) "~a\t~a\n" (hex (current-address)) src))

(define (compile-rom pth rl)
  (match-define
   (struct* rom-layout ([lo-rom? lo-rom?]
                        [slow-rom? slow-rom?]
                        [id id]
                        [name name]
                        [rom-bank-size rom-bank-size]
                        [rom-banks rom-banks]
                        [empty-fill empty-fill]
                        [rom-size rom-size]
                        [sram-size sram-size]
                        [native-interrupt->label native-interrupt->label]
                        [country-code country-code]
                        [license-code license-code]
                        [version version]
                        [cartridge-type cartridge-type]
                        [sections these-sections]))
   rl)
  (define total-rom-size
    (* rom-bank-size 
       rom-banks))
  (define bank->start (make-hasheq))
  (for ([bank (in-range rom-banks)])
    (hash-set! bank->start bank (* bank rom-bank-size)))

  (define header-start (if lo-rom? #x7F00 #xFF00))
  
  (define labels (make-hasheq))
  (call-with-output-file pth
    #:mode 'binary #:exists 'replace
    (λ (out)
       ; Empty fill
      (for ([i (in-range total-rom-size)])
        (write-byte empty-fill out))

      ; Write header
      ;; XXX look at http://patpend.net/technical/snes/sneskart.html#embededcartridge
      (write-bytes@ out (+ header-start #xB2) id)
      (write-bytes@ out (+ header-start #xC0) name)
      (write-bytes@ out (+ header-start #xD5) (bytes (if slow-rom? #x20 #x00)))
      (write-bytes@ out (+ header-start #xD6) (bytes cartridge-type))
      (write-bytes@ out (+ header-start #xD7) (bytes rom-size))
      (write-bytes@ out (+ header-start #xD8) (bytes sram-size))
      (write-bytes@ out (+ header-start #xD9) (bytes country-code))
      (write-bytes@ out (+ header-start #xDA) (bytes license-code))
      (write-bytes@ out (+ header-start #xDB) (bytes version))

      (parameterize ([current-labels labels]
                     [current-output-port out])
        ;; Write interrupt tables
        (file-position out (+ header-start #xE4))
        (write-label-use (hash-ref native-interrupt->label 'COP) 'absolute)
        (write-label-use (hash-ref native-interrupt->label 'BRK) 'absolute)
        (write-label-use (hash-ref native-interrupt->label 'ABORT) 'absolute)
        (write-label-use (hash-ref native-interrupt->label 'NMI) 'absolute)
        (write-bytes (bytes #x00 #x000) out)
        (write-label-use (hash-ref native-interrupt->label 'IRQ) 'absolute)

        ;; Write sections
        (call-with-output-file
            (format "~a.debug" pth) #:exists 'replace
            (lambda (dout)
              (parameterize ([current-debug dout])
                (for ([s (in-list these-sections)])
                  (match-define (section name bank force? semi-free? thunk) s)
                  (define bank-start (hash-ref bank->start bank))
                  (file-position out bank-start)
                  (thunk)
                  (hash-set! bank->start bank (current-address))
                  (eprintf "Wrote ~a from ~a to ~a in bank ~a\n"
                           name (hex bank-start) (hex (current-address)) bank)))))

        ;; Rewrite labels
        (for ([(label-name l) (in-hash labels)])
          (match-define (label label-addr refs) l)
          (unless label-addr
            (error 'compile 
                   "Label ~e was never defined"
                   label-name))
          (for ([r (in-list refs)])
            (match-define (label-reference use-addr kind) r)
            (define written-addr (format-addr label-name label-addr use-addr kind))
            (eprintf "Rewrote ~a to use ~a (the ~a form of ~a which is called ~a)\n"
                     (hex use-addr) 
                     (bytes->hex-string written-addr)
                     kind
                     (hex label-addr)
                     label-name)
            (write-bytes@ out use-addr written-addr))))))

  ;; Discover the checksum (copied from WLALINK, compute.c)
  (define-values
    (inverse-checksum checksum)
    (call-with-input-file pth
      #:mode 'binary
      (λ (in)
         (define n #f)
         (define m #f)

         (if (total-rom-size . < . (* 512 1024))
             (begin (set! n total-rom-size)
                    (set! m 0))
             (error 'xxx))

         (define x 0)
         (for ([i (in-range 0 n)])
              (if lo-rom?
                  (unless (<= #x7FDC i #x7FDF)
                    (set! x (16bit+ x (read-byte@ in i))))
                  (unless (<= #xFFDC i #xFFDF)
                    (set! x (16bit+ x (read-byte@ in i))))))

         (set! x (16bit+ x (* 2 255)))

         (define l (bitwise-xor (bitwise-and x #xFFFF) #xFFFF))

         (values l x))))

  ;; Write the checksum
  (call-with-output-file pth
    #:mode 'binary #:exists 'update
    (λ (out)
     (write-bytes@ out (+ header-start #xDC)
                   (bytes-append (integer->integer-bytes inverse-checksum 2 #f)
                                 (integer->integer-bytes checksum 2 #f))))))

(provide (all-defined-out))
