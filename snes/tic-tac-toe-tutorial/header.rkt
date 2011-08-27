#lang s-exp "../snes.rkt"
(provide (all-defined-out))
;==LoRom==      ; We'll get to HiRom some other time.

(define-memory-map ; Begin describing the system architecture.
  #:slot-size #x8000                ; The slot is #x8000 bytes in size. More details on slots later.
  #:default-slot 0                 ; There's only 1 slot in SNES, there are more in other consoles.
  #:slot-start [0 #x8000])                  ; Define's Slot 0's starting address.

(define-rom-bank-size #x8000)              ; Every ROM bank is 32 KBytes in size
(define-rom-banks 8)                     ; 2 Mbits - Tell WLA we want to use 8 ROM Banks

(define-snes-header
  #:id "SNES"                     ; 1-4 letter string, just leave it as "SNES"
  
  #:name "A small game         "  ; Program Title - can't be over 21 bytes,
  ;    "123456789012345678901"  ; use spaces for unused bytes of the name.
  
  #:slow-rom
  #:lo-rom
  
  #:cartridge-type #x00             ; #x00 = ROM only, see WLA documentation for others
  #:rom-size #x08                   ; #x08 = 2 Mbits,  see WLA doc for more..
  #:sram-size #x00                  ; No SRAM         see WLA doc for more..
  #:country #x01                   ; #x01 = U.S.  #x00 = Japan, that's all I know
  #:license-code #x00              ; Just use #x00
  #:version #x00)                   ; #x00 = 1.00, #x01 = 1.01, etc.

(define-native-vector               ; Define Native Mode interrupt vector table
  #:COP (label-ref EmptyHandler)
  #:BRK (label-ref EmptyHandler)
  #:ABORT (label-ref EmptyHandler)
  #:NMI (label-ref VBlank)
  #:IRQ (label-ref EmptyHandler))

(define-emulation-vector       ; Define Emulation Mode interrupt vector table
  #:COP (label-ref EmptyHandler)
  #:ABORT (label-ref EmptyHandler)
  #:NMI (label-ref VBlank)
  #:RESET (label-ref Start)                   ; where execution starts
  #:IRQBRK (label-ref EmptyHandler))

(define-section EmptyVectors
  #:bank 0 #:slot 0 ; Defines the ROM bank and the slot it is inserted in memory.
  #:org 0 ; .ORG 0 is really #x8000, because the slot starts at #x8000
  #:semi-free
  (label EmptyHandler)
  (rti))

(define-empty-fill #x00)                  ; fill unused areas with #x00, opcode for BRK.  
; BRK will crash the snes if executed.
