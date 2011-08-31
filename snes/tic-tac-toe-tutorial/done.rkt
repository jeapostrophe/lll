#lang s-exp "../snes.rkt"
(require "InitSNES.rkt"
         "tiles.rkt")
(provide ROM)

(define-section EmptyVectors
  #:bank 0 #:semi-free
  (label EmptyHandler)
  (rti))

(define (ConvertX)
  ; Data in: our coord in A
  ; Data out: SNES scroll data in C (the 16 bit A)
  (repeat 5
          (asl/A))		; multiply A by 32
  (rep #b00100000)	; 16 bit A
  (eor #xFFFF)	; this will do A=1-A
  (ina)		; A=A+1
  (sep #b00100000))	; 8 bit A

(define (ConvertY)
  ; Data in: our coord in A
  ; Data out: SNES scroll data in C (the 16 bit A)
  (repeat 5
          (asl/A))		; multiply A by 32
  (rep #b00100000)	; 16 bit A
  (eor #xFFFF)	; this will do A=1-A
  (sep #b00100000))	; 8 bit A

(define-section Vblank
  #:bank 0
  
  ;--------------------------------------
  (label VBlank)
  (lda (addr #x4212))	; get joypad status
  (and #b00000001)	; if joy is not ready
  (bne (label-ref VBlank))	; wait
  (lda (addr #x4219))	; read joypad (BYSTudlr)
  (sta (addr #x0201))	; store it
  (cmp (addr #x0200))	; compare it with the previous
  (bne (label-ref +))		; if not equal, go
  (rti)		; if it's equal, then return
  
  (label +)
  (sta (addr #x0200))	; store
  (and #b00010000)	; get the start button
  ; this will be the delete key
  (beq (label-ref +))		; if it's 0, we don't have to delete
  (ldx #x0000)
  (label -)
  (stz/X (addr #x0000))	; delete addresses $0000 to $0008
  (inx)
  (cpx #x09)	; this is 9. Guess why (homework :) )
  (bne (label-ref -))
  (stz (addr #x0100))	; delete the scroll
  (stz (addr #x0101))	; data also
  
  (label +)
  (lda (addr #x0201))	; get back the temp value
  (and #b11000000)	; Care only about B and Y
  (beq (label-ref +))		; if empty, skip this
  ; so, B or Y is pressed. Let's say B is O,
  ; and Y is X.
  (cmp #b11000000)	; both are pressed?
  (beq (label-ref +))		; then don't do anything
  (cmp #b10000000)	; B?
  (bne (label-ref +))		; no, try Y
  ; B is pressed, write an O ($08)
  ; we have to tell the cursor position,
  ; and calculate an address from that
  ; Formula: Address=3*Y+X
  (lda (addr #x0101))	; get Y
  (sta (addr #x0202))	; put it to a temp value
  (clc)
  (adc (addr #x0202))	; multiply by 3 - an easy way
  (adc (addr #x0202))	; A*3=A+A+A :)
  (adc (addr #x0100))	; add X
  ; Now A contains our address
  (ldx #x0000)	; be on the safe side
  (tax)
  (lda #x08)
  (sta/X (addr #x0000))	; put $08 to the good address
  (jmp (label-ref +))		; done with this
  
  (label ++)		; now for Y
  (cmp #b01000000)	; Y?
  (bne (label-ref +))		; no, jump forward (this should not happen)
  ; Y is pressed, write an X ($0A)
  (lda (addr #x0101))	; get Y
  (sta (addr #x0202))	; put it to a temp value
  (clc)
  (adc (addr #x0202))	; multiply by 3 - an easy way
  (adc (addr #x0202))	; A*3=A+A+A :)
  (adc (addr #x0100))	; add X
  ; Now A contains our address
  (ldx (addr #x0000))	; be on the safe side
  (tax)
  (lda #x0A)
  (sta/X (addr #x0000))	; put $0A to the good address
  (label +)		; finished putting tiles
  
  ; cursor moving comes now
  (lda (addr #x0201))	; get control
  (and #b00001111)	; care about directions
  (sta (addr #x0201))	; store this
  
  (cmp #b00001000)	; up?
  (bne (label-ref +))		; if not, skip
  (lda (addr #x0101))	; get scroll Y
  (cmp #x00)	; if on the top,
  (beq (label-ref +))		; don't do anything
  (dec (addr #x0101))	; sub 1 from Y
  (label +)
  
  (lda (addr #x0201))	; get control
  (cmp #b00000100)	; down?
  (bne (label-ref +))		; if not, skip
  (lda (addr #x0101))
  (cmp #x02)	; if on the bottom,
  (beq (label-ref +))		; don't do anything
  (inc (addr #x0101))	; add 1 to Y
  (label +)
  
  (lda (addr #x0201))	; get control
  (cmp #b00000010)	; left?
  (bne (label-ref +))		; if not, skip
  (lda (addr #x0100))
  (cmp #x00)	; if on the left,
  (beq (label-ref +))		; don't do anything
  (dec (addr #x0100))	; sub 1 from X
  (label +)
  
  (lda (addr #x0201))	; get control
  (cmp #b00000001)	; right?
  (bne (label-ref +))		; if not, skip
  (lda (addr #x0100))
  (cmp #x02)	; if on the right,
  (beq (label-ref +))		; don't do anything
  (inc (addr #x0100))	; add 1 to X
  (label +)
  (rti))		; F|NisH3D!
;--------------------------------------

(define-section Main
  #:bank 0 
  ;--------------------------------------
  
  (label Start)
  (InitSNES)
  (rep #b00010000)	;16 bit xy
  (sep #b00100000)	;8 bit ab
  
  (ldx #x0000)
  (label -)
  (lda/X (label-ref UntitledPalette 'long))
  (sta (addr #x2122))
  (inx)
  (cpx 8)
  (bne (label-ref -))
  
  ;I'll explain this later
  ;We'll have two palettes, only one color is needed for the second:
  (lda 33)		;The color we need is the 33rd
  (sta (addr #x2121))
  (lda.l (label-ref Palette2))
  (sta (addr #x2122))
  (lda.l (label-ref Palette2PlusOne) #;(+ (label-ref Palette2) 1))
  (sta (addr #x2122))
  (ldx (label-ref UntitledData))	; Address
  (lda (label-ref UntitledData 'bank))	; of UntitledData 
  (ldy (* 15 16 2))	; length of data
  (stx (addr #x4302))	; write
  (sta (addr #x4304))	; address
  (sty (addr #x4305))	; and length
  (lda #b00000001)	; set this mode (transferring words)
  (sta (addr #x4300))
  (lda #x18)	; $211[89]: VRAM data write
  (sta (addr #x4301))	; set destination
  
  (ldy #x0000)	; Write to VRAM from $0000
  (sty (addr #x2116))
  
  (lda #b00000001)	; start DMA, channel 0
  (sta (addr #x420B))
  (lda #b10000000)	; VRAM writing mode
  (sta (addr #x2115))
  (ldx #x4000)	; write to vram
  (stx (addr #x2116))	; from $4000
  
  ;ugly code starts here - it writes the # shape I mentioned before.
  (repeat 2
          ;X|X|X
          (repeat 2
                  (ldx #x0000)	; tile 0 ( )
                  (stx (addr #x2118))
                  (ldx #x0002)	; tile 2 (|)
                  (stx (addr #x2118)))
          (ldx #x0000)
          (stx (addr #x2118))
          ;first line finished, add BG's
          (repeat 27
                  (stx (addr #x2118)))	; X=0
          ;beginning of 2nd line
          ;-+-+-
          (repeat 2
                  (ldx #x0004)	; tile 4 (-)
                  (stx (addr #x2118))
                  (ldx #x0006)	; tile 6 (+)
                  (stx (addr #x2118)))
          (ldx #x0004)	; tile 4 (-)
          (stx (addr #x2118))
          (ldx #x0000)
          (repeat 27
                  (stx (addr #x2118))))
  (repeat 2
          (ldx #x0000)	; tile 0 ( )
          (stx (addr #x2118))
          (ldx #x0002)	; tile 2 (|)
          (stx (addr #x2118)))
  (ldx #x6000)	; BG2 will start here
  (stx (addr #x2116))
  (ldx #x000C)	; And will contain 1 tile
  (stx (addr #x2118))
  ;set up the screen
  (lda #b00110000)	; 16x16 tiles, mode 0
  (sta (addr #x2105))	; screen mode register
  (lda #b01000000)	; data starts from $4000
  (sta (addr #x2107))	; for BG1
  (lda #b01100000)	; and $6000
  (sta (addr #x2108))	; for BG2
  
  (stz (addr #x210B))	; BG1 and BG2 use the $0000 tiles
  
  (lda #b00000011)	; enable bg1&2
  (sta (addr #x212C))
  
  ;The PPU doesn't process the top line, so we scroll down 1 line.
  (rep #x20)	; 16bit a
  (lda #x07FF)	; this is -1 for BG1
  (sep #x20)	; 8bit a
  (sta (addr #x210E))	; BG1 vert scroll
  (xba)
  (sta (addr #x210E))
  
  (rep #x20)	; 16bit a
  (lda #xFFFF)	; this is -1 for BG2
  (sep #x20)	; 8bit a
  (sta (addr #x2110))	; BG2 vert scroll
  (xba)
  (sta (addr #x2110))
  
  (lda #b00001111)	; enable screen, set brightness to 15
  (sta (addr #x2100))
  
  (lda #b10000001)	; enable NMI and joypads
  (sta (addr #x4200))
  
  (label forever)
  (wai)
  (rep #b00100000)	; get 16 bit A
  (lda #x0000)		; empty it
  (sep #b00100000)	; 8 bit A
  (lda (addr #x0100))		; get our X coord
  (ConvertX)		; WLA needs a space before a macro name
  (sta (addr #x210F))		; BG2 horz scroll
  (xba)
  (sta (addr #x210F))		; write 16 bits
  
  ;now repeat it, but change $0100 to $0101, and $210F to $2110
  (rep #b00100000)	; get 16 bit A
  (lda #x0000)		; empty it
  (sep #b00100000)	; 8 bit A
  (lda (addr #x0101))		; get our Y coord
  (ConvertY)		; WLA needs a space before a macro name
  (sta (addr #x2110))		; BG2 vert scroll
  (xba)
  (sta (addr #x2110))		; write 16 bits
  ;--------------------------------------
  (ldx #x0000)		; reset our counter
  (label -)
  (rep #b00100000)		; 16 bit A
  (lda #x0000)		; empty it
  (sep #b00100000)		; 8 bit a
  (lda/X (label-ref VRAMtable 'long))	; this is a long indexed address, nice :)
  (rep #b00100000)
  (clc)
  (adc #x4000)		; add $4000 to the value
  (sta (addr #x2116))		; write to VRAM from here
  (lda #x0000)		; reset A while it's still 16 bit
  (sep #b00100000)		; 8 bit A
  (lda/X (addr #x0000))		; get the corresponding tile from RAM
  ; VRAM data write mode is still %10000000
  (sta (addr #x2118))		; write
  (stz (addr #x2119))		; this is the hi-byte
  (inx)
  (cpx 9)			; finished?
  (bne (label-ref -))			; no, go back
  (jmp (label-ref forever)))

(define-section Conversiontable
  #:bank 2 
  (label VRAMtable)
  (data (bytes #x00 #x02 #x04 #x40 #x42 #x44 #x80 #x82 #x84)))

(define ROM
  (make-rom
   ;==LoRom==      ; We'll get to HiRom some other time.
   #:rom-bank-size #x8000              ; Every ROM bank is 32 KBytes in size
   #:rom-banks 8                    ; 2 Mbits - Tell WLA we want to use 8 ROM Banks
   
   #:id #"SNES"                     ; 1-4 letter string, just leave it as "SNES"
   
   #:name #"A small game         "  ; Program Title - can't be over 21 bytes,
   ;    "123456789012345678901"  ; use spaces for unused bytes of the name.
   
   #:slow-rom? #t
   #:lo-rom? #t
   
   #:cartridge-type #x00             ; #x00 = ROM only, see WLA documentation for others
   #:rom-size #x08                   ; #x08 = 2 Mbits,  see WLA doc for more..
   #:sram-size #x00                  ; No SRAM         see WLA doc for more..
   #:country #x01                   ; #x01 = U.S.  #x00 = Japan, that's all I know
   #:license-code #x00              ; Just use #x00
   #:version #x00                   ; #x00 = 1.00, #x01 = 1.01, etc.
   
   #:native-interrupts
   (hasheq 'COP (label-ref EmptyHandler)
           'BRK (label-ref EmptyHandler)
           'ABORT (label-ref EmptyHandler)
           'NMI (label-ref VBlank)
           'IRQ (label-ref EmptyHandler))
   
   #:emulation-interrupts
   (hasheq 'COP (label-ref EmptyHandler)
           'ABORT (label-ref EmptyHandler)
           'NMI (label-ref VBlank)
           'RESET (label-ref Start) ; where execution starts
           'IRQBRK (label-ref EmptyHandler))
   
   ; fill unused areas with #x00, opcode for BRK.  
   ; BRK will crash the snes if executed.
   #:empty-fill #x00
   
   #:sections 
   (list InitializeSNESCode Main Vblank EmptyVectors Tiledata Conversiontable)))
