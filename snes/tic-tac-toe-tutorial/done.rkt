#lang racket/base
(require "../snes.rkt"
         "InitSNES.rkt"
         "tiles.rkt")
(provide ROM)

(define-section EmptyVectors
  #:bank 0 #:semi-free
  (label EmptyHandler)
  (RTI))

(define (ConvertX)
  ; Data in: our coord in A
  ; Data out: SNES scroll data in C (the 16 bit A)
  (for ([i (in-range 5)])
       (ASL/A))		; multiply A by 32
  (REP #b00100000)	; 16 bit A
  (EOR #xFFFF)	; this will do A=1-A
  (INA)		; A=A+1
  (SEP #b00100000))	; 8 bit A

(define (ConvertY)
  ; Data in: our coord in A
  ; Data out: SNES scroll data in C (the 16 bit A)
  (for ([i (in-range 5)])
       (ASL/A))		; multiply A by 32
  (REP #b00100000)	; 16 bit A
  (EOR #xFFFF)	; this will do A=1-A
  (SEP #b00100000))	; 8 bit A

(define-section Vblank
  #:bank 0
  
  ;--------------------------------------
  (label VBlank)
  (LDA (addr #x4212))	; get joypad status
  (AND #b00000001)	; if joy is not ready
  (BNE (label-ref VBlank))	; wait
  (LDA (addr #x4219))	; read joypad (BYSTudlr)
  (STA (addr #x0201))	; store it
  (CMP (addr #x0200))	; compare it with the previous
  (UNLESS BNE		; if not equal, go
          (RTI))		; if it's equal, then return
  
  (STA (addr #x0200))	; store
  (AND #b00010000)	; get the start button
  ; this will be the delete key
  (UNLESS BEQ		; if it's 0, we don't have to delete
          (LDX #x0000)
          (DO-WHILE
           (STZ/DP/X #x00)	; delete addresses $0000 to $0008
           (INX)
           (CPX.l #x09)	; this is 9. Guess why (homework :) )
           BNE)
          (STZ (addr #x0100))	; delete the scroll
          (STZ (addr #x0101)))	; data also
  
  (LDA (addr #x0201))	; get back the temp value
  (AND #b11000000)	; Care only about B AND Y
  (BEQ (label-ref +))		; if empty, skip this
  ; so, B or Y is pressed. Let's say B is O,
  ; AND Y is X.
  (CMP #b11000000)	; both are pressed?
  (BEQ (label-ref +))		; then don't do anything
  (CMP #b10000000)	; B?
  (BNE (label-ref ++))		; now, try Y
  ; B is pressed, write an O ($08)
  ; we have to tell the cursor position,
  ; AND calculate an address from that
  ; Formula: Address=3*Y+X
  (LDA (addr #x0101))	; get Y
  (STA (addr #x0202))	; put it to a temp value
  (CLC)
  (ADC (addr #x0202))	; multiply by 3 - an easy way
  (ADC (addr #x0202))	; A*3=A+A+A :)
  (ADC (addr #x0100))	; add X
  ; Now A contains our address
  (LDX #x0000)	; be on the safe side
  (TAX)
  (LDA #x08)
  (STA/DP/X #x00)	; put $08 to the good address
  (JMP (label-ref +))		; done with this
  
  (label ++)		; now for Y
  (CMP #b01000000)	; Y?
  (BNE (label-ref +))		; no, jump forward (this should not happen)
  ; Y is pressed, write an X ($0A)
  (LDA (addr #x0101))	; get Y
  (STA (addr #x0202))	; put it to a temp value
  (CLC)
  (ADC (addr #x0202))	; multiply by 3 - an easy way
  (ADC (addr #x0202))	; A*3=A+A+A :)
  (ADC (addr #x0100))	; add X
  ; Now A contains our address
  (LDX #x00)	; be on the safe side
  (TAX)
  (LDA #x0A)
  (STA/DP/X #x00)	; put $0A to the good address
  (label +)		; finished putting tiles
  
  ; cursor moving comes now
  (LDA (addr #x0201))	; get control
  (AND #b00001111)	; care about directions
  (STA (addr #x0201))	; store this
  
  (CMP #b00001000)	; up?
  (BNE (label-ref +))		; if not, skip
  (LDA (addr #x0101))	; get scroll Y
  (CMP #x00)	; if on the top,
  (BEQ (label-ref +))		; don't do anything
  (DEC (addr #x0101))	; sub 1 from Y
  (label +)
  
  (LDA (addr #x0201))	; get control
  (CMP #b00000100)	; down?
  (BNE (label-ref +))		; if not, skip
  (LDA (addr #x0101))
  (CMP #x02)	; if on the bottom,
  (BEQ (label-ref +))		; don't do anything
  (INC (addr #x0101))	; add 1 to Y
  (label +)
  
  (LDA (addr #x0201))	; get control
  (CMP #b00000010)	; left?
  (BNE (label-ref +))		; if not, skip
  (LDA (addr #x0100))
  (CMP #x00)	; if on the left,
  (BEQ (label-ref +))		; don't do anything
  (DEC (addr #x0100))	; sub 1 from X
  (label +)
  
  (LDA (addr #x0201))	; get control
  (CMP #b00000001)	; right?
  (BNE (label-ref +))		; if not, skip
  (LDA (addr #x0100))
  (CMP #x02)	; if on the right,
  (BEQ (label-ref +))		; don't do anything
  (INC (addr #x0100))	; add 1 to X
  (label +)
  (RTI))		; F|NisH3D!
;--------------------------------------

(define-section Main
  #:bank 0 
  ;--------------------------------------
  
  (label Start)
  (InitSNES)
  (REP #b00010000)	;16 bit xy
  (SEP #b00100000)	;8 bit ab
  
  (LDX #x0000)
  (label -)
  (LDA.l/X (label-ref UntitledPalette 'long))
  (STA (addr #x2122))
  (INX)
  (CPX.l 8)
  (BNE (label-ref -))
  
  ;I'll explain this later
  ;We'll have two palettes, only one color is needed for the second:
  (LDA 33)		;The color we need is the 33rd
  (STA (addr #x2121))
  (LDA (label-ref Palette2 'long))
  (STA (addr #x2122))
  (LDA (label-ref Palette2PlusOne 'long) #;(+ (label-ref Palette2) 1))
  (STA (addr #x2122))
  (LDX (label-ref UntitledData '&))	; Address
  (LDA (label-ref UntitledData 'bank))	; of UntitledData 
  (LDY (* 15 16 2))	; length of data
  (STX (addr #x4302))	; write
  (STA (addr #x4304))	; address
  (STY (addr #x4305))	; AND length
  (LDA #b00000001)	; set this mode (transferring words)
  (STA (addr #x4300))
  (LDA #x18)	; $211[89]: VRAM data write
  (STA (addr #x4301))	; set destination
  
  (LDY #x0000)	; Write to VRAM from $0000
  (STY (addr #x2116))
  
  (LDA #b00000001)	; start DMA, channel 0
  (STA (addr #x420B))
  (LDA #b10000000)	; VRAM writing mode
  (STA (addr #x2115))
  (LDX #x4000)	; write to vram
  (STX (addr #x2116))	; from $4000
  
  ;ugly code starts here - it writes the # shape I mentioned before.
  (for ([i (in-range 2)])
          ;X|X|X
          (for ([i (in-range 2)])
                  (LDX #x0000)	; tile 0 ( )
                  (STX (addr #x2118))
                  (LDX #x0002)	; tile 2 (|)
                  (STX (addr #x2118)))
          (LDX #x0000)
          (STX (addr #x2118))
          ;first line finished, add BG's
          (for ([i (in-range 27)])
                  (STX (addr #x2118)))	; X=0
          ;beginning of 2nd line
          ;-+-+-
          (for ([i (in-range 2)])
                  (LDX #x0004)	; tile 4 (-)
                  (STX (addr #x2118))
                  (LDX #x0006)	; tile 6 (+)
                  (STX (addr #x2118)))
          (LDX #x0004)	; tile 4 (-)
          (STX (addr #x2118))
          (LDX #x0000)
          (for ([i (in-range 27)])
                  (STX (addr #x2118))))
  (for ([i (in-range 2)])
          (LDX #x0000)	; tile 0 ( )
          (STX (addr #x2118))
          (LDX #x0002)	; tile 2 (|)
          (STX (addr #x2118)))
  (LDX #x6000)	; BG2 will start here
  (STX (addr #x2116))
  (LDX #x000C)	; AND will contain 1 tile
  (STX (addr #x2118))
  ;set up the screen
  (LDA #b00110000)	; 16x16 tiles, mode 0
  (STA (addr #x2105))	; screen mode register
  (LDA #b01000000)	; data starts from $4000
  (STA (addr #x2107))	; for BG1
  (LDA #b01100000)	; AND $6000
  (STA (addr #x2108))	; for BG2
  
  (STZ (addr #x210B))	; BG1 AND BG2 use the $0000 tiles
  
  (LDA #b00000011)	; enable bg1&2
  (STA (addr #x212C))
  
  ;The PPU doesn't process the top line, so we scroll down 1 line.
  (REP #x20)	; 16bit a
  (LDA #x07FF)	; this is -1 for BG1
  (SEP #x20)	; 8bit a
  (STA (addr #x210E))	; BG1 vert scroll
  (XBA)
  (STA (addr #x210E))
  
  (REP #x20)	; 16bit a
  (LDA #xFFFF)	; this is -1 for BG2
  (SEP #x20)	; 8bit a
  (STA (addr #x2110))	; BG2 vert scroll
  (XBA)
  (STA (addr #x2110))
  
  (LDA #b00001111)	; enable screen, set brightness to 15
  (STA (addr #x2100))
  
  (LDA #b10000001)	; enable NMI AND joypads
  (STA (addr #x4200))
  
  (label forever)
  (WAI)
  (REP #b00100000)	; get 16 bit A
  (LDA.w #x0000)		; empty it
  (SEP #b00100000)	; 8 bit A
  (LDA (addr #x0100))		; get our X coord
  (ConvertX)		; WLA needs a space before a macro name
  (STA (addr #x210F))		; BG2 horz scroll
  (XBA)
  (STA (addr #x210F))		; write 16 bits
  
  ;now repeat it, but change $0100 to $0101, AND $210F to $2110
  (REP #b00100000)	; get 16 bit A
  (LDA.w #x0000)		; empty it
  (SEP #b00100000)	; 8 bit A
  (LDA (addr #x0101))		; get our Y coord
  (ConvertY)		; WLA needs a space before a macro name
  (STA (addr #x2110))		; BG2 vert scroll
  (XBA)
  (STA (addr #x2110))		; write 16 bits
  ;--------------------------------------
  (LDX #x0000)		; reset our counter
  (label -)
  (REP #b00100000)		; 16 bit A
  (LDA.w #x0000)		; empty it
  (SEP #b00100000)		; 8 bit a
  (LDA.l/X (label-ref VRAMtable 'long))	; this is a long indexed address, nice :)
  (REP #b00100000)
  (CLC)
  (ADC #x4000)		; add $4000 to the value
  (STA (addr #x2116))		; write to VRAM from here
  (LDA.w #x0000)		; reset A while it's still 16 bit
  (SEP #b00100000)		; 8 bit A
  (LDA/DP/X #x00)		; get the corresponding tile from RAM
  ; VRAM data write mode is still %10000000
  (STA (addr #x2118))		; write
  (STZ (addr #x2119))		; this is the hi-byte
  (INX)
  (CPX.l 9)			; finished?
  (BNE (label-ref -))			; no, go back
  (JMP (label-ref forever)))

(define-section Conversiontable
  #:bank 2 
  (label VRAMtable)
  (data (bytes #x00 #x02 #x04 #x40 #x42 #x44 #x80 #x82 #x84)))

(define ROM
  (make-rom
   ;==LoRom==      ; We'll get to HiRom some other time.

   #:slot-start #x8000
   
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
      
   InitializeSNESCode Main Vblank EmptyVectors Tiledata Conversiontable))
