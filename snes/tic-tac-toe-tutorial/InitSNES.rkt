#lang s-exp "../snes.rkt"
;------------------------------------------------------------------------
;-  Written by: Neviksti
;-     If you use my code, please share your creations with me
;-     as I am always curious :)
;------------------------------------------------------------------------
(provide (all-defined-out))

;----------------------------------------------------------------------------
; InitSNES -- my "standard" initialization of SNES memory and registers
;----------------------------------------------------------------------------
(define (InitSNES)
  (sei)                     ;disable interrupts
  (clc)                     ;switch to native mode
  (xce)
  
  (rep #x38)		; mem/A = 16 bit, X/Y = 16 bit
  ;decimal mode off
  
  (ldx #x1FFF)	;Setup the stack
  (txs)			;Transfer Index X to Stack Pointer Register
  
  ;do the rest of the initialization in a routine
  (jsl (addr #x008000))
  
  (sep #x20))		; mem/A = 8 bit

;----------------------------------------------------------------------------

(define-section InitializeSNESCode
  #:force
  #:bank 0
  (label InitializeSNES)
  (phk)			;set Data Bank = Program Bank
  (plb)
  
  (lda #x0000)	;set Direct Page = $0000
  (tcd)			;Transfer Accumulator to Direct Register
  
  (ldx (addr #x1FFD))		;we clear all the mem at one point ...
  (stx (addr #x4372))  	;so save the return address in a place that won't get overwritten
  (ldx (addr #x1FFF))
  (stx (addr #x4374))
  
  (sep #x20)		; mem/A = 8 bit
  (rep #x10)
  
  (lda #x8F)
  (sta (addr #x2100))		;turn screen off for now, set brightness to normal
  
  (ldx #x2101)
  (label _Loop00)		;regs $2101-$210C
  (stz/DP/X #x00)		;set Sprite,Character,Tile sizes to lowest, and set addresses to $0000
  (inx)
  (cpx.l #x210D)
  (bne (label-ref _Loop00))
  
  (label _Loop01)		;regs $210D-$2114
  (stz/DP/X #x00)		;Set all BG scroll values to $0000
  (stz/DP/X #x00)
  (inx)
  (cpx.l #x2115)
  (bne (label-ref _Loop01))
  
  (lda #x80)		;reg $2115
  (sta (addr #x2115))		; Initialize VRAM transfer mode to word-access, increment by 1
  
  (stz (addr #x2116))		;regs $2117-$2117
  (stz (addr #x2117))		;VRAM address = $0000
  
  ;reg $2118-$2119
  ;VRAM write register... don't need to initialize
  
  (stz (addr #x211A))		;clear Mode7 setting
  
  (ldx #x211B)
  (label _Loop02)		;regs $211B-$2120
  (stz/DP/X #x00)		;clear out the Mode7 matrix values
  (stz/DP/X #x00)
  (inx)
  (cpx.l #x2121)
  (bne (label-ref _Loop02))
  
  ;reg $2121 - Color address, doesn't need initilaizing
  ;reg $2122 - Color data, is initialized later
  
  (ldx #x2123)
  (label _Loop03)		;regs $2123-$2133
  (stz/DP/X #x00)		;turn off windows, main screens, sub screens, color addition,
  (inx)			;fixed color = $00, no super-impose (external synchronization),
  (cpx.l #x2134)	;no interlaced mode, normal resolution
  (bne (label-ref _Loop03))
  
  ;regs $2134-$2136  - multiplication result, no initialization needed
  ;reg $2137 - software H/V latch, no initialization needed
  ;reg $2138 - Sprite data read, no initialization needed
  ;regs $2139-$213A  - VRAM data read, no initialization needed
  ;reg $213B - Color RAM data read, no initialization needed
  ;regs $213C-$213D  - H/V latched data read, no initialization needed
  
  (stz (addr #x213E))		;reg $213E - might not be necesary, but selects PPU master/slave mode
  ;reg $213F - PPU status flag, no initialization needed
  
  ;reg $2140-$2143 - APU communication regs, no initialization required
  
  ;reg $2180  -  read/write WRAM register, no initialization required
  ;reg $2181-$2183  -  WRAM address, no initialization required
  
  ;reg $4016-$4017  - serial JoyPad read registers, no need to initialize
  
  
  (stz (addr #x4200))		;reg $4200  - disable timers, NMI,and auto-joyread
  
  (lda #xFF)
  (sta (addr #x4201))		;reg $4201  - programmable I/O write port, initalize to allow reading at in-port
  
  ;regs $4202-$4203  - multiplication registers, no initialization required
  ;regs $4204-$4206  - division registers, no initialization required
  
  ;regs $4207-$4208  - Horizontal-IRQ timer setting, since we disabled this, it is OK to not init
  ;regs $4209-$420A  - Vertical-IRQ timer setting, since we disabled this, it is OK to not init
  
  (stz (addr #x420B))		;reg $420B  - turn off all general DMA channels
  (stz (addr #x420C))		;reg $420C  - turn off all H-MA channels
  
  (stz (addr #x420D))		;reg $420D  - ROM access time to slow (2.68Mhz)
  
  (lda (addr #x4210))		;reg $4210  - NMI status, reading resets
  
  ;reg $4211  - IRQ status, no need to initialize
  ;reg $4212  - H/V blank and JoyRead status, no need to initialize
  ;reg $4213  - programmable I/O inport, no need to initialize
  
  ;reg $4214-$4215  - divide results, no need to initialize
  ;reg $4216-$4217  - multiplication or remainder results, no need to initialize
  
  ;regs $4218-$421f  - JoyPad read registers, no need to initialize
  
  ;regs $4300-$437F
  ;no need to intialize because DMA was disabled above
  ;also, we're not sure what all of the registers do, so it is better to leave them at
  ;their reset state value
  
  (jsr (label-ref ClearVRAM))      ;Reset VRAM
  (jsr (label-ref ClearPalette))   ;Reset colors
  
  ;**** clear Sprite tables ********
  
  (stz (addr #x2102))	;sprites initialized to be off the screen, palette 0, character 0
  (stz (addr #x2103))
  (ldx #x80)
  (lda #xF0)
  (label _Loop08)
  (sta (addr #x2104))	;set X = 240
  (sta (addr #x2104))	;set Y = 240
  (stz (addr #x2104))	;set character = $00
  (stz (addr #x2104))	;set priority=0, no flips
  (dex)
  (bne (label-ref _Loop08))
  
  (ldx #x0020)
  (label _Loop09)
  (stz (addr #x2104))		;set size bit=0, x MSB = 0
  (dex)
  (bne (label-ref _Loop09))
  
  ;**** clear WRAM ********
  
  (stz (addr #x2181))		;set WRAM address to $000000
  (stz (addr #x2182))
  (stz (addr #x2183))
  
  (ldx #x8008)
  (stx (addr #x4300))         ;Set DMA mode to fixed source, BYTE to $2180
  (ldx (label-ref wram_fill_byte))
  (stx (addr #x4302))         ;Set source offset
  (lda (label-ref wram_fill_byte 'bank))
  (sta (addr #x4304))         ;Set source bank
  (ldx #x0000)
  (stx (addr #x4305))         ;Set transfer size to 64k bytes
  (lda #x01)
  (sta (addr #x420B))         ;Initiate transfer
  
  (lda #x01)          ;now set the next 64k bytes
  (sta (addr #x420B))         ;Initiate transfer
  
  (phk)			;make sure Data Bank = Program Bank
  (plb)
  
  (cli)			;enable interrupts again
  
  (ldx (addr #x4372))  	;get our return address...
  (stx (addr #x1FFD))
  (lda (addr #x4374))
  (sta (addr #x1FFF))
  (rtl)
  
  (label wram_fill_byte)
  (data (bytes #x00))
  
  ;----------------------------------------------------------------------------
  ; ClearVRAM -- Sets every byte of VRAM to zero
  ; In: None
  ; Out: None
  ; Modifies: flags
  ;----------------------------------------------------------------------------
  (label ClearVRAM)
  (pha)
  (phx)
  (php)
  
  (rep #x30)		; mem/A = 8 bit, X/Y = 16 bit
  (sep #x20)
  
  (lda #x80)
  (sta (addr #x2115))         ;Set VRAM port to word access
  (ldx #x1809)
  (stx (addr #x4300))         ;Set DMA mode to fixed source, WORD to $2118/9
  (ldx #x0000)
  (stx (addr #x2116))         ;Set VRAM port address to $0000
  (stx/DP #x00)         ;Set $00:0000 to $0000 (assumes scratchpad ram)
  (stx (addr #x4302))         ;Set source address to $xx:0000
  (lda #x00)
  (sta (addr #x4304))         ;Set source bank to $00
  (ldx #xFFFF)
  (stx (addr #x4305))         ;Set transfer size to 64k-1 bytes
  (lda #x01)
  (sta (addr #x420B))         ;Initiate transfer
  
  (stz (addr #x2119))         ;clear the last byte of the VRAM
  
  (plp)
  (plx)
  (pla)
  (rts)
  
  ;----------------------------------------------------------------------------
  ; ClearPalette -- Reset all palette colors to zero
  ; In: None
  ; Out: None
  ; Modifies: flags
  ;----------------------------------------------------------------------------
  (label ClearPalette)
  (phx)
  (php)
  (rep #x30)		; mem/A = 8 bit, X/Y = 16 bit
  (sep #x20)
  
  (stz (addr #x2121))
  (ldx #x0100)
  (label ClearPaletteLoop)
  (stz (addr #x2122))
  (stz (addr #x2122))
  (dex)
  (bne (label-ref ClearPaletteLoop))
  
  (plp)
  (plx)
  (rts))
