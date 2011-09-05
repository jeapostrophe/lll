#lang racket/base
(require "../snes.rkt")
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
  (SEI)                     ;disable interrupts
  (CLC)                     ;switch to native mode
  (XCE)
  
  (REP #x38)		; mem/A = 16 bit, X/Y = 16 bit
  ;decimal mode off
  
  (LDX #x1FFF)	;Setup the stack
  (TXS)			;Transfer InDEX X to Stack Pointer Register
  
  ;do the rest of the initialization in a routine
  (JSL (addr #x008000))
  
  (SEP #x20))		; mem/A = 8 bit

;----------------------------------------------------------------------------

(define InitializeSNESCode
  (code
   (label InitializeSNES)
   (PHK)			;set Data Bank = Program Bank
   (PLB)
   
   (LDA #x0000)	;set Direct Page = $0000
   (TCD)			;Transfer Accumulator to Direct Register
   
   (LDX (addr #x1FFD))		;we clear all the mem at one point ...
   (STX (addr #x4372))  	;so save the return address in a place that won't get overwritten
   (LDX (addr #x1FFF))
   (STX (addr #x4374))
   
   (SEP #x20)		; mem/A = 8 bit
   (REP #x10)
   
   (LDA #x8F)
   (STA (addr #x2100))		;turn screen off for now, set brightness to normal
   
   (LDX #x2101)
   (DO-WHILE ;regs $2101-$210C
    (STZ/DP/X #x00)		;set Sprite,Character,Tile sizes to lowest, and set addresses to $0000
    (INX)
    (CPX.l #x210D)
    BNE)

   (DO-WHILE ;regs $210D-$2114
    (STZ/DP/X #x00)		;Set all BG scroll values to $0000
    (STZ/DP/X #x00)
    (INX)
    (CPX.l #x2115)
    BNE)
   
   (LDA #x80)		;reg $2115
   (STA (addr #x2115))		; Initialize VRAM transfer mode to word-access, increment by 1
   
   (STZ (addr #x2116))		;regs $2117-$2117
   (STZ (addr #x2117))		;VRAM address = $0000
   
                                        ;reg $2118-$2119
                                        ;VRAM write register... don't need to initialize
   
   (STZ (addr #x211A))		;clear Mode7 setting
   
   (LDX #x211B)
   (DO-WHILE		;regs $211B-$2120
    (STZ/DP/X #x00)		;clear out the Mode7 matrix values
    (STZ/DP/X #x00)
    (INX)
    (CPX.l #x2121)
    BNE)
   
                                        ;reg $2121 - Color address, doesn't need initilaizing
                                        ;reg $2122 - Color data, is initialized later
   
   (LDX #x2123)
   (DO-WHILE		;regs $2123-$2133
    (STZ/DP/X #x00)		;turn off windows, main screens, sub screens, color addition,
    (INX)			;fixed color = $00, no super-impose (external synchronization),
    (CPX.l #x2134)	;no interlaced mode, normal resolution
    BNE)
   
                                        ;regs $2134-$2136  - multiplication result, no initialization needed
                                        ;reg $2137 - software H/V latch, no initialization needed
                                        ;reg $2138 - Sprite data read, no initialization needed
                                        ;regs $2139-$213A  - VRAM data read, no initialization needed
                                        ;reg $213B - Color RAM data read, no initialization needed
                                        ;regs $213C-$213D  - H/V latched data read, no initialization needed
   
   (STZ (addr #x213E))		;reg $213E - might not be necesary, but selects PPU master/slave mode
                                        ;reg $213F - PPU status flag, no initialization needed
   
                                        ;reg $2140-$2143 - APU communication regs, no initialization required
   
                                        ;reg $2180  -  read/write WRAM register, no initialization required
                                        ;reg $2181-$2183  -  WRAM address, no initialization required
   
                                        ;reg $4016-$4017  - serial JoyPad read registers, no need to initialize
   
   
   (STZ (addr #x4200))		;reg $4200  - disable timers, NMI,and auto-joyread
   
   (LDA #xFF)
   (STA (addr #x4201))		;reg $4201  - programmable I/O write port, initalize to allow reading at in-port
   
                                        ;regs $4202-$4203  - multiplication registers, no initialization required
                                        ;regs $4204-$4206  - division registers, no initialization required
   
                                        ;regs $4207-$4208  - Horizontal-IRQ timer setting, since we disabled this, it is OK to not init
                                        ;regs $4209-$420A  - Vertical-IRQ timer setting, since we disabled this, it is OK to not init
   
   (STZ (addr #x420B))		;reg $420B  - turn off all general DMA channels
   (STZ (addr #x420C))		;reg $420C  - turn off all H-MA channels
   
   (STZ (addr #x420D))		;reg $420D  - ROM access time to slow (2.68Mhz)
   
   (LDA (addr #x4210))		;reg $4210  - NMI status, reading resets
   
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
   
   (JSR (label-ref ClearVRAM))      ;Reset VRAM
   (JSR (label-ref ClearPalette))   ;Reset colors
   
                                        ;**** clear Sprite tables ********
   
   (STZ (addr #x2102))	;sprites initialized to be off the screen, palette 0, character 0
   (STZ (addr #x2103))
   (LDX #x80)
   (LDA #xF0)
   (DO-WHILE
    (STA (addr #x2104))	;set X = 240
    (STA (addr #x2104))	;set Y = 240
    (STZ (addr #x2104))	;set character = $00
    (STZ (addr #x2104))	;set priority=0, no flips
    (DEX)
    BNE)
   
   (LDX #x0020)
   (DO-WHILE
    (STZ (addr #x2104))		;set size bit=0, x MSB = 0
    (DEX)
    BNE)
   
                                        ;**** clear WRAM ********
   
   (STZ (addr #x2181))		;set WRAM address to $000000
   (STZ (addr #x2182))
   (STZ (addr #x2183))
   
   (LDX #x8008)
   (STX (addr #x4300))         ;Set DMA mode to fixed source, BYTE to $2180
   (LDX (label-ref wram_fill_byte '&))
   (STX (addr #x4302))         ;Set source offset
   (LDA (label-ref wram_fill_byte 'bank))
   (STA (addr #x4304))         ;Set source bank
   (LDX #x0000)
   (STX (addr #x4305))         ;Set transfer size to 64k bytes
   (LDA #x01)
   (STA (addr #x420B))         ;Initiate transfer
   
   (LDA #x01)          ;now set the next 64k bytes
   (STA (addr #x420B))         ;Initiate transfer
   
   (PHK)			;make sure Data Bank = Program Bank
   (PLB)
   
   (CLI)			;enable interrupts again
   
   (LDX (addr #x4372))  	;get our return address...
   (STX (addr #x1FFD))
   (LDA (addr #x4374))
   (STA (addr #x1FFF))
   (RTL)
   
   (label wram_fill_byte)
   (data (bytes #x00))
   
                                        ;----------------------------------------------------------------------------
                                        ; ClearVRAM -- Sets every byte of VRAM to zero
                                        ; In: None
                                        ; Out: None
                                        ; Modifies: flags
                                        ;----------------------------------------------------------------------------
   (label ClearVRAM)
   (PHA)
   (PHX)
   (PHP)
   
   (REP #x30)		; mem/A = 8 bit, X/Y = 16 bit
   (SEP #x20)
   
   (LDA #x80)
   (STA (addr #x2115))         ;Set VRAM port to word access
   (LDX #x1809)
   (STX (addr #x4300))         ;Set DMA mode to fixed source, WORD to $2118/9
   (LDX #x0000)
   (STX (addr #x2116))         ;Set VRAM port address to $0000
   (STX/DP #x00)         ;Set $00:0000 to $0000 (assumes scratchpad ram)
   (STX (addr #x4302))         ;Set source address to $xx:0000
   (LDA #x00)
   (STA (addr #x4304))         ;Set source bank to $00
   (LDX #xFFFF)
   (STX (addr #x4305))         ;Set transfer size to 64k-1 bytes
   (LDA #x01)
   (STA (addr #x420B))         ;Initiate transfer
   
   (STZ (addr #x2119))         ;clear the last byte of the VRAM
   
   (PLP)
   (PLX)
   (PLA)
   (RTS)
   
                                        ;----------------------------------------------------------------------------
                                        ; ClearPalette -- Reset all palette colors to zero
                                        ; In: None
                                        ; Out: None
                                        ; Modifies: flags
                                        ;----------------------------------------------------------------------------
   (label ClearPalette)
   (PHX)
   (PHP)
   (REP #x30)		; mem/A = 8 bit, X/Y = 16 bit
   (SEP #x20)
   
   (STZ (addr #x2121))
   (LDX #x0100)
   (DO-WHILE
    (STZ (addr #x2122))
    (STZ (addr #x2122))
    (DEX)
    BNE)
   
   (PLP)
   (PLX)
   (RTS)))
