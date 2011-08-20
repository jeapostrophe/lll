#lang racket/base
(require racket/match
         racket/list
         ffi/unsafe)
(provide (all-defined-out))

(define _unsigned _uint32)

(define (prep-define l)
  (append-map
   (match-lambda
     [`(define ,id ,val)
      `(,id = ,val)])
   l))

(define _snes_port
  (_enum '(SNES_PORT_1 = 0
           SNES_PORT_2 = 1)
         _unsigned))

(define _snes_device
  (_enum '(SNES_DEVICE_NONE =          0
           SNES_DEVICE_JOYPAD =        1
           SNES_DEVICE_MULTITAP =      2
           SNES_DEVICE_MOUSE =         3
           SNES_DEVICE_SUPER_SCOPE =   4
           SNES_DEVICE_JUSTIFIER =     5
           SNES_DEVICE_JUSTIFIERS =    6
           SNES_DEVICE_SERIAL_CABLE =  7)
         _unsigned))

(define _snes_id
  (_enum (prep-define
          '((define SNES_DEVICE_ID_JOYPAD_B        0)
            (define SNES_DEVICE_ID_JOYPAD_Y        1)
            (define SNES_DEVICE_ID_JOYPAD_SELECT   2)
            (define SNES_DEVICE_ID_JOYPAD_START    3)
            (define SNES_DEVICE_ID_JOYPAD_UP       4)
            (define SNES_DEVICE_ID_JOYPAD_DOWN     5)
            (define SNES_DEVICE_ID_JOYPAD_LEFT     6)
            (define SNES_DEVICE_ID_JOYPAD_RIGHT    7)
            (define SNES_DEVICE_ID_JOYPAD_A        8)
            (define SNES_DEVICE_ID_JOYPAD_X        9)
            (define SNES_DEVICE_ID_JOYPAD_L       10)
            (define SNES_DEVICE_ID_JOYPAD_R       11)
            
            (define SNES_DEVICE_ID_MOUSE_X      0)
            (define SNES_DEVICE_ID_MOUSE_Y      1)
            (define SNES_DEVICE_ID_MOUSE_LEFT   2)
            (define SNES_DEVICE_ID_MOUSE_RIGHT  3)
            
            (define SNES_DEVICE_ID_SUPER_SCOPE_X        0)
            (define SNES_DEVICE_ID_SUPER_SCOPE_Y        1)
            (define SNES_DEVICE_ID_SUPER_SCOPE_TRIGGER  2)
            (define SNES_DEVICE_ID_SUPER_SCOPE_CURSOR   3)
            (define SNES_DEVICE_ID_SUPER_SCOPE_TURBO    4)
            (define SNES_DEVICE_ID_SUPER_SCOPE_PAUSE    5)
            
            (define SNES_DEVICE_ID_JUSTIFIER_X        0)
            (define SNES_DEVICE_ID_JUSTIFIER_Y        1)
            (define SNES_DEVICE_ID_JUSTIFIER_TRIGGER  2)
            (define SNES_DEVICE_ID_JUSTIFIER_START    3)))
         _unsigned))

(define _snes_region
  (_enum '(SNES_REGION_NTSC = 0
           SNES_REGION_PAL = 1)
         _bool))

(define _snes_memory
  (_enum (prep-define
          '((define SNES_MEMORY_CARTRIDGE_RAM       0)
            (define SNES_MEMORY_CARTRIDGE_RTC       1)
            (define SNES_MEMORY_BSX_RAM             2)
            (define SNES_MEMORY_BSX_PRAM            3)
            (define SNES_MEMORY_SUFAMI_TURBO_A_RAM  4)
            (define SNES_MEMORY_SUFAMI_TURBO_B_RAM  5)
            (define SNES_MEMORY_GAME_BOY_RAM        6)
            (define SNES_MEMORY_GAME_BOY_RTC        7)
            
            (define SNES_MEMORY_WRAM    100)
            (define SNES_MEMORY_APURAM  101)
            (define SNES_MEMORY_VRAM    102)
            (define SNES_MEMORY_OAM     103)
            (define SNES_MEMORY_CGRAM   104)))
         _unsigned))
