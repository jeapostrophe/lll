#lang s-exp "../snes.rkt"
(provide (all-defined-out))
; Created with eKid's pcx2snes converter ;

(define-section Tiledata
  #:bank 1 ; We'll use bank 1
  #:slot 0 #:org 0
  
  (label UntitledData)
  (data
   (bytes
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03
    #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF #xFF #xFF #xFF
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFF #xFF #xFF #xFF
    #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #xFF #xFF #xFF #xFF
    #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xFF #xFF #xFF #xFF
    #x00 #xC0 #x00 #xE0 #x00 #x70 #x00 #x38 #x00 #x1C #x00 #x0E #x00 #x07 #x00 #x03
    #x00 #x03 #x00 #x07 #x00 #x0E #x00 #x1C #x00 #x38 #x00 #x70 #x00 #xE0 #x00 #xC0
    #x00 #x07 #x00 #x0F #x00 #x18 #x00 #x30 #x00 #x60 #x00 #xC0 #x00 #xC0 #x00 #xC0
    #x00 #xE0 #x00 #xF0 #x00 #x18 #x00 #x0C #x00 #x06 #x00 #x03 #x00 #x03 #x00 #x03
    #xFC #x00 #xF8 #x00 #xF0 #x00 #xE0 #x00 #xC0 #x00 #x80 #x00 #x00 #x00 #x00 #x00
    #x3F #x00 #x1F #x00 #x0F #x00 #x07 #x00 #x03 #x00 #x01 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03
    #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0
    #xFF #xFF #xFF #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #xFF #xFF #xFF #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #xFF #xFF #xFF #xFF #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03 #x03
    #xFF #xFF #xFF #xFF #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0 #xC0
    #x00 #x03 #x00 #x07 #x00 #x0E #x00 #x1C #x00 #x38 #x00 #x70 #x00 #xE0 #x00 #xC0
    #x00 #xC0 #x00 #xE0 #x00 #x70 #x00 #x38 #x00 #x1C #x00 #x0E #x00 #x07 #x00 #x03
    #x00 #xC0 #x00 #xC0 #x00 #xC0 #x00 #x60 #x00 #x30 #x00 #x18 #x00 #x0F #x00 #x07
    #x00 #x03 #x00 #x03 #x00 #x03 #x00 #x06 #x00 #x0C #x00 #x18 #x00 #xF0 #x00 #xE0
    #x00 #x00 #x00 #x00 #x80 #x00 #xC0 #x00 #xE0 #x00 #xF0 #x00 #xF8 #x00 #xFC #x00
    #x00 #x00 #x00 #x00 #x01 #x00 #x03 #x00 #x07 #x00 #x0F #x00 #x1F #x00 #x3F #x00))
  
  (label UntitledPalette)
  (data
   (bytes
    #x00 #x00 #xE0 #x7F #x1F #x00 #xFF #x03))
  
  (label Palette2)
  (data
   (bytes
    #xE0 #x7F))
  
  ; 30 tiles (2 spaces)
  ; 480 bytes
  )