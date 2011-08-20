#lang racket/base
(require "libsnes.rkt"
         ffi/unsafe/cvector
         (only-in ffi/unsafe _uint16)
         racket/file
         racket/runtime-path
         racket/gui)

(define-runtime-path some-rom "Earthbound.smc")

(define frame (new frame%
                   [label "Super Nintendo"]
                   [width 512]
                   [height 239]))
(define snes-bitmap
  (make-object bitmap% 512 239))

(define canvas
  (new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc draw-bitmap
                      snes-bitmap
                      0 0))]))
 
(snes_library_id)
(snes_library_revision_major)
(snes_library_revision_minor)
(snes_init)

(snes_set_cartridge_basename "Earthbound")
(snes_load_cartridge_normal #f (file->bytes some-rom))

(snes_set_controller_port_device 'SNES_PORT_1 'SNES_DEVICE_JOYPAD)
(snes_set_controller_port_device 'SNES_PORT_2 'SNES_DEVICE_NONE)

(snes_set_input_poll
 (λ ()
   (void)))
(snes_set_input_state
 (λ (port device index id)
   (printf "~v\n" (list 'input-state port device index id))
   0))
(snes_set_audio_sample
 (λ (left right)
   (unless (and (zero? left) (zero? right))
     (printf "~v\n" (list 'audio-sample left right)))))

(define snes-pixels (make-bytes (* 512 239 4)))
(snes_set_video_refresh
 (λ (data width height)
   (define cv (make-cvector* data _uint16 (* width height)))
   
   (for* ([x (in-range 0 width)]
          [y (in-range 0 height)])
     (define offset (+ (* x height) y))
     (define raw 
       (cvector-ref cv offset))
     (define r (bitwise-bit-field raw 0 4))
     (define g (bitwise-bit-field raw 5 9))
     (define b (bitwise-bit-field raw 10 14))
     (bytes-set! snes-pixels (+ offset 1) r)
     (bytes-set! snes-pixels (+ offset 2) g)
     (bytes-set! snes-pixels (+ offset 3) b))
   
   (send snes-bitmap
         set-argb-pixels
         0 0
         width height
         snes-pixels)
   
   (send canvas refresh-now)))

(for ([memory (in-list '(SNES_MEMORY_CARTRIDGE_RAM  SNES_MEMORY_CARTRIDGE_RTC SNES_MEMORY_BSX_RAM SNES_MEMORY_BSX_PRAM SNES_MEMORY_SUFAMI_TURBO_A_RAM SNES_MEMORY_SUFAMI_TURBO_B_RAM SNES_MEMORY_GAME_BOY_RAM SNES_MEMORY_GAME_BOY_RTC SNES_MEMORY_WRAM SNES_MEMORY_APURAM SNES_MEMORY_VRAM SNES_MEMORY_OAM SNES_MEMORY_CGRAM))])
  (printf "~v => ~v\n"
          memory
          (snes_get_memory_size memory)))

(send frame show #t)

(printf "Powering on...\n")
(snes_power)

(snes_get_region)

(printf "Running...\n")
(let loop ()
  (printf ".") (flush-output)
  (snes_run)
  (loop))

(snes_unload_cartridge)
(snes_term)
