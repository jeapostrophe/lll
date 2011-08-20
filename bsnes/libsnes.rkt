#lang racket/base
(require racket/runtime-path
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/cvector)
(provide (all-defined-out))

; Library
(define libsnes (ffi-lib "libsnes"))
(define-ffi-definer define-libsnes libsnes)

; Types
(require "libsnes-defines.rkt")

; XXX these should actually be connected to snes_term
(define _snes_video_refresh_t
  (_fun #:keep #t
        #:atomic? #t
        ; XXX data is _uint16 ptr
        [data : _pointer]
        [width : _unsigned]
        [height : _unsigned]
        ->
        _void))
(define _snes_audio_sample_t
  (_fun #:keep #t
        #:atomic? #t
        [left : _uint16]
        [right : _uint16]
        ->
        _void))
(define _snes_input_poll_t
  (_fun #:keep #t
        #:atomic? #t
        ->
        _void))
(define _snes_input_state_t
  (_fun #:keep #t
        #:atomic? #t
        [port : _snes_port]
        [device : _snes_device]
        [index : _unsigned]
        [id : _snes_id]
        ->
        _int16))

; Definitions
(define-libsnes snes_library_id 
  (_fun -> _string))
(define-libsnes snes_library_revision_major
  (_fun -> _unsigned))
(define-libsnes snes_library_revision_minor
  (_fun -> _unsigned))

(define-libsnes snes_set_video_refresh
  (_fun _snes_video_refresh_t -> _void))
(define-libsnes snes_set_audio_sample
  (_fun _snes_audio_sample_t -> _void))
(define-libsnes snes_set_input_poll
  (_fun _snes_input_poll_t -> _void))
(define-libsnes snes_set_input_state
  (_fun _snes_input_state_t -> _void))

(define-libsnes snes_set_controller_port_device
  (_fun [port : _snes_port]
        [device : _snes_device]
        ->
        _void))
(define-libsnes snes_set_cartridge_basename
  (_fun [basename : _string]
        ->
        _void))

(define-syntax-rule (define-libsnes-void id)
  (define-libsnes id (_fun -> _void)))
(define-libsnes-void snes_init)
(define-libsnes-void snes_term) ; XXX this should free our resources
(define-libsnes-void snes_power)
(define-libsnes-void snes_reset)
(define-libsnes-void snes_run)

(define-libsnes snes_serialize_size
  (_fun -> _unsigned))
(define-libsnes snes_serialize
  (_fun [data : (_bytes o size)]
        [size : _unsigned]
        ->
        _bool))
(define-libsnes snes_unserialize
  (_fun [data : _bytes]
        [size : _unsigned = (bytes-length data)]
        ->
        _bool))

(define-libsnes-void snes_cheat_reset)
(define-libsnes snes_cheat_set
  (_fun [index : _unsigned]
        [enabled : _bool]
        [code : _string]
        ->
        _void))

(define-libsnes snes_load_cartridge_normal
  (_fun [rom_xml : _string]
        [rom_data : _bytes]
        [rom_size : _unsigned = (bytes-length rom_data)]
        ->
        _bool))
; XXX there are more loading functions

(define-libsnes-void snes_unload_cartridge)

(define-libsnes snes_get_region
  ; XXX use a better type than bool
  (_fun -> _bool))
(define-libsnes snes_get_memory_data
  (_fun [id : _snes_memory]
        ->
        ; output is a pointer to _uint8
        _pointer))
(define-libsnes snes_get_memory_size
  (_fun [id : _snes_memory]
        ->
        _unsigned))

