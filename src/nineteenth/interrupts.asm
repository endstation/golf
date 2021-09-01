; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
interrupts_c_SPLIT = $fa


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
interrupts_s_cb
    lda VICIRQ
    bpl interrupts_clean_up

    jsr nteenth_s_update

;    jmp interrupts_reset
    ; Allow 'fall-through' to reset routine.
; end sub interrupts_s_cb
} ; !zone

; **************************************************

; NOTE: this is 'jmp'd to by the callback (cb) routines.
!zone {
interrupts_reset
    lda #interrupts_c_SPLIT
    sta RASTER
    +utils_m_clear_raster_bit9 
    ; Release latch.
    asl VICIRQ

; NOTE: will often jump straight to here.
interrupts_clean_up
    ; Pull registers off of stack and restore.
    pla
    tay
    pla
    tax
    pla
    rti

; end sub interrupts_reset
} ; !zone

; **************************************************

!zone {
interrupts_s_uninstall
    sei
    lda #0
    sta IRQMSK
    cli
    rts
; end sub interrupts_s_uninstall
} ; !zone

; **************************************************

!zone {
interrupts_s_install
    sei

    lda #<interrupts_s_cb
    sta CINV
    lda #>interrupts_s_cb
    sta CINV+1

    ; Enable raster interrupts.
    lda #$01
    sta IRQMSK
    ; Turn off CIA interrupts.
    lda #$7f
    sta $dc0d
    
    lda #interrupts_c_SPLIT
    sta RASTER
    +utils_m_clear_raster_bit9 

    cli
    rts

; end sub interrupts_s_install
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

