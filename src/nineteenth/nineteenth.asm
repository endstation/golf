; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "nineteenth.o",cbm
!source "../core/labels.asm"
!source "../core/mymacros.asm"
!source "../core/vic_ii.asm"

*= end_of_core

!zone {
nteenth_s_init
    +utils_m_enable_bitmap_mode
    +utils_m_enable_multicolor_mode
    lda #0
    sta SPENA
    sta nteenth_v_must_exit
    lda #<nteenth_c_WAIT_TIME_FRAMES
    sta nteenth_v_timer_lo
    lda #>nteenth_c_WAIT_TIME_FRAMES
    sta nteenth_v_timer_hi

    lda #<nteenth_c_THE_BITMAP
    sta P0
    lda #>nteenth_c_THE_BITMAP
    sta P1
    jsr bmap_s_draw_multicolor

    ldx #joy_c_PORT2 
    +joy_m_lock_fire

    jsr interrupts_s_install

.wait_loop
    lda nteenth_v_must_exit
    +branch_if_false .wait_loop

    jsr interrupts_s_uninstall

    rts
; end sub nteenth_s_init
} ; !zone


; *****************
; *** CONSTANTS ***
; *****************
nteenth_c_THE_BITMAP
!source "../../assets/pictures/nineteenth.asm"

; PAL = 50Hz. So 50 frames per second.
; Wait half a minute before exiting (or until user presses fire button).
; 50*30 = 1500
nteenth_c_WAIT_TIME_FRAMES = 1500


; *****************
; *** VARIABLES ***
; *****************
nteenth_v_must_exit !byte   0
nteenth_v_timer_lo  !byte   0
nteenth_v_timer_hi  !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
nteenth_s_update
    lda nteenth_v_timer_lo
    sec
    sbc #1
    sta nteenth_v_timer_lo
    bcs +
    dec nteenth_v_timer_hi
    bmi .must_exit
+

    ; Check joystick.
    ldx #joy_c_PORT2 
    +joy_m_is_fire 
    bne .unlock
    +joy_m_is_locked_fire
    beq .must_exit
    rts ; EXIT POINT.
.unlock
    +joy_m_release_fire
    rts ; EXIT POINT.

.must_exit
    inc nteenth_v_must_exit

.end
    rts
; end sub nteenth_s_update
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
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

!source "interrupts.asm"
!source "../common/decompressor.asm"
!source "../common/bitmap.asm"


