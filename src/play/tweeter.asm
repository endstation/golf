; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


tweeter_c_BEGIN = *

; *****************
; *** CONSTANTS ***
; *****************
tweeter_c_MIN_WAIT_TICKS = 150


; *****************
; *** VARIABLES ***
; *****************
; Channel on which sfx is being played, else (-1).
tweeter_v_channel       !byte   0
; This must get to zero before we can initialize a countdown!
tweeter_v_current_wait  !byte   0
tweeter_v_is_last_wait  !byte   0
tweeter_v_is_on         !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; NOTE: still want this to run even if tweeter is turned off, to allow any
; existing sfx to finish gracefully.
!zone {
tweeter_s_update
    ldx tweeter_v_channel
    bmi .check_wait

    ; Sound is playing - has it finished? 
    ; NOTE: this isn't 100% reliable!
    lda snd_v_channel_active,x
    +branch_if_true .end

    ; Sound must have finished.
    jsr tweeter_s_reset
    rts ; EXIT POINT.

.check_wait
    dec tweeter_v_current_wait
    bne .end
    ; Wait has counted down (to zero).  If this is the 'last' wait, it's now
    ; time to launch a new bird call sfx.  Else pick a random wait time.
    lda tweeter_v_is_last_wait
    +branch_if_true .new_sfx
    jsr rand_s_get_fast
    sta tweeter_v_current_wait
    inc tweeter_v_is_last_wait
    rts ; EXIT POINT.

.new_sfx
    jsr tweeter_s_spawn

.end
    rts
; end sub tweeter_s_update
} ; !zone

; **************************************************

!zone {
tweeter_s_spawn
    lda tweeter_v_is_on
    ; Better to pretend we must wait longer?!
    +branch_if_false .no_channel

    jsr rand_s_get_fast
    ldy #sfx_c_BIRD1     
    and #$01
    beq +
    iny
+

    ; This routine returns channel in X or $ff if no free channel was
    ; available.
    jsr snd_s_init_sfx
    stx tweeter_v_channel
    txa
    bpl .end

.no_channel
    ; No channel available, so wait a bit longer.
    lda #tweeter_c_MIN_WAIT_TICKS 
    sta tweeter_v_current_wait
    sta tweeter_v_is_last_wait

.end
    rts
; end sub tweeter_s_spawn
} ; !zone

; **************************************************

!zone {
tweeter_s_reset
    ldx #$ff
    stx tweeter_v_channel
    inx
    stx tweeter_v_is_last_wait
    lda #tweeter_c_MIN_WAIT_TICKS 
    sta tweeter_v_current_wait
    rts
; end sub tweeter_s_reset
} ; !zone

; **************************************************

!zone {
tweeter_s_turn_on
    jsr tweeter_s_reset
    ; When that routine exits, accumulator has non-zero value in it.
    sta tweeter_v_is_on
    rts
; end sub tweeter_s_turn_on
} ; !zone

; **************************************************

!zone {
tweeter_s_turn_off
    +clr tweeter_v_is_on
    rts
; end sub tweeter_s_turn_off
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

tweeter_c_SIZE = *-tweeter_c_BEGIN

