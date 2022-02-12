; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


wash_c_BEGIN = *

; *****************
; *** CONSTANTS ***
; *****************
wash_l_SEQUENCE !byte   BLUE,BLUE,CYAN,CYAN,LIGHT_RED,YELLOW
wash_c_SEQUENCE_LEN = 6
wash_c_FRAME_RATE = 2
wash_c_DEST_BASE = gfxs_c_DISPLAY_BASE+24*40


; *****************
; *** VARIABLES ***
; *****************
wash_v_frame_count  !byte   0
wash_v_active       !byte   0
; NOTE: duration measured in frames.
wash_v_duration     !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   X = duration.
!zone {
wash_s_init
    stx wash_v_duration

    ldx #wash_c_SEQUENCE_LEN-1
-
    lda wash_l_SEQUENCE,x
    sta wash_c_DEST_BASE,x
    dex
    bpl -

    lda #wash_c_FRAME_RATE
    sta wash_v_frame_count
    sta wash_v_active

    rts
; end sub wash_s_init
} ; !zone

; **************************************************

!zone {
wash_s_update
    lda wash_v_active
    +branch_if_false .end

    dec wash_v_duration
    bne +
    jsr wash_s_stop
    rts ; EXIT POINT.

+
    dec wash_v_frame_count
    bne .end
    lda #wash_c_FRAME_RATE
    sta wash_v_frame_count

    ; Push last color onto stack - it will eventually be written to offset=0.
    ldx playmsg_v_last_index
    lda wash_c_DEST_BASE,x
    pha
    ; Continue to shift colors right until X=0.
-
    lda wash_c_DEST_BASE-1,x
    sta wash_c_DEST_BASE,x
    dex
    bne -
    ; Now deal with the 'wraparound'.
    pla
    sta wash_c_DEST_BASE

.end
    rts
; end sub wash_s_update
} ; !zone

; **************************************************

!zone {
wash_s_stop
    lda wash_v_active
    +branch_if_false .end
    lda #0
    sta wash_v_active

    ; Clean up colors in message area.
    ; FIXME: Maybe a more slick way of doing this visually?!
    ldx playmsg_v_last_index
    lda #GREY3
-
    sta wash_c_DEST_BASE,x
    dex
    bpl -

.end
    rts
; end sub wash_s_stop
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

wash_c_SIZE = *-wash_c_BEGIN

