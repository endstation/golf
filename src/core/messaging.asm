; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


msg_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; NOTE: in 'bitmap' coordinates.
msg_c_ROW = 24*8
msg_c_OUT_OF_BOUNDS_STR !raw "Out of bounds!"
msg_c_SCORE_CARDS_STR   !raw "Loading score cards"
msg_c_FISHING_STR       !raw "Gone fishing!"
msg_c_CONCEDE_STR       !raw "Concede hole? No Yes"
msg_c_WAIT_STR          !raw "Please wait..."

msg_l_MSG_ADDR_LO   !byte   <msg_c_OUT_OF_BOUNDS_STR
                    !byte   <msg_c_SCORE_CARDS_STR
                    !byte   <msg_c_FISHING_STR
                    !byte   <msg_c_CONCEDE_STR
                    !byte   <msg_c_WAIT_STR
msg_l_MSG_ADDR_HI   !byte   >msg_c_OUT_OF_BOUNDS_STR
                    !byte   >msg_c_SCORE_CARDS_STR
                    !byte   >msg_c_FISHING_STR
                    !byte   >msg_c_CONCEDE_STR
                    !byte   >msg_c_WAIT_STR
msg_l_MSG_LENGTHS   !byte   14,19,13,20,14

msg_c_OUT_OF_BOUNDS         = 0
msg_c_LOADING_SCORE_CARDS   = 1
msg_c_FISHING               = 2
msg_c_CONCEDE_HOLE          = 3
msg_c_WAIT                  = 4


; *****************
; *** VARIABLES ***
; *****************
msg_v_last_len  !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   P0-P1 = address of text, P4 = length
;           P3 = column (char column * 4) - REQUIRED ONLY IF CALLING
;             'msg_s_display_at_column'.
!zone {
msg_s_display
    jsr msg_s_clear

    lda #0
    sta P3
msg_s_display_at_column
    lda #msg_c_ROW 
    sta P2
    lda P4
    sta msg_v_last_len
    jsr font_s_draw_text
    rts
; end sub msg_s_display
} ; !zone

; **************************************************

; INPUTS:   X = index.
!zone {
msg_s_display_stock_msg
    jsr msg_s_clear

    lda msg_l_MSG_ADDR_LO,x   
    sta P0
    lda msg_l_MSG_ADDR_HI,x   
    sta P1
    lda #msg_c_ROW
    sta P2
    lda #0
    sta P3
    lda msg_l_MSG_LENGTHS,x
    sta P4
    sta msg_v_last_len
    jsr font_s_draw_text
    rts
; end sub msg_s_display_stock_msg
} ; !zone

; **************************************************

!zone {
msg_s_clear
    lda msg_v_last_len
    beq .end

    lda #<gfxs_c_BITMAP_BASE+(24*320) 
    sta MATHS0
    lda #>gfxs_c_BITMAP_BASE+(24*320) 
    sta MATHS1
    lda #<gfxs_c_BITMAP_BASE+(24*320)+160 
    sta MATHS2
    lda #>gfxs_c_BITMAP_BASE+(24*320)+160 
    sta MATHS3

    ldy #0
    lda #$ff
-
    sta (MATHS0),y
    sta (MATHS2),y
    iny
    cpy #160
    bne -

    lda #0
    sta msg_v_last_len

.end
    rts
; end sub msg_s_clear
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

msg_c_SIZE = *-msg_c_BEGIN

