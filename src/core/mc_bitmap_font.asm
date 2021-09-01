; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
font_c_ASCII_SPACE = 32
font_c_ASCII_PLUS = 43
font_c_ASCII_MINUS = 45
font_c_ASCII_0 = 48
font_c_ASCII_A = 65
; NOTE: this isn't ASCII standard!!!
font_c_ONE_HALF = 34

font_l_DATA
    !bin "../../assets/chars/mc_bitmap_font_ingame.bin"
font_l_CHAR_DATA_LO = * - font_c_ASCII_SPACE
!for i,91 {
    !byte <(font_l_DATA+((i-1)*8))
} ; !for
font_l_CHAR_DATA_HI = * - font_c_ASCII_SPACE
!for i,91 {
    !byte >(font_l_DATA+((i-1)*8))
} ; !for

; lsr, lsr, ora #$c0
font_l_SHIFT_RIGHT_BYTES    !byte   $4a,$4a,$09,$c0 
font_c_NOP_OPCODE = $ea


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   P0-P1 = address of text
;           P2 = row, P3 = column (in bitmap coordinates)
;           P4 = length
; NOTE: we assume colors have been dealt with separately (for time being)...
; Column should be a multiple of 4; row should have a value <= 192.
!zone {
.CHAR_ITER = MATHS2
.TXT_LEN = P4

font_s_draw_text
    ; In the case that .TXT_LEN is 0, there's nothing to do.
    lda .TXT_LEN
    bne +
    rts ; EXIT POINT.

+
    ; Work out the destination (bitmap) address.
    ; Place in BITMAP_LO/HI.
    ldx P2
    lda dp_l_BITMAP_ROWS_LO,x
    sta BITMAP_LO
    lda dp_l_BITMAP_ROWS_HI,x
    sta BITMAP_HI
    ldx P3
    lda BITMAP_LO
    clc
    adc dp_l_BITMAP_COLS_LO,x
    sta BITMAP_LO
    lda BITMAP_HI
    adc dp_l_BITMAP_COLS_HI,x
    sta BITMAP_HI

; NOTE: alternatively, you can call the routine from here if the destination
; address is already loaded into BITMAP_LO/HI...
font_s_draw_text_direct   

    ldy #0
.loop_top
    sty .CHAR_ITER
    lda (P0),y

    ; Get the data source into PATTERN_LO/HI.
    ; ASCII code is in the accumulator.
    tax
    lda font_l_CHAR_DATA_LO,x
    sta PATTERN_LO
    lda font_l_CHAR_DATA_HI,x
    sta PATTERN_HI

    ; Now copy 8 bytes from 'char data' into the bitmap.
    ldy #7
-
    lda (PATTERN_LO),y

    ; NOTE: reserve four bytes here so can have option to align chars
    ; differently (e.g. shift right two (hi-res) pixels).
font_v_draw_modify
    nop
    nop
    nop
    nop

    sta (BITMAP_LO),y
    dey
    bpl -

    ; Add 8 to the bitmap pointer so we're ready for the next char (if there
    ; is one!).  Also here increment the 'char' iterator.
    lda BITMAP_LO
    clc
    adc #8
    sta BITMAP_LO
    lda BITMAP_HI
    adc #0
    sta BITMAP_HI
    ldy .CHAR_ITER
    iny
    cpy .TXT_LEN
    bne .loop_top

.end
    rts
; end sub font_s_draw_text
} ; !zone

; **************************************************

; INPUTS:   P0 = row (chars), P1 = column (chars),
;           P2 = f/g color, P3 = b/g color, P4 = length
!zone {
.FG_COLOR = P2
.BG_COLOR = P3
.LEN = P4

font_s_prepare_colors
    ldx P0
    lda dp_l_VIDEO_RAM_ROWS_LO,x
    clc
    adc P1
    sta MATHS0
    lda dp_l_VIDEO_RAM_ROWS_HI,x
    adc #0
    sta MATHS1
    lda dp_l_COLOR_RAM_ROWS_LO,x
    clc
    adc P1
    sta MATHS2
    lda dp_l_COLOR_RAM_ROWS_HI,x
    adc #0
    sta MATHS3

    ldy #0
-
    lda .FG_COLOR
    sta (MATHS0),y
    lda .BG_COLOR
    sta (MATHS2),y
    iny
    cpy .LEN
    bne -

    rts
; end sub font_s_prepare_colors
} ; !zone

; **************************************************

!zone {
font_s_shift_text_right
    ldx #3
-
    lda font_l_SHIFT_RIGHT_BYTES,x
    sta font_v_draw_modify,x
    dex
    bpl -
    rts
; end sub font_s_shift_text_right
} ; !zone

; **************************************************

!zone {
font_s_reset_shift
    ldx #3
    lda #font_c_NOP_OPCODE
-
    sta font_v_draw_modify,x
    dex
    bpl -
    rts
; end sub font_s_reset_shift
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

