; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "finale.o",cbm
!source "../core/labels.asm"
!source "../core/mymacros.asm"
!source "../core/vic_ii.asm"

*= end_of_core

!zone {
finale_s_init
    jsr finale_s_load_best

    +utils_m_enable_bitmap_mode
    +utils_m_enable_multicolor_mode
    lda #0
    sta SPENA
    sta finale_v_must_exit

    lda #<finale_c_THE_BITMAP
    sta P0
    lda #>finale_c_THE_BITMAP
    sta P1
    jsr bmap_s_draw_multicolor
    jsr finale_s_draw_best_rounds
    jsr interrupts_s_install
    
.wait_loop
    lda finale_v_must_exit
    +branch_if_false .wait_loop

    jsr interrupts_s_uninstall
    rts
; end sub finale_s_init
} ; !zone


; *****************
; *** CONSTANTS ***
; *****************
finale_c_THE_BITMAP
!source "../../assets/pictures/best_rounds_template2.asm"

finale_l_BEST_NAME_OFFSETS  !byte   0,10,20
finale_c_BEST_SCORE_BASE = finale_v_best_rounds_data+(3*shared_c_MAX_NAME_LEN)
finale_l_BEST_NAME_ROWS !byte   10*8,11*8,12*8
finale_c_BEST_NAME_COL = 2*4   
finale_c_BEST_SCORE_COL = 14*4
finale_l_BEST_ROWS_CHARS    !byte   10,11,12
finale_c_BEST_COL_CHARS = 2
finale_c_BEST_ENTRY_LEN = 14
finale_l_BEST_COLORS    !byte   WHITE,YELLOW,LIGHT_GREEN


; *****************
; *** VARIABLES ***
; *****************
; bestXX.prg will be loaded to here from disk.
finale_v_best_rounds_data   !fill   33,0
finale_v_must_exit          !byte   0
finale_v_best_filename      !pet    "best0x.prg",0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
finale_s_update
    rts
; end sub finale_s_update
} ; !zone

; **************************************************

!zone {
finale_s_load_best
    inc EXTCOL

    ; NOTE: while there are fewer than 10 courses, only one char needs to
    ; be replaced in the filename!
    lda shared_v_course_index
    ; Set bits 4 and 5 to get PETSCII digit.
    ora #$30
    sta finale_v_best_filename+5

    ldx #<finale_v_best_filename
    ldy #>finale_v_best_filename
    jsr CB_LOADFILE

    dec EXTCOL
    rts
; end sub finale_s_load_best
} ; !zone

; **************************************************

!zone {
.ITER = TREES_LO
.score_str_buffer   !fill   2

finale_s_draw_best_rounds
    ldx #2
.loop_top
    stx .ITER

    ; Colors.
    lda finale_l_BEST_ROWS_CHARS,x
    sta P0
    lda #finale_c_BEST_COL_CHARS
    sta P1
    lda finale_l_BEST_COLORS,x
    sta P2
    lda #LIGHT_BLUE
    sta P3
    lda #finale_c_BEST_ENTRY_LEN
    sta P4
    jsr font_s_prepare_colors
    
    ; Address of text into P0-P1.
    ldx .ITER
    lda #<finale_v_best_rounds_data
    clc
    adc finale_l_BEST_NAME_OFFSETS,x
    sta P0
    lda #>finale_v_best_rounds_data
    adc #0
    sta P1
    ; Row in P2, column in P3 (bitmap coordinates).
    lda finale_l_BEST_NAME_ROWS,x
    sta P2
    lda #finale_c_BEST_NAME_COL
    sta P3
    lda #shared_c_MAX_NAME_LEN
    sta P4
    jsr font_s_draw_text

    ; And now the score.
    ldx .ITER
    lda finale_l_BEST_NAME_ROWS,x
    sta P2
    lda #finale_c_BEST_SCORE_COL
    sta P3
    lda #2
    sta P4
    lda finale_c_BEST_SCORE_BASE,x
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; Safe to assume a two-digit value!  Digits are stored in 'reverse' order
    ; in utils_v_dec_digits (i.e. low byte first).
    lda utils_v_dec_digits
    ora #$30
    sta .score_str_buffer+1
    lda utils_v_dec_digits+1
    ora #$30
    sta .score_str_buffer
    lda #<.score_str_buffer
    sta P0
    lda #>.score_str_buffer
    sta P1
    jsr font_s_draw_text

    ldx .ITER
    dex
    bpl .loop_top

    rts
; end sub finale_s_draw_best_rounds
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

!source "interrupts.asm"
!source "../common/decompressor.asm"
!source "../common/bitmap.asm"


