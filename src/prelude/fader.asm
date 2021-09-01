; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
fader_l_FADE_TO_GREY
    !byte   BLACK,WHITE,GREY1,WHITE,GREY2,GREY2,GREY1,WHITE,GREY2,BLACK,GREY3,GREY1,GREY2,WHITE,GREY3,GREY3

fader_l_GREYSCALE_TABLE_SR !fill   1000,0
fader_l_GREYSCALE_TABLE_CR !fill   1000,0

; Destination addresses.
fader_l_ROWS_COLRAM_LO !byte $00,$c8,$90,$58,$20
fader_l_ROWS_COLRAM_HI !byte $d8,$d8,$d9,$da,$db
fader_l_ROWS_SCRRAM_LO
    !byte <gfxs_c_DISPLAY_BASE
    !byte <(gfxs_c_DISPLAY_BASE+200)
    !byte <(gfxs_c_DISPLAY_BASE+400)
    !byte <(gfxs_c_DISPLAY_BASE+600)
    !byte <(gfxs_c_DISPLAY_BASE+800)
fader_l_ROWS_SCRRAM_HI
    !byte >gfxs_c_DISPLAY_BASE
    !byte >(gfxs_c_DISPLAY_BASE+200)
    !byte >(gfxs_c_DISPLAY_BASE+400)
    !byte >(gfxs_c_DISPLAY_BASE+600)
    !byte >(gfxs_c_DISPLAY_BASE+800)

; Source addresses.
; NOTE: For each table, the 'colorized' addresses come first, then the
; greyscale.  SO in every case, greyscale indices must be offset by an
; additional 5 bytes.
fader_c_COLRAM_SRC_BASE = $8328
fader_c_SCRRAM_SRC_BASE = $7f40
fader_l_ROWS_COLRAM_SRC_LO
    !byte   <fader_c_COLRAM_SRC_BASE 
    !byte   <(fader_c_COLRAM_SRC_BASE+200)
    !byte   <(fader_c_COLRAM_SRC_BASE+(2*200))
    !byte   <(fader_c_COLRAM_SRC_BASE+(3*200))
    !byte   <(fader_c_COLRAM_SRC_BASE+(4*200))
    !byte   <fader_l_GREYSCALE_TABLE_CR
    !byte   <(fader_l_GREYSCALE_TABLE_CR+200)
    !byte   <(fader_l_GREYSCALE_TABLE_CR+(2*200));
    !byte   <(fader_l_GREYSCALE_TABLE_CR+(3*200));
    !byte   <(fader_l_GREYSCALE_TABLE_CR+(4*200));
fader_l_ROWS_COLRAM_SRC_HI
    !byte   >fader_c_COLRAM_SRC_BASE 
    !byte   >(fader_c_COLRAM_SRC_BASE+200)
    !byte   >(fader_c_COLRAM_SRC_BASE+(2*200))
    !byte   >(fader_c_COLRAM_SRC_BASE+(3*200))
    !byte   >(fader_c_COLRAM_SRC_BASE+(4*200))
    !byte   >fader_l_GREYSCALE_TABLE_CR
    !byte   >(fader_l_GREYSCALE_TABLE_CR+200)
    !byte   >(fader_l_GREYSCALE_TABLE_CR+(2*200));
    !byte   >(fader_l_GREYSCALE_TABLE_CR+(3*200));
    !byte   >(fader_l_GREYSCALE_TABLE_CR+(4*200));
fader_l_ROWS_SCRRAM_SRC_LO
    !byte   <fader_c_SCRRAM_SRC_BASE 
    !byte   <(fader_c_SCRRAM_SRC_BASE+200)
    !byte   <(fader_c_SCRRAM_SRC_BASE+(2*200))
    !byte   <(fader_c_SCRRAM_SRC_BASE+(3*200))
    !byte   <(fader_c_SCRRAM_SRC_BASE+(4*200))
    !byte   <fader_l_GREYSCALE_TABLE_SR
    !byte   <(fader_l_GREYSCALE_TABLE_SR+200)
    !byte   <(fader_l_GREYSCALE_TABLE_SR+(2*200));
    !byte   <(fader_l_GREYSCALE_TABLE_SR+(3*200));
    !byte   <(fader_l_GREYSCALE_TABLE_SR+(4*200));
fader_l_ROWS_SCRRAM_SRC_HI
    !byte   >fader_c_SCRRAM_SRC_BASE 
    !byte   >(fader_c_SCRRAM_SRC_BASE+200)
    !byte   >(fader_c_SCRRAM_SRC_BASE+(2*200))
    !byte   >(fader_c_SCRRAM_SRC_BASE+(3*200))
    !byte   >(fader_c_SCRRAM_SRC_BASE+(4*200))
    !byte   >fader_l_GREYSCALE_TABLE_SR
    !byte   >(fader_l_GREYSCALE_TABLE_SR+200)
    !byte   >(fader_l_GREYSCALE_TABLE_SR+(2*200));
    !byte   >(fader_l_GREYSCALE_TABLE_SR+(3*200));
    !byte   >(fader_l_GREYSCALE_TABLE_SR+(4*200));

fader_c_NUM_ROWS = 5

fader_c_DOWN    = $01
fader_c_UP      = $ff
fader_c_TYPE_GREYSCALE  = 0
fader_c_TYPE_BLACK      = 1
fader_c_TYPE_RECOLORIZE = 2

; Used for recolorizing.
fader_c_FRAME_RATE = 1


; *****************
; *** VARIABLES ***
; *****************
fader_v_rows_iter   !byte   0
fader_v_finished    !byte   0
fader_v_direction   !byte   0
fader_v_type        !byte   0
fader_v_rows_end    !byte   0
fader_v_frame_count !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
fader_s_update_recolorize
    dec fader_v_frame_count
    beq +
    rts ; EXIT POINT.

+
    lda #fader_c_FRAME_RATE 
    sta fader_v_frame_count

    ldy #0
    lda fader_v_type
    cmp #fader_c_TYPE_BLACK
    beq .loop_black

.loop_grey_colorize
    lda (FADE_CR_SRC_LO),y
    sta (FADE_CR_LO),y
    lda (FADE_SR_SRC_LO),y
    sta (FADE_SR_LO),y
    iny
    cpy #(fader_c_NUM_ROWS*40)
    bne .loop_grey_colorize
    beq .next_row

.loop_black
    lda #0
-
    sta (FADE_CR_LO),y
    sta (FADE_SR_LO),y
    iny
    cpy #(fader_c_NUM_ROWS*40)
    bne -

.next_row
    lda fader_v_rows_iter
    clc
    adc fader_v_direction
    sta fader_v_rows_iter
    tax
    cpx fader_v_rows_end
    bne +

    ; Finished!
    inc fader_v_finished
    rts ; EXIT POINT.

+
    jsr fader_s_set_addr_pointers

    rts
; end sub fader_s_update_recolorize
} ; !zone

; **************************************************

; NOTE: set the two input variables manually before calling this routine.
; INPUTS:   fader_v_direction,fader_v_type
!zone {
fader_s_init
    lda #0
    sta fader_v_finished
    ; We want the effect to begin immediately, so set count to 1.
    lda #1
    sta fader_v_frame_count

    ; Direction:
    lda fader_v_direction
    cmp #fader_c_DOWN
    beq .down
    ; So UP!
    ldx #(fader_c_NUM_ROWS-1)
    ldy #$ff
    bne +
.down
    ldx #0
    ldy #fader_c_NUM_ROWS
+
    stx fader_v_rows_iter
    sty fader_v_rows_end

    jsr fader_s_set_addr_pointers
    rts

; end sub fader_s_init
} ; !zone

; **************************************************

!zone {
fader_s_update
    lda fader_v_finished
    +branch_if_false +
    rts ; EXIT POINT.

+
    jsr fader_s_update_recolorize
    rts
; end sub fader_s_update
} ; !zone

; **************************************************

; INPUTS:   X = row index
!zone {
fader_s_set_addr_pointers
    ; Destination:
    lda fader_l_ROWS_COLRAM_LO,x 
    sta FADE_CR_LO
    lda fader_l_ROWS_COLRAM_HI,x 
    sta FADE_CR_HI
    lda fader_l_ROWS_SCRRAM_LO,x 
    sta FADE_SR_LO
    lda fader_l_ROWS_SCRRAM_HI,x 
    sta FADE_SR_HI

    ; Source:
    lda fader_v_type
    cmp #fader_c_TYPE_BLACK
    beq .end
    cmp #fader_c_TYPE_RECOLORIZE
    beq +
    ; The row addresses for greyscale are offset by 'num rows'.
    txa
    clc
    adc #fader_c_NUM_ROWS 
    tax

+
    lda fader_l_ROWS_COLRAM_SRC_LO,x
    sta FADE_CR_SRC_LO 
    lda fader_l_ROWS_COLRAM_SRC_HI,x
    sta FADE_CR_SRC_HI 
    lda fader_l_ROWS_SCRRAM_SRC_LO,x
    sta FADE_SR_SRC_LO
    lda fader_l_ROWS_SCRRAM_SRC_HI,x
    sta FADE_SR_SRC_HI

.end
    rts
; end sub fader_s_set_addr_pointers
} ; !zone

; **************************************************

!zone {
fader_s_prepare_greyscale_table
    ldy #0

.loop_top
    lda fader_c_COLRAM_SRC_BASE,y
    jsr fader_s_cr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_CR,y
    lda fader_c_COLRAM_SRC_BASE+250,y
    jsr fader_s_cr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_CR+250,y
    lda fader_c_COLRAM_SRC_BASE+500,y
    jsr fader_s_cr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_CR+500,y
    lda fader_c_COLRAM_SRC_BASE+750,y
    jsr fader_s_cr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_CR+750,y

    lda fader_c_SCRRAM_SRC_BASE,y
    jsr fader_s_sr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_SR,y
    lda fader_c_SCRRAM_SRC_BASE+250,y
    jsr fader_s_sr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_SR+250,y
    lda fader_c_SCRRAM_SRC_BASE+500,y
    jsr fader_s_sr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_SR+500,y
    lda fader_c_SCRRAM_SRC_BASE+750,y
    jsr fader_s_sr_byte_to_greyscale
    sta fader_l_GREYSCALE_TABLE_SR+750,y

    iny
    cpy #250
    bne .loop_top

    rts
; end sub fader_s_prepare_greyscale_table
} ; !zone

; **************************************************

; INPUT:    A = colorized byte.
; OUTPUT:   A = greyscale byte.
!zone {
fader_s_sr_byte_to_greyscale
    ; Use MATHS0 for temporary storage of upper nybble.
    ldx #0
    stx MATHS0

    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda fader_l_FADE_TO_GREY,x
    asl
    asl
    asl
    asl
    sta MATHS0

    pla
    and #$0f
    tax
    lda fader_l_FADE_TO_GREY,x
    ora MATHS0
    
    rts
; end sub fader_s_sr_byte_to_greyscale
} ; !zone

; **************************************************

; INPUT:    A = colorized byte.
; OUTPUT:   A = greyscale byte.
!zone {
fader_s_cr_byte_to_greyscale
    and #$0f
    tax
    lda fader_l_FADE_TO_GREY,x
    rts
; end sub fader_s_cr_byte_to_greyscale
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************

