; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


ingm_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************


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
.NUM_PAGES = 10
;.patterns   !byte   %10000000,%00100000,%00001000,%00000010
.patterns   !byte   %00101010,%10001010,%10100010,%10101000
.PAGE_COUNT = MATHS2

ingm_s_draw_rough
    ; Start address into MATHS0-MATHS1.
    lda #<gfxs_c_BITMAP_BASE+(16*40*8)
    sta MATHS0
    lda #>gfxs_c_BITMAP_BASE+(16*40*8)
    sta MATHS1

    ; Write .PATTERN to the next 10 pages of memory.
    ldx #.NUM_PAGES
    stx .PAGE_COUNT
    ldy #0

-
    jsr rand_s_get_fast
    and #$03
    tax
    lda .patterns,x 
    sta (MATHS0),y
    iny
    bne -

    ldx .PAGE_COUNT
    dex
    beq .end
    stx .PAGE_COUNT
    inc MATHS1
    jmp -

.end
    rts
; end sub ingm_s_draw_rough
} ; !zone

; **************************************************

; INPUTS:   P0 = row (chars), P1 = column (chars),
;           P4-P5 = ptr to pattern matrix
!zone {
.DATA_ITER = CAMERA0
.CHARS_ADVANCED = CAMERA1
.ROW_ADDEND_LO = CAMERA2
.ROW_ADDEND_HI = CAMERA3
.END_OF_ROW = $fe
.BLANK_TILE = $ff
.END_OF_PATTERN = $fd
; NOTE: .END refers to ONE-PAST-THE-END!!!
.BEGIN = EDGES_LO
.END = EDGES_HI
.W_CHARS = WS_X_LO 
.COLUMN_COUNT = WS_X_HI
.CURRENT_COL = FADE_CR_LO
.CURRENT_ROW = FADE_CR_HI
; NOTE: .BASE_COL is where we go back to at the end of each row.
.BASE_COL = FADE_SR_LO

ingm_s_draw_tile_pattern
    ; Extract object's width from data block.
    ldy #0
    lda (P4),y
    sta .W_CHARS

    ; Calculate the BEGINNING and (one-past-the) END chars that must be drawn.
    ; I.e. work out any clipping that needs to take place at lhs or rhs.    
    ; .BEGIN: if column (=P1) is negative, 2's comp is the char to start at;
    ; otherwise 0.
    lda P1
    bpl +
    +nega

    bne ++
+
    lda #0
++
    sta .BEGIN
    ; Quick test to see if .BEGIN is >= .W_CHARS, because if it is there's 
    ; nothing visible on-screen that needs to be drawn!
    cmp .W_CHARS
    bcc +
    rts ; EXIT POINT.

+
    lda #40
    sec
    sbc P1
    cmp .W_CHARS
    bcc +
    ; No clipping needed at rhs.
    lda .W_CHARS
+
    sta .END

    ; If P1 (= destination column) was negative, set it to 0 here.
    lda P1
    bpl +
    +clr P1
+
    ; NOTE: value of P1 is in the accumulator.
    sta .CURRENT_COL
    sta .BASE_COL
    lda P0
    sta .CURRENT_ROW

    ; Multiply row by 8 and column by 4 to get destination location in bitmap
    ; coordinates.
    lda P0
    asl
    asl
    asl
    sta P0
    lda P1
    asl
    asl
    sta P1
    ; Bitmap destination address into BITMAP_LO/HI.
    ldx P0
    ldy P1
    lda dp_l_BITMAP_ROWS_LO,x
    clc
    adc dp_l_BITMAP_COLS_LO,y
    sta BITMAP_LO
    lda dp_l_BITMAP_ROWS_HI,x
    adc dp_l_BITMAP_COLS_HI,y
    sta BITMAP_HI

    lda #0
    sta .CHARS_ADVANCED
    sta .COLUMN_COUNT
    ; NOTE: .DATA_ITER begins at 1 because we need to jump over the object
    ; width.
    lda #1
    sta .DATA_ITER

.loop_top
    ldy .DATA_ITER
    lda (P4),y
    cmp #.END_OF_ROW
    beq .next_row
    cmp #.END_OF_PATTERN
    beq .end
    cmp #.BLANK_TILE
    bne .valid_tile
    ; So this is a blank tile.  How we deal with it depends on whether or not
    ; we're off-screen lhs.
    lda .COLUMN_COUNT
    cmp .BEGIN
    ; NOTE: following two branches cover all eventualities!  We'll never fall
    ; through to '.valid_tile'.
    bcc .skip_tile_lhs
    bcs .next_tile

.valid_tile
    ; A valid tile.  Get source ptr into TREES_LO/HI.
    tax
    ; First let's see if we should draw this tile (or if it's off-screen).
    ; 'Current char' (stored in .COLUMN_COUNT) should be >= .BEGIN and 
    ; < .END.
    lda .COLUMN_COUNT
    cmp .BEGIN
    bcc .skip_tile_lhs
    cmp .END
    bcs .next_tile

    ; NOTE: this is a dummy routine - you must write address of your own 
    ; routine here before calling!!!
ingm_mod0 = *+1
    jsr $c000   ;ingm_s_default_draw_tile

.next_tile
    ; Advance destination pointer by 8 bytes and increment '.CHARS_ADVANCED'.
    inc .CHARS_ADVANCED
    inc .CURRENT_COL
    lda BITMAP_LO
    clc
    adc #8
    sta BITMAP_LO
    lda BITMAP_HI
    adc #0
    sta BITMAP_HI
.skip_tile_lhs
    inc .DATA_ITER
    inc .COLUMN_COUNT
    bne .loop_top

.next_row
    inc .DATA_ITER
    ; We will add this to the destination pointer: 320-(c*8), where c is the 
    ; number of chars 'advanced'.
    lda .CHARS_ADVANCED
    asl
    asl
    asl
    sta MATHS0
    lda #<320
    sec
    sbc MATHS0
    sta .ROW_ADDEND_LO
    lda #>320
    sbc #0
    sta .ROW_ADDEND_HI
    lda BITMAP_LO
    clc
    adc .ROW_ADDEND_LO
    sta BITMAP_LO
    lda BITMAP_HI
    adc .ROW_ADDEND_HI
    sta BITMAP_HI
    lda #0
    sta .CHARS_ADVANCED
    sta .COLUMN_COUNT
    inc .CURRENT_ROW
    lda .BASE_COL
    sta .CURRENT_COL
    jmp .loop_top

.end
    rts
; end sub ingm_s_draw_tile_pattern
} ; !zone

; **************************************************

; INPUTS:   X = tile ID; BITMAP_LO/HI = destination char.
!zone {
ingm_s_default_draw_tile
    lda bdrop_l_TILES_ADDR_LO,x
    sta TREES_LO
    lda bdrop_l_TILES_ADDR_HI,x
    sta TREES_HI

    ; Copy 8 bytes from source->destination.
    ldy #0
    +utils_m_kernal_out
-
    lda (BITMAP_LO),y
    ora (TREES_LO),y
    sta (BITMAP_LO),y
    iny
    cpy #8
    bne -
    +utils_m_kernal_in

    rts
; end sub ingm_s_default_draw_tile
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

ingm_c_SIZE = *-ingm_c_BEGIN
