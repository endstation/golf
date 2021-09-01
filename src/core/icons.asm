; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


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
; INPUTS:   P0-P1 = start address of data, P2 = global color RAM (or $ff)
!zone {
.NUM_COLS = TREES_LO
.NUM_ROWS = TREES_HI
.BYTES_PER_ROW = PATTERN_LO
.COLORS_OFFSET = PATTERN_HI
.HEADER_SIZE = 5    ; in bytes.
.GLOBAL_COLOR_RAM = MATHS3

icon_s_draw
    lda P2
    sta .GLOBAL_COLOR_RAM

    ; Row and column into P2 and P3 respectively.
    ldy #0
    lda (P0),y
    sta P2
    iny
    lda (P0),y
    sta P3

    ; Store dimensions in .NUM_ROWS/COLS.
    iny
    lda (P0),y
    sta .NUM_ROWS
    iny
    lda (P0),y
    sta .NUM_COLS
    ; Mulitply cols by 8 to get number of bytes per (char) row and store.
    asl
    asl
    asl
    sta .BYTES_PER_ROW
    iny
    lda (P0),y
    sta .COLORS_OFFSET

    ; Destination address into BITMAP_LO/BITMAP_HI.
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

    ; Use X to count rows and Y to count bytes.
    ; Add 'header' size to the source address (P0-P1) so that source and
    ; destination are synchronized.
    lda P0
    clc
    adc #.HEADER_SIZE
    sta P0
    lda P1
    adc #0
    sta P1

    ldx .NUM_ROWS
.loop
    ldy .BYTES_PER_ROW
-
    dey
    cpy #$ff
    beq .next_row
    lda (P0),y
    sta (BITMAP_LO),y
    jmp -

.next_row
    ; Add .BYTES_PER_ROW to source address pointer.
    lda P0
    clc
    adc .BYTES_PER_ROW
    sta P0
    lda P1
    adc #0
    sta P1
    ; This may have been the final row (of pixel data).
    dex
    beq .done_pixels
    ; Add 320 to destination (BITMAP_LO/HI).
    lda BITMAP_LO
    clc
    adc #<320
    sta BITMAP_LO
    lda BITMAP_HI
    adc #>320
    sta BITMAP_HI
    jmp .loop

.done_pixels
    ; Now we must handle the colors.
    ; P0-P1 points to source of color data. 
    ; Put video RAM destination into P4-P5; color RAM P6-P7.
    ; Remember P2 and P3 hold row and column, but in terms of pixels not 
    ; character cells.  So need to divide first by 8 and 4 respectively.
    lsr P2
    lsr P2
    lsr P2
    lsr P3
    lsr P3
    ldx P2
    lda dp_l_VIDEO_RAM_ROWS_LO,x
    sta P4
    lda dp_l_VIDEO_RAM_ROWS_HI,x
    sta P5
    lda dp_l_COLOR_RAM_ROWS_LO,x
    sta P6
    lda dp_l_COLOR_RAM_ROWS_HI,x
    sta P7
    ; Add column.
    lda P4
    clc
    adc P3
    sta P4
    lda P5
    adc #0
    sta P5
    lda P6
    clc
    adc P3
    sta P6
    lda P7
    adc #0
    sta P7

    ; P0-P1 points to start of video RAM color data.
    ; Add size of that block (= .COLORS_OFFSET) to this to get color RAM data,
    ; and store in MATHS0-MATHS1.
    ; So now we have two sources.
    lda P0
    clc
    adc .COLORS_OFFSET
    sta MATHS0
    lda P1
    adc #0
    sta MATHS1

    ; X counts down number of rows; Y counts up number of columns.
    ldx .NUM_ROWS
--
    ldy #0
-
    lda (P0),y
    sta (P4),y
    lda .GLOBAL_COLOR_RAM
    bpl +
    lda (MATHS0),y
+
    sta (P6),y
    iny
    cpy .NUM_COLS
    bne -

    dex
    beq .end

    ; Add 40 (i.e. one row) to destination pointers, ready for the next row.
    lda P4
    clc
    adc #40
    sta P4
    lda P5
    adc #0
    sta P5
    lda P6
    clc
    adc #40
    sta P6
    lda P7
    adc #0
    sta P7
    ; Next row so add .NUM_COLS to source pointers.
    lda P0
    clc
    adc .NUM_COLS
    sta P0
    lda P1
    adc #0
    sta P1
    lda MATHS0
    clc
    adc .NUM_COLS
    sta MATHS0
    lda MATHS1
    adc #0
    sta MATHS1

    jmp --

.end
    rts
; end sub icon_s_draw
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
; **************************************************

