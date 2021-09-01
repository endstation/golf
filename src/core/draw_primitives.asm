; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
dp_c_BITMAP_START = gfxs_c_BITMAP_BASE

; Record the addresses of the start of each bitmap row.
dp_l_BITMAP_ROWS_LO
!for i,200 {
    !byte <(dp_c_BITMAP_START+((i-1)%8)+(((i-1)/8)*320))
} ; !for
dp_l_BITMAP_ROWS_HI
!for i,200 {
    !byte >(dp_c_BITMAP_START+((i-1)%8)+(((i-1)/8)*320))
} ; !for
dp_l_BITMAP_COLS_LO
!for i,160 {
    !byte <(((i-1)/4)*8)
} ; !for
dp_l_BITMAP_COLS_HI
!for i,160 {
    !byte >(((i-1)/4)*8)
} ; !for

dp_l_BITMAP_MASKS !byte   %00111111,%11001111,%11110011,%11111100
dp_l_BITMAP_PATTERNS
    !byte   $00,$40,$80,$c0 ; mask position 0 (starting at lhs)
    !byte   $00,$10,$20,$30 ; mask position 1
    !byte   $00,$04,$08,$0c ; etc.
    !byte   $00,$01,$02,$03

dp_l_BYTE_PATTERNS    !byte   $00,$55,$aa,$ff

dp_l_COLOR_RAM_ROWS_LO
!for i,25 {
    !byte <(COLOR_RAM+((i-1)*40))
} ; !for
dp_l_COLOR_RAM_ROWS_HI
!for i,25 {
    !byte >(COLOR_RAM+((i-1)*40))
} ; !for

dp_l_VIDEO_RAM_ROWS_LO
!for i,25 {
    !byte <(gfxs_c_DISPLAY_BASE+((i-1)*40))
} ; !for
dp_l_VIDEO_RAM_ROWS_HI
!for i,25 {
    !byte >(gfxs_c_DISPLAY_BASE+((i-1)*40))
} ; !for

; Lookup table for chars (0-63).
dp_l_CHARS_LO
!for i,64 {
    !byte <(gfxs_c_CHARS_TO+((i-1)*8))
} ; !for
dp_l_CHARS_HI
!for i,64 {
    !byte >(gfxs_c_CHARS_TO+((i-1)*8))
} ; !for


; *****************
; *** VARIABLES ***
; *****************
dp_v_edges_iter   !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   X = x coord, Y = y coord, A = color code (0-3)
!zone {
.mask       !byte   0
.pattern    !byte   0

dp_s_draw_pixel
    ; Store color code in '.pattern' variable until we need it.
    sta .pattern
    
    lda dp_l_BITMAP_ROWS_LO,y
    clc
    adc dp_l_BITMAP_COLS_LO,x
    sta BITMAP_LO
    lda dp_l_BITMAP_ROWS_HI,y
    adc dp_l_BITMAP_COLS_HI,x
    sta BITMAP_HI

    ; Find mask and pattern.
    txa
    ; Get remainder after dividing by 4.
    and #$03
    tax
    lda dp_l_BITMAP_MASKS,x
    sta .mask
    ; Multiply mask index by 4, then add the color code.  This is the index
    ; into the 'patterns' table.
    txa
    asl
    asl
    clc
    adc .pattern    ; i.e. color code!
    tax
    lda dp_l_BITMAP_PATTERNS,x
    sta .pattern

    +utils_m_kernal_out
    ldy #0
    lda (BITMAP_LO),y
    and .mask
    ora .pattern
    sta (BITMAP_LO),y
    +utils_m_kernal_in
    
    rts
; end sub dp_s_draw_pixel
} ; !zone

; **************************************************

; INPUTS:   X = x coord, Y = y coord, A = color code (0-3)
!zone {
dp_s_draw_byte
    ; Color code onto stack until we need it.
    pha

    ; Look up the bitmap byte we need and load address into zeropage.
    lda dp_l_BITMAP_ROWS_LO,y
    clc
    adc dp_l_BITMAP_COLS_LO,x
    sta BITMAP_LO
    lda dp_l_BITMAP_ROWS_HI,y
    adc dp_l_BITMAP_COLS_HI,x
    sta BITMAP_HI

    ldy #0
    pla
    sta (BITMAP_LO),y

    rts
; end sub dp_s_draw_byte
} ; !zone

; **************************************************

; INPUTS:   LINE_X0_LO, LINE_Y0_LO, LINE_X1_LO, LINE_Y1_LO
;           A = color code (0-3)
;           Y = index into edges array (where to store)
; OUTPUTS:  Y = 'edges' iterator - tells you how many were recorded.
!zone {
.dx                     !byte   0
.dy                     !byte   0
.error_summand_lo       !byte   0
.error_summand_hi       !byte   0
.error_subtrahend_lo    !byte   0
.error_subtrahend_hi    !byte   0
.error_lo               !byte   0
.error_hi               !byte   0
; '.info' holds information about the line: bits #7 and #6 are two booleans -
; is line x-major and is y going up, respectively.
.info                   !byte   0
; When we're x-major, 8-bit value to add to y when error becomes positive.
; It's either 1 or (-1).  Also use when y-major for increment to x.
.delta_y                !byte   0
.delta_x                !byte   0
.color_code             !byte   0

; Masks into the .info variable.
.IS_X_MAJOR =   1<<7
.IS_Y_UP    =   1<<6
.IS_X_LEFT  =   1<<5
; For self-modifying code.
.BCS_CODE   =   $b0
.BCC_CODE   =   $90
.BEQ_CODE   =   $f0 
.BIT_CODE   =   $24

dp_s_draw_line
    sta .color_code
    sty dp_v_edges_iter

    lda #0
    sta .info
    ; Default value of .delta_y is 1 assuming y is going down.  Change this to
    ; (-1 = $ff) if y going up.  Also, default .delta_x is 1 (going right).
    lda #1
    sta .delta_y
    sta .delta_x
    ; WARNING: self-modifying code!!!
    ; Reset defaults for incrementing y-major when y going down.
    lda #.BEQ_CODE
    sta .mod2
    sta .mod4
    lda #.BCC_CODE
    sta .mod3
    sta .mod5

    ; First calculate dx and dy (overall change in x and y).
    lda LINE_X1_LO
    sec
    sbc LINE_X0_LO
    sta .dx

    ; If the result of that subtraction was negative, it means x is going
    ; from right->left, so store 2's complement of result instead and 
    ; record that we're going left.
    ; NOTE: check C flag rather than using 'bpl', because result could 
    ; overflow beyond signed 8-bit bounds... (i.e. a negative result could
    ; appear positive).
    bcs +
    ; 2's comp. of dx.
    lda .dx
    eor #$ff
    clc
    adc #1
    sta .dx
    lda .info
    ora #.IS_X_LEFT
    sta .info
    lda #$ff
    sta .delta_x
    ; WARNING: self-modifying code!!!
    lda #.BCS_CODE
    sta .mod4
    lda #.BIT_CODE
    sta .mod5

+
    lda LINE_Y1_LO
    sec
    sbc LINE_Y0_LO
    sta .dy
    ; If this is negative, line is going UP so we must record this in .info
    ; and take 2's complement.
    bcs +
    lda .dy
    eor #$ff
    clc
    adc #1
    sta .dy
    lda .info
    ora #.IS_Y_UP
    sta .info
    lda #$ff
    sta .delta_y
    ; WARNING: self-modifying code!!!
    lda #.BCS_CODE
    sta .mod2
    lda #.BIT_CODE
    sta .mod3

+
    ; And now dx and dy doubled.
    ; Store these as error_subtrahend_lo/hi and error_summand_lo/hi
    ; respectively.  If it turns out that dy > dx, we'll swap them over.
    lda .dx
    asl
    sta .error_subtrahend_lo
    lda #0
    rol
    sta .error_subtrahend_hi

    lda .dy
    asl
    sta .error_summand_lo
    lda #0
    rol
    sta .error_summand_hi
    
    ; What's greater: dx or dy?
    lda .dx
    cmp .dy
    bcs .x_major

.y_major
    ; So we've got to swap error summand with error subtrahend.
    ldx .error_summand_lo
    ldy .error_summand_hi
    lda .error_subtrahend_lo
    sta .error_summand_lo
    lda .error_subtrahend_hi
    sta .error_summand_hi
    stx .error_subtrahend_lo
    sty .error_subtrahend_hi
    ; WARNING: modifying code in next section!!!
    lda .dy
    sta .mod0+1
    jmp .init_error

.x_major
    ; Record that we're x-major.
    lda .info
    ora #.IS_X_MAJOR
    sta .info
    ; WARNING: modifying code in next section!!!
    lda .dx
    sta .mod0+1
    ; Record position of first edge here when x-major.
    jsr dp_s_record_edge

.init_error
    ; Initialize '.error' value with: 2*error_summand - [dx or dy]
    lda .error_summand_lo
    sec
.mod0
    sbc #0  ; dummy value - modified from above...
    sta .error_lo
    lda .error_summand_hi
    sbc #0
    sta .error_hi

.draw
    ldx LINE_X0_LO
    ldy LINE_Y0_LO
    lda .color_code
    jsr dp_s_draw_pixel

.skip
    ; Increment to the next pixel and see if we've reached the end.
    ; Three branches for octants 1/8, 2 and 7.
    lda .info
    bmi .increment_x_major
    ; So incrementing y-major.
    ; First record the edge.
    jsr dp_s_record_edge

    lda LINE_Y0_LO
    clc
    adc .delta_y
    sta LINE_Y0_LO
    lda LINE_Y0_LO
    cmp LINE_Y1_LO
    ; When y going up, we just want to check if carry set (check error) or
    ; not (finished).  Code modified in set-up.
.mod2
    beq .check_error
.mod3
    bcc .check_error
    jmp .end

.increment_x_major
    lda LINE_X0_LO
    clc
    adc .delta_x
    sta LINE_X0_LO
    ; NOTE: if line's going right-to-left, just need to make sure we haven't
    ; strayed off-screen (to x=(-1)).  If so, we've finished.
    cmp #$ff
    beq .end
    ; Check if we've reached the end of the line.
    lda LINE_X0_LO
    cmp LINE_X1_LO
.mod4
    beq .check_error
.mod5
    bcc .check_error
    jmp .end

.check_error
    lda .error_lo
    clc
    adc .error_summand_lo
    sta .error_lo
    lda .error_hi
    adc .error_summand_hi
    sta .error_hi
    bpl +
    jmp .draw
+

    ; Adjust minor axis.
    lda .info
    bmi .adjust_y
    ; Y-major so increment x.
    lda LINE_X0_LO
    clc
    adc .delta_x
    sta LINE_X0_LO

    ; Make sure we haven't gone offscreen left.
    cmp #$ff
    bne +
    inc LINE_X0_LO
    jmp .reset_error
+
    ; ... or right!
    cmp #$a0
    bne +
    dec LINE_X0_LO
+
    jmp .reset_error
.adjust_y
    ; First record edge.
    jsr dp_s_record_edge
    lda LINE_Y0_LO
    clc
    adc .delta_y
    sta LINE_Y0_LO
    ; NOTE: what if y is now off-screen?!
    ; In that case, make adjustments as with LINE_X0_LO above.
    ; Off-screen top?
    cmp #$ff
    bne +
    inc LINE_Y0_LO
    jmp .reset_error
+
    ; ... or bottom?
    cmp #$c8
    bne .reset_error
    dec LINE_Y0_LO

.reset_error
    lda .error_lo
    sec 
    sbc .error_subtrahend_lo
    sta .error_lo
    lda .error_hi
    sbc .error_subtrahend_hi
    sta .error_hi
    jmp .draw

.end
    ldy dp_v_edges_iter
    rts
; end sub dp_s_draw_line
} ; !zone

; **************************************************

; TODO: makes a difference whether we're on lhs or rhs of min-y point?!
; Helper function for dp_draw_line.
!zone {
dp_s_record_edge
    ldy dp_v_edges_iter
    lda LINE_X0_LO
    sta (EDGES_LO),y
    inc dp_v_edges_iter
    rts
; end sub dp_s_record_edge
} ; !zone

; **************************************************

; INPUTS:   MATHS0=row, MATHS1=col, MATHS2=color code. 
!zone {
dp_s_set_color_ram_cell
    ldx MATHS0
    lda dp_l_COLOR_RAM_ROWS_LO,x
    clc
    adc MATHS1
    sta P0
    lda dp_l_COLOR_RAM_ROWS_HI,x
    adc #0
    sta P1
    ldy #0
    lda MATHS2
    sta (P0),y
    rts
; end sub dp_s_set_color_ram_cell
} ; !zone

; **************************************************

; INPUTS:   MATHS0=row, MATHS1=col, MATHS2=color code. 
!zone {
dp_s_set_video_ram_cell
    ldx MATHS0
    lda dp_l_VIDEO_RAM_ROWS_LO,x
    clc
    adc MATHS1
    sta P0
    lda dp_l_VIDEO_RAM_ROWS_HI,x
    adc #0
    sta P1
    ldy #0
    lda MATHS2
    sta (P0),y
    rts
; end sub dp_s_set_video_ram_cell
} ; !zone

; **************************************************

; INPUTS:   P6=pattern
; OUTPUTS:  P7=mask
; Build a single-byte bitmask on the fly using the given pattern.
; NOTE: we assume m/c mode (where horizontal resolution is halved).
!zone {
dp_s_build_mask
    +clr P7

    ; Start with leftmost pixel (cf. 11000000).
    lda #$c0
    bit P6
    bne +
    ora P7
    sta P7
+
    lda #$30
    bit P6
    bne +
    ora P7
    sta P7
+
    lda #$0c
    bit P6
    bne +
    ora P7
    sta P7
+
    lda #$03
    bit P6
    bne +
    ora P7
    sta P7
+

    rts
; end sub dp_s_build_mask
} ; !zone

; **************************************************

; INPUTS:   P0-P1=address of string, P2=row, P3=column, P6=string length (?!),
;           P7=color code (in upper nybble)
;           C flag set = normal pattern (or inverted if C flag clear).
; NOTE: row should be multiple of 8; column a multiple of 4.
!zone {
.ROW = P2
.COL = P3
.STR_ITER = MATHS0
.SRC_LO = MATHS1
.SRC_HI = MATHS2
.DEST_LO = P4
.DEST_HI = P5
.STR_LEN = P6
.VRAM_LO = MATHS3
.VRAM_HI = MATHS4
.COLOR_CODE = P7
.INVERT = MATHS5

dp_s_draw_string
    lda #$ff
    adc #0
    sta .INVERT

    ; Prepare the destination address, to be held in P4-P5.
    ldx .ROW
    ldy .COL
    lda dp_l_BITMAP_ROWS_LO,x
    clc
    adc dp_l_BITMAP_COLS_LO,y
    sta .DEST_LO
    lda dp_l_BITMAP_ROWS_HI,x
    adc dp_l_BITMAP_COLS_HI,y
    sta .DEST_HI

    ; Prepare VRAM address for colour.
    ; Divide row by 8 and column by 4.
    lda .ROW
    lsr
    lsr
    lsr
    tax
    lda .COL
    lsr
    lsr
    clc
    adc dp_l_VIDEO_RAM_ROWS_LO,x
    sta .VRAM_LO
    lda dp_l_VIDEO_RAM_ROWS_HI,x
    adc #0
    sta .VRAM_HI

    ; Initialize the iterator for the string.
    ldy #0
    sty .STR_ITER

    ; Next character.
.next_char
    lda (P0),y
    tax
    ; Prepare source address (based on screen code).  This goes into
    ; MATHS1-MATHS2.
    lda dp_l_CHARS_LO,x
    sta .SRC_LO
    lda dp_l_CHARS_HI,x
    sta .SRC_HI

    ; Copy one char (= 8 bytes).
    ldy #7
-
    lda (.SRC_LO),y
    eor .INVERT
    sta (.DEST_LO),y
    dey
    bpl -

    ; Any more characters?
    ldy .STR_ITER
    iny
    cpy .STR_LEN
    beq .end
    sty .STR_ITER
    ; Advance destination address by 8 bytes.
    lda .DEST_LO
    clc
    adc #8
    sta .DEST_LO
    lda .DEST_HI
    adc #0
    sta .DEST_HI
    jmp .next_char

.end
    rts
; end sub dp_s_draw_string
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

