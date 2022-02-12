; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "sctblsp.o",cbm
!source "play_labels.asm"
!source "../core/mymacros.asm"


; Start to assemble this at beginning of quads variables block.
; Doesn't matter if this moves because we've got its latest address in the
; file 'play_labels.asm'.
*= quads_n

sctblsp_c_BEGIN = *


; NOTE: put this subroutine at the very top of the file so other modules
; (specifically, 'play') know where it is!
!zone {
sc_s_init
    ; Clear lines 1,2 and 3 of the bitmap.
    ldx #0
    lda #0
-
    sta gfxs_c_BITMAP_BASE+320,x
    sta gfxs_c_BITMAP_BASE+320+240,x
    sta gfxs_c_BITMAP_BASE+320+480,x
    sta gfxs_c_BITMAP_BASE+320+720,x
    inx
    cpx #240
    bne -

    ldx shared_v_num_players
    jsr sc_s_prepare_colors
    jsr sc_s_draw

    ; Lock fire button on joystick 2.
    ldx #joy_c_PORT2
    +joy_m_lock_fire
    
    rts
; end sub sc_s_init
} ; !zone




; *****************
; *** CONSTANTS ***
; *****************
; 18 addresses for each player.
sc_l_CELL_ADDR_LO
!for k,4 {
    !for m,18 {
        !byte <gfxs_c_BITMAP_BASE+(2*320)+((k-1)*5*320)+(((m-1)/9)*2*320)+(4*8)+(((m-1)%9)*3*8)
    } ; !for m
} ; !for k
sc_l_CELL_ADDR_HI
!for k,4 {
    !for m,18 {
        !byte >gfxs_c_BITMAP_BASE+(2*320)+((k-1)*5*320)+(((m-1)/9)*2*320)+(4*8)+(((m-1)%9)*3*8)
    } ; !for l
} ; !for k

sc_l_CELL_TOTALS_ADDR_LO
!for k,4 {
    !byte   <gfxs_c_BITMAP_BASE+((2+(5*(k-1)))*320)+(33*8)
    !byte   <gfxs_c_BITMAP_BASE+((4+(5*(k-1)))*320)+(33*8)
} ; !for k
sc_l_CELL_TOTALS_ADDR_HI
!for k,4 {
    !byte   >gfxs_c_BITMAP_BASE+((2+(5*(k-1)))*320)+(33*8)
    !byte   >gfxs_c_BITMAP_BASE+((4+(5*(k-1)))*320)+(33*8)
} ; !for k

sc_l_BOX_CR_ADDR_LO
    !byte <COLOR_RAM+(2*40)+4
    !byte <COLOR_RAM+(7*40)+4
    !byte <COLOR_RAM+(12*40)+4
    !byte <COLOR_RAM+(17*40)+4
sc_l_BOX_CR_ADDR_HI
    !byte >COLOR_RAM+(2*40)+4
    !byte >COLOR_RAM+(7*40)+4
    !byte >COLOR_RAM+(12*40)+4
    !byte >COLOR_RAM+(17*40)+4
sc_l_BOX_SR_ADDR_LO
    !byte <gfxs_c_DISPLAY_BASE+(2*40)+4
    !byte <gfxs_c_DISPLAY_BASE+(7*40)+4
    !byte <gfxs_c_DISPLAY_BASE+(12*40)+4
    !byte <gfxs_c_DISPLAY_BASE+(17*40)+4
sc_l_BOX_SR_ADDR_HI
    !byte >gfxs_c_DISPLAY_BASE+(2*40)+4
    !byte >gfxs_c_DISPLAY_BASE+(7*40)+4
    !byte >gfxs_c_DISPLAY_BASE+(12*40)+4
    !byte >gfxs_c_DISPLAY_BASE+(17*40)+4

sc_l_SYMBOL_ADDR_LO !byte <sc_l_PLUS_SYMBOL,<sc_l_PLUS_SYMBOL_RHS,<sc_l_MINUS_SYMBOL,<sc_l_MINUS_SYMBOL_RHS,<sc_l_EQUALS_SYMBOL,<sc_l_EQUALS_SYMBOL_RHS
sc_l_SYMBOL_ADDR_HI !byte >sc_l_PLUS_SYMBOL,>sc_l_PLUS_SYMBOL_RHS,>sc_l_MINUS_SYMBOL,>sc_l_MINUS_SYMBOL_RHS,>sc_l_EQUALS_SYMBOL,>sc_l_EQUALS_SYMBOL_RHS

sc_c_SYMBOL_TYPE_PLUS       = 0
sc_c_SYMBOL_TYPE_PLUS_RHS   = 1
sc_c_SYMBOL_TYPE_MINUS      = 2
sc_c_SYMBOL_TYPE_MINUS_RHS  = 3
sc_c_SYMBOL_TYPE_EQUALS     = 4
sc_c_SYMBOL_TYPE_EQUALS_RHS = 5

sc_l_PLUS_SYMBOL        !byte $aa,$ff,$ef,$ab,$ef
sc_l_PLUS_SYMBOL_RHS    !byte $aa,$fe,$ee,$aa,$ee
sc_l_MINUS_SYMBOL       !byte $aa,$ff,$ff,$eb,$ff
sc_l_MINUS_SYMBOL_RHS   !byte $aa,$fe,$fe,$ea,$fe
sc_l_EQUALS_SYMBOL      !byte $aa,$ff,$eb,$ff,$eb
sc_l_EQUALS_SYMBOL_RHS  !byte $aa,$fe,$ea,$fe,$ea

sc_l_SCORES_OFFSETS   !byte   0,18,36,54
sc_l_BOX_COLORS !byte   CYAN,LIGHT_BLUE,YELLOW,LIGHT_GREEN
sc_c_BOX_WIDTH2 = 27
sc_c_BOX_HEIGHT = 4
sc_l_NAME_ROW_CHARS !byte   1,6,11,16
sc_c_NAME_COL_CHARS = 4


; *****************
; *** VARIABLES ***
; *****************
; NOTE: 6 chars, i.e. 6*8=48 bytes!
sc_v_cell_buffer    !fill   6*8,0
sc_v_digits_buffer  !fill   6*8,0

; For your convenience: boolean to record whether this is hole 9 or 18.
; Used by draw routines.
sc_v_is_rightmost_cell  !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   X = num players.

!zone {
.PLAYER_ITER = TREES_LO
.SLOT_COLOR  = TREES_HI
.NUM_PLAYERS = CAMERA0

sc_s_prepare_colors
    stx .NUM_PLAYERS

    ldx #0
.loop_top
    stx .PLAYER_ITER

    ; Color RAM address into MATHS2-3; display RAM into MATHS4-5.
    lda sc_l_BOX_CR_ADDR_LO,x
    sta MATHS2
    lda sc_l_BOX_CR_ADDR_HI,x
    sta MATHS3
    lda sc_l_BOX_SR_ADDR_LO,x
    sta MATHS4
    lda sc_l_BOX_SR_ADDR_HI,x
    sta MATHS5
    lda sc_l_BOX_COLORS,x
    sta .SLOT_COLOR

    ; 4 rows, each 27 chars in width.
    ; Use X to count rows.
    ldx #sc_c_BOX_HEIGHT
.row_loop
    ldy #0
-
    lda #GREY1
    sta (MATHS4),y
    lda .SLOT_COLOR
    sta (MATHS2),y
.skip
    iny
    cpy #sc_c_BOX_WIDTH2 
    beq .skip
    cpy #sc_c_BOX_WIDTH2+1
    beq .skip
    cpy #32
    bne -
    ; End of row.
    dex
    beq .next_player
    ; More rows to go, so add 40 to both pointers.
    lda MATHS2
    clc
    adc #40
    sta MATHS2
    lda MATHS3
    adc #0
    sta MATHS3
    lda MATHS4
    clc
    adc #40
    sta MATHS4
    lda MATHS5
    adc #0
    sta MATHS5
    jmp .row_loop

.next_player
    ldx .PLAYER_ITER
    inx
    cpx .NUM_PLAYERS    ;shared_v_num_players
    bne .loop_top

    rts
; end sub sc_s_prepare_colors
} ; !zone

; **************************************************

!zone {
.SLOT_ITER  = TREES_LO
.HOLE_ITER  = TREES_HI
.HOLE_END   = BITMAP_LO
.SCORES_ON  = BITMAP_HI
; NOTE: .HOLE_COUNT counts from 0 to 17, regardless of the player #.
.HOLE_COUNT = EDGES_LO
.CURRENT_PLAYER = EDGES_HI
.CELL_COUNT = VM_LO
.TOTAL_CELL_COUNT = VM_HI

sc_s_draw
    lda #1
    sta .SCORES_ON
    lda #0
    sta .HOLE_COUNT
    sta .CELL_COUNT
    sta .TOTAL_CELL_COUNT

    ldx #0
.player_loop
    stx .SLOT_ITER
    lda players_v_playing_order,x
    sta .CURRENT_PLAYER
    tax

    lda sc_l_SCORES_OFFSETS,x
    sta .HOLE_ITER
    clc
    adc round_v_current_hole
    sta .HOLE_END
    ldx .HOLE_ITER
.hole_loop
    stx .HOLE_ITER
    
    lda #0
    sta sc_v_is_rightmost_cell
    lda .HOLE_COUNT
    cmp #8
    beq +
    cmp #17
    bne ++
+
    inc sc_v_is_rightmost_cell
++

    jsr sc_s_clear_digits_buffer

    ; If playing only front or back 9, we will 'ghost out' any slots that won't
    ; be used...
    lda shared_v_holes
    beq .valid_slot
    cmp #shared_c_PLAYING_FRONT_9
    bne .playing_back_9
    ; So we're playing the front 9 only.  Any slots after #8 are invalid.
    lda .HOLE_COUNT
    cmp #9
    bcc .valid_slot
    bcs .must_ghost
.playing_back_9
    ; Any slots before #9 are invalid.
    lda .HOLE_COUNT
    cmp #9
    bcs .valid_slot
.must_ghost
    jsr sc_s_ghost_digits_buffer 
    jsr sc_s_draw_cell_border_to_buffer
    jmp .to_bitmap

.valid_slot
    lda .SCORES_ON
    +branch_if_false .draw_border
    ; Get score for current hole.
    ldx .HOLE_ITER
    lda sc_v_scores,x
    jsr sc_s_write_digit_chars_to_buffer

.draw_border
    jsr sc_s_draw_cell_border_to_buffer

    lda .SCORES_ON
    beq +
    ldx .HOLE_ITER
    ldy .HOLE_COUNT
    jsr sc_s_get_rel_to_par
    lda sc_l_SYMBOL_ADDR_LO,x
    sta WS_X_LO 
    lda sc_l_SYMBOL_ADDR_HI,x
    sta WS_X_HI 
    ldy #4
-
    lda (WS_X_LO),y
    sta sc_v_digits_buffer+(2*8),y
    dey
    bpl -

+
.to_bitmap
    ; Buffer is now ready to be drawn to the bitmap screen.
    ldx .CELL_COUNT
    lda sc_l_CELL_ADDR_LO,x
    sta MATHS2
    lda sc_l_CELL_ADDR_HI,x
    sta MATHS3
    jsr sc_s_write_buffer_to_bitmap

    ; Check next hole.
    inc .CELL_COUNT
    inc .HOLE_COUNT
    ldx .HOLE_COUNT
    cpx #18
;    beq .check_next_player
    beq .running_totals

    ; .HOLE_ITER must be incremented no matter what.  Turn '.SCORES_ON' off
    ; (i.e. set it to 0) when .HOLE_ITER is equal to .HOLE_END (because there
    ; are no more scores to draw after this point, but we still need to draw
    ; the empty cells).
    ldx .HOLE_ITER
    inx
    stx .HOLE_ITER
    cpx .HOLE_END
    bne +
    dec .SCORES_ON
+
    jmp .hole_loop

.running_totals
    ; TODO: ...
    ldx .TOTAL_CELL_COUNT
    ldy .CURRENT_PLAYER
    jsr sc_s_draw_totals
    ldx .TOTAL_CELL_COUNT
    inx
    inx
    stx .TOTAL_CELL_COUNT

.check_next_player
    ldx .CURRENT_PLAYER 
    ldy .SLOT_ITER
    jsr sc_s_draw_name_for_score_card
    ldx .SLOT_ITER
    inx
    cpx shared_v_num_players
    beq .end
    ; RESET THESE FOR NEXT PLAYER!!!
    ldy #1
    sty .SCORES_ON
    dey
    sty .HOLE_COUNT

    jmp .player_loop

.end
    rts
; end sub sc_s_draw
} ; !zone

; **************************************************

; INPUTS:   X = current player, Y = slot
!zone {
sc_s_draw_name_for_score_card
    ; So X already has player #.
    ; Need to fill in destination row and column (in chars) in P0 and P1
    ; respectively.
    lda sc_l_NAME_ROW_CHARS,y
    sta P0
    lda #sc_c_NAME_COL_CHARS
    sta P1
    jsr sc_s_draw_name_and_score
    rts
; end sub sc_s_draw_name_for_score_card
} ; !zone

; **************************************************

; INPUTS:   X = 'total' cell count, Y = current player
!zone {
.ITER = COLORS_LO
.COUNT = COLORS_HI
.PLAYER = LINE_X0_LO

sc_s_draw_totals
    stx .COUNT
    sty .PLAYER

    ldx #2
.loop
    stx .ITER

    jsr sc_s_clear_digits_buffer
    jsr sc_s_check_if_playing_18_holes_for_totals
    bcs +

    ldx .PLAYER
    ; NOTE: second time round this will load from back 9.
    lda sc_v_front9_scores,x
    beq +
    jsr sc_s_write_digit_chars_to_buffer
+
    jsr sc_s_draw_cell_border_to_buffer
    ldx .COUNT
    lda sc_l_CELL_TOTALS_ADDR_LO,x
    sta MATHS2
    lda sc_l_CELL_TOTALS_ADDR_HI,x
    sta MATHS3
    jsr sc_s_write_buffer_to_bitmap

    ldx .ITER
    dex
    beq .end
    inc .COUNT
    lda .PLAYER
    clc
    adc #4
    sta .PLAYER
    bne .loop

.end
    rts
; end sub sc_s_draw_totals
} ; !zone

; **************************************************

; INPUTS:   X = hole iterator (takes into account player)
;           Y = hole count (in range: [0,18)).
; OUTPUTS:  X = index into lo/hi addresses for symbol char tables.
!zone {
sc_s_get_rel_to_par
    lda sc_v_scores,x
    cmp sc_v_pars,y
    beq .even
    bcc .below

    ; So must be above...
    ldx #sc_c_SYMBOL_TYPE_PLUS
    lda sc_v_is_rightmost_cell
    +branch_if_false +
    inx
+
    rts ; EXIT POINT.

.even
    ldx #sc_c_SYMBOL_TYPE_EQUALS
    lda sc_v_is_rightmost_cell
    +branch_if_false +
    inx
+
    rts ; EXIT POINT.

.below
    ldx #sc_c_SYMBOL_TYPE_MINUS
    lda sc_v_is_rightmost_cell
    +branch_if_false +
    inx
+
    rts
; end sub sc_s_get_rel_to_par
} ; !zone

; **************************************************

!zone {
sc_s_clear_digits_buffer
    ldx #((6*8)-1)
    lda #$ff
-
    sta sc_v_digits_buffer,x
    dex
    bpl -
    rts
; end sub sc_s_clear_digits_buffer
} ; !zone

; **************************************************

; Helper routine for drawing.
!zone {
sc_s_draw_cell_border_to_buffer
    lda #$aa
    sta sc_v_digits_buffer
    sta sc_v_digits_buffer+8
    sta sc_v_digits_buffer+16
    sta sc_v_digits_buffer+31
    sta sc_v_digits_buffer+39
    sta sc_v_digits_buffer+47

    ldx #0
-
    lda sc_v_digits_buffer+1,x
    and #$3f
    ora #$80
    sta sc_v_digits_buffer+1,x
    lda sc_v_digits_buffer+24,x
    and #$3f
    ora #$80
    sta sc_v_digits_buffer+24,x

    ; FIXME: find a better solution for this!
    lda sc_v_is_rightmost_cell
    +branch_if_false ++

    lda sc_v_digits_buffer+17,x
    and #$fc
    ora #$02
    sta sc_v_digits_buffer+17,x
    lda sc_v_digits_buffer+40,x
    and #$fc
    ora #$02
    sta sc_v_digits_buffer+40,x
++
    inx
    cpx #7
    bne -

    
    rts
; end sub sc_s_draw_cell_border_to_buffer
} ; !zone

; **************************************************

; A helper function for sc_s_draw.
; INPUTS:   A = value to be represented.
!zone {
sc_s_write_digit_chars_to_buffer
    cmp #10
    bcs .two_digits

    ; 1 digit.  Add value for ASCII '0' to get index into font data.
    clc
    adc #font_c_ASCII_0 
    tax
    lda font_l_CHAR_DATA_LO,x 
    sta MATHS2
    lda font_l_CHAR_DATA_HI,x 
    sta MATHS3

    ; LAYOUT:
    ; *-----*-----*-----*
    ; |  0  |  1  |  2  |
    ; *-----*-----*-----*
    ; |  3  |  4  |  5  |
    ; *-----*-----*-----*
    ; Copy this char data into bytes #1 and #4 of buffer.
    ldy #0
-
    lda (MATHS2),y
    jsr utils_s_ror_two_bits
    sta sc_v_digits_buffer+(1*8)+5,y
    iny
    cpy #3
    bne -
-
    lda (MATHS2),y
    jsr utils_s_ror_two_bits
    sta sc_v_digits_buffer+(4*8)-3,y
    iny
    cpy #8
    bne -
    rts ; EXIT POINT.

.two_digits
    ; So we need to break this down into two decimal digits.
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; There should be exactly two digits.  LSB goes into cells 2 and 5, MSB 
    ; into cells 1 and 4.
    ; Let's set up two pointers to char data.  MATHS2-3 and MATHS4-5.
    lda utils_v_dec_digits     
    clc
    adc #font_c_ASCII_0 
    tax
    lda font_l_CHAR_DATA_LO,x
    sta MATHS2
    lda font_l_CHAR_DATA_HI,x
    sta MATHS3
    lda utils_v_dec_digits+1     
    clc
    adc #font_c_ASCII_0 
    tax
    lda font_l_CHAR_DATA_LO,x
    sta MATHS4
    lda font_l_CHAR_DATA_HI,x
    sta MATHS5

    ldy #0
-
    lda (MATHS2),y
    sta sc_v_digits_buffer+(2*8)+5,y
    lda (MATHS4),y
    sta sc_v_digits_buffer+8+5,y
    iny
    cpy #3
    bne -
-
    lda (MATHS2),y
    sta sc_v_digits_buffer+(5*8)-3,y
    lda (MATHS4),y
    sta sc_v_digits_buffer+(4*8)-3,y
    iny
    cpy #8
    bne -

    ; Shift 0,1,2 two bits left.
    ; Then 3,4,5 (two bits left).
    ; FIXME: messy!!!
    ldx #0
.shift_loop
    asl sc_v_digits_buffer+(2*8),x
    rol sc_v_digits_buffer+8,x
    rol sc_v_digits_buffer,x
    asl sc_v_digits_buffer+(2*8),x
    rol sc_v_digits_buffer+8,x
    rol sc_v_digits_buffer,x
    ; Replace the 'background' color lost at right edge.
    lda #$03
    ora sc_v_digits_buffer+(2*8),x
    sta sc_v_digits_buffer+(2*8),x

    asl sc_v_digits_buffer+(5*8),x
    rol sc_v_digits_buffer+(4*8),x
    rol sc_v_digits_buffer+(3*8),x
    asl sc_v_digits_buffer+(5*8),x
    rol sc_v_digits_buffer+(4*8),x
    rol sc_v_digits_buffer+(3*8),x
    lda #$03
    ora sc_v_digits_buffer+(5*8),x
    sta sc_v_digits_buffer+(5*8),x

    inx 
    cpx #8
    bne .shift_loop

    rts
; end sub sc_s_write_digit_chars_to_buffer
} ; !zone

; **************************************************

; INPUTS:   MATHS2-3 = destination of top-left byte.
!zone {
sc_s_write_buffer_to_bitmap
    lda MATHS2
    clc
    adc #<320
    sta MATHS4
    lda MATHS3
    adc #>320
    sta MATHS5
    
    ldy #0
-
    lda sc_v_digits_buffer,y
    sta (MATHS2),y
    lda sc_v_digits_buffer+(3*8),y
    sta (MATHS4),y
    iny
    cpy #(3*8)
    bne -

    rts
; end sub sc_s_write_buffer_to_bitmap
} ; !zone

; **************************************************

!zone {
sc_s_ghost_digits_buffer
    lda #$bb
    sta sc_v_digits_buffer+12
    sta sc_v_digits_buffer+13
    sta sc_v_digits_buffer+14
    sta sc_v_digits_buffer+33
    sta sc_v_digits_buffer+34
    sta sc_v_digits_buffer+35
    lda #$ef
    sta sc_v_digits_buffer+15
    sta sc_v_digits_buffer+32
    rts

; end sub sc_s_ghost_digits_buffer
} ; !zone

; **************************************************

; INPUTS:   COLORS_LO (2 = front 9 total, 1 = back 9 total).
; OUTPUTS:  C flag clear - OK to process total; C flag set - not used
!zone {
sc_s_check_if_playing_18_holes_for_totals
    lda shared_v_holes
    beq .end
    cmp #shared_c_PLAYING_FRONT_9
    beq .front9

    ; So we're playing back 9 only.
    lda COLORS_LO
    cmp #2
    bne .end
    beq +

.front9
    lda COLORS_LO
    cmp #1
    bne .end
+
    jsr sc_s_ghost_digits_buffer
    sec
    rts ; EXIT POINT.

.end
    clc
    rts
; end sub sc_s_check_if_playing_18_holes_for_totals
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

sctblsp_c_SIZE = *-sctblsp_c_BEGIN


