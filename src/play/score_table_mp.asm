; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "sctblmp.o",cbm
!source "play_labels.asm"
!source "../core/mymacros.asm"


; Start to assemble this at beginning of quads variables block.
; Doesn't matter if this moves because we've got its latest address in the
; file 'play_labels.asm'.
*= quads_n

sctblmp_c_BEGIN = *


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

    jsr sc_s_prepare_colors
    jsr sc_s_draw_match_play

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
!for k,2 {
    !for m,18 {
        !byte <gfxs_c_BITMAP_BASE+(2*320)+((k-1)*5*320)+(((m-1)/9)*2*320)+(4*8)+(((m-1)%9)*3*8)
    } ; !for m
} ; !for k
sc_l_CELL_ADDR_HI
!for k,2 {
    !for m,18 {
        !byte >gfxs_c_BITMAP_BASE+(2*320)+((k-1)*5*320)+(((m-1)/9)*2*320)+(4*8)+(((m-1)%9)*3*8)
    } ; !for l
} ; !for k

sc_l_CELL_TOTALS_ADDR_LO
!for k,2 {
    !byte   <gfxs_c_BITMAP_BASE+((2+(5*(k-1)))*320)+(33*8)
    !byte   <gfxs_c_BITMAP_BASE+((4+(5*(k-1)))*320)+(33*8)
} ; !for k
sc_l_CELL_TOTALS_ADDR_HI
!for k,2 {
    !byte   >gfxs_c_BITMAP_BASE+((2+(5*(k-1)))*320)+(33*8)
    !byte   >gfxs_c_BITMAP_BASE+((4+(5*(k-1)))*320)+(33*8)
} ; !for k

sc_l_BOX_CR_ADDR_LO
    !byte <COLOR_RAM+(2*40)+4
    !byte <COLOR_RAM+(7*40)+4
;    !byte <COLOR_RAM+(12*40)+4
;    !byte <COLOR_RAM+(17*40)+4
sc_l_BOX_CR_ADDR_HI
    !byte >COLOR_RAM+(2*40)+4
    !byte >COLOR_RAM+(7*40)+4
;    !byte >COLOR_RAM+(12*40)+4
;    !byte >COLOR_RAM+(17*40)+4
sc_l_BOX_SR_ADDR_LO
    !byte <gfxs_c_DISPLAY_BASE+(2*40)+4
    !byte <gfxs_c_DISPLAY_BASE+(7*40)+4
;    !byte <gfxs_c_DISPLAY_BASE+(12*40)+4
;    !byte <gfxs_c_DISPLAY_BASE+(17*40)+4
sc_l_BOX_SR_ADDR_HI
    !byte >gfxs_c_DISPLAY_BASE+(2*40)+4
    !byte >gfxs_c_DISPLAY_BASE+(7*40)+4
;    !byte >gfxs_c_DISPLAY_BASE+(12*40)+4
;    !byte >gfxs_c_DISPLAY_BASE+(17*40)+4

sc_l_SCORES_OFFSETS   !byte   0,18
sc_l_BOX_COLORS !byte   CYAN,LIGHT_BLUE
sc_c_BOX_WIDTH2 = 27
sc_c_BOX_HEIGHT = 4
sc_l_NAME_ROW_CHARS !byte   1,6
sc_c_NAME_COL_CHARS = 4


; *****************
; *** VARIABLES ***
; *****************
; NOTE: 6 chars, i.e. 6*8=48 bytes!
sc_v_digits_buffer  !fill   8*8,0

; NOTE: this box is open-ended on the rhs.
sc_v_2x3_box
    !byte   $aa,$bf,$bf,$bf,$bf,$bf,$bf,$bf
    !byte   $aa,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !byte   $aa,$ff,$ff,$ff,$ff,$ff,$ff,$ff
    !byte   $bf,$bf,$bf,$bf,$bf,$bf,$bf,$aa
    !byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$aa
    !byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$aa

; For your convenience: boolean to record whether this is hole 9 or 18.
; Used by draw routines.
sc_v_is_rightmost_cell  !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
.TEAM_ITER = TREES_LO
.SLOT_COLOR  = TREES_HI
.NUM_TEAMS = 2

sc_s_prepare_colors
    ldx #0
.loop_top
    stx .TEAM_ITER

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
    cpy #33
    bne -
    ; End of row.
    dex
    beq .next_team
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

.next_team
    ldx .TEAM_ITER
    inx
    cpx #.NUM_TEAMS
    bne .loop_top

    rts
; end sub sc_s_prepare_colors
} ; !zone

; **************************************************

!zone {
sc_s_clear_digits_buffer
    ldx #((8*8)-1)
    lda #$ff
-
    sta sc_v_digits_buffer,x
    dex
    bpl -
    rts
; end sub sc_s_clear_digits_buffer
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
    lda sc_v_2x3_box,y
    sta (MATHS2),y
    lda sc_v_2x3_box+(3*8),y
    sta (MATHS4),y
    iny
    cpy #(3*8)
    bne -

    rts
; end sub sc_s_write_buffer_to_bitmap
} ; !zone

; **************************************************

!zone {
.SLOT_ITER  = TREES_LO
.HOLE_ITER  = TREES_HI
.HOLE_END   = BITMAP_LO
.SCORES_ON  = BITMAP_HI
; NOTE: .HOLE_COUNT counts from 0 to 17, regardless of the player #.
.HOLE_COUNT         = EDGES_LO
.CURRENT_TEAM       = EDGES_HI
.CELL_COUNT         = VM_LO
.TOTAL_CELL_COUNT   = VM_HI
.order  !fill   2

sc_s_draw_match_play
    ; Establish order.  Use 0,1 as default.
    ldx #0
    stx .order
    inx
    stx .order+1
    lda sc_v_round_scores   
    cmp sc_v_round_scores+1
    +bge_s +
    inc .order
    dec .order+1
    
+
    lda #1
    sta .SCORES_ON
    lda #0
    sta .HOLE_COUNT
    sta .CELL_COUNT
    sta .TOTAL_CELL_COUNT

    ldx #0
.team_loop
    stx .SLOT_ITER
    lda .order,x
    sta .CURRENT_TEAM
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
    ; .HOLE_COUNT keeps track of hole, whether or not we have a score for
    ; it yet...
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
    jmp .draw_border

.valid_slot
    lda .SCORES_ON
    +branch_if_false .draw_border
    ; Get score for current hole.
    ; It will be either 0, 1 or 1/2.  If 1/2, C flag will be set after 
    ; shifting right.  If 0 or 1, correct digit will be in accumulator
    ; after that shift right as well.
    ldx .HOLE_ITER
    lda sc_v_team_scores,x
    lsr
    bcs .one_half
    ; So 0 or 1.
    clc
    adc #font_c_ASCII_0 
    tax
    +skip_2_bytes 

.one_half
    ldx #font_c_ONE_HALF 
    jsr sc_s_write_char_to_buffer2

.draw_border
    jsr sc_s_draw_cell_border2

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
    ; TODO: clear out any contents of sc_v_2x3_box at this point!
    jsr sc_s_clear_box_contents
+
    jmp .hole_loop

.running_totals
    ldy .CURRENT_TEAM
    lda .SLOT_ITER
    sta P0
    jsr sc_s_draw_totals_match_play
    ldx .TOTAL_CELL_COUNT
    inx
    inx
    stx .TOTAL_CELL_COUNT

    ldx .CURRENT_TEAM
    ldy .SLOT_ITER
    lda sc_l_NAME_ROW_CHARS,y
    sta P0
    lda #sc_c_NAME_COL_CHARS
    sta P1
    jsr sc_s_draw_name_and_score

    ldx .SLOT_ITER
    inx
    cpx #2  ;shared_v_num_players
    beq .end
    ; RESET THESE FOR NEXT PLAYER!!!
    ldy #1
    sty .SCORES_ON
    dey
    sty .HOLE_COUNT

    jmp .team_loop

.end
    rts
; end sub sc_s_draw_match_play
} ; !zone

; **************************************************

; INPUTS:   Y = current team, P0 = slot #
!zone {
.ascii_buffer   !fill   3
.CURRENT_TEAM   = COLORS_LO
.CHAR_ITER      = COLORS_HI
.ONE_PAST_END   = LINE_X0_LO 
.NUM_CHARS      = LINE_Y0_LO 
; NOTE: these are offsets into sc_v_digits_buffer.
.UPPER_OFFSETS  !byte   28,20,12
.LOWER_OFFSETS  !byte   56,48,40
.UPPER          = LINE_X1_LO 
.LOWER          = LINE_Y1_LO 
; Front 9 or back 9?
.HALF_ITER      = WS_X_LO
; Offset for 'sc_v_front9_scores'.
.SRC_OFFSET     = WS_X_HI
.SLOT           = WS_Z_LO

sc_s_draw_totals_match_play
    sty .CURRENT_TEAM
    lda #0
    sta .HALF_ITER
    lda P0
    sta .SLOT

.loop_outer
    sty .SRC_OFFSET

    ; How many chars make up the total score?  Will be in the range [1,3].
    ldx #0
    lda sc_v_front9_scores,y
    lsr
    bcc +
    ; So the total score includes '1/2'.
    pha
    lda #font_c_ONE_HALF 
    sta .ascii_buffer
    inx
    pla
+
    stx .CHAR_ITER

    ; Get decimal digits for whatever's in the accumulator.
    cmp #0
    bne +
    ; If score is 1/2, we'll ignore this zero.  Otherwise, that's the team's
    ; score!
    ldy .CHAR_ITER
    bne .have_ascii
+
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; X holds number of digits - will be in range [1,2].
    stx .ONE_PAST_END

    ldx #0
    ldy .CHAR_ITER
-
    lda utils_v_dec_digits,x
    clc
    adc #font_c_ASCII_0 
    sta .ascii_buffer,y
    iny
    inx
    cpx .ONE_PAST_END
    bne -

.have_ascii
    sty .NUM_CHARS

    jsr sc_s_clear_digits_buffer

    ; Make sure we're actually playing this half of the course!
    lda shared_v_holes
    beq .valid
    cmp #shared_c_PLAYING_FRONT_9
    beq .front_9_only
    ; So back 9 only...
    lda .HALF_ITER
    bne .valid
    beq .must_ghost
.front_9_only
    lda .HALF_ITER
    beq .valid
.must_ghost
    jsr sc_s_ghost_digits_buffer2
    jmp .draw_border

.valid
    ; If back 9 total but our current hole is <=10, just draw an empty box
    ; and be done.  NOTE: 'round_v_current_hole' starts counting from 0.
    lda .HALF_ITER
    beq +
    lda round_v_current_hole
    cmp #10
;    bcs +
    bcc .draw_border

+
    ; We have upto three 'ascii' codes in .ascii_buffer.
    ; Now need to look up the font data for them and write into the digits
    ; buffer.
    ldx #0
.loop_top
    stx .CHAR_ITER

    ; Source address into MATHS0-MATHS1.
    lda .ascii_buffer,x
    tay
    lda font_l_CHAR_DATA_LO,y
    sta MATHS0
    lda font_l_CHAR_DATA_HI,y
    sta MATHS1
    ; Record upper and lower destination offsets (into sc_v_digits_buffer).
    lda .LOWER_OFFSETS,x
    sta .LOWER
    lda .UPPER_OFFSETS,x
    sta .UPPER
    ; And upper offset straight into X.
    tax

    ; Copy char data into the buffer.
    ; Upper half.
    ldy #0
-
    lda (MATHS0),y
    sta sc_v_digits_buffer,x
    inx
    iny
    cpy #4
    bne -
    ; Lower half.
    ldx .CHAR_ITER
    lda .LOWER_OFFSETS,x
    tax
-
    lda (MATHS0),y
    sta sc_v_digits_buffer,x
    inx
    iny
    cpy #8
    bne -

    ; Any more chars to go?
    ldx .CHAR_ITER
    inx
    cpx .NUM_CHARS
    bne .loop_top

    ; X holds value for '.NUM_CHARS'.
    jsr sc_s_shift_buffer
.draw_border
    jsr sc_s_draw_totals_border

    ; Offset into destination addresses (into X).  Two bytes apart; add 1 to
    ; get destination for 'back 9'.
    lda .SLOT
    asl
    clc
    adc .HALF_ITER
    tax

    lda sc_l_CELL_TOTALS_ADDR_LO,x
    sta MATHS2
    lda sc_l_CELL_TOTALS_ADDR_HI,x
    sta MATHS3
    jsr sc_s_write_buffer_to_bitmap2

    ; If this was front 9, we still have the back 9 to do!
    ldx .HALF_ITER
    inx
    cpx #2
    beq .end
    stx .HALF_ITER
    ; NOTE: running totals for front and back 9 are 4 bytes apart!
    lda .SRC_OFFSET
    clc
    adc #4
    tay
    jmp .loop_outer

.end
    rts
; end sub sc_s_draw_totals_match_play
} ; !zone

; **************************************************

; INPUTS:   X = num chars (in range [1,3])
!zone {
; NOTE: these are actually 'double' shifts!
.NUM_SHIFTS !byte   0,6,4,2

sc_s_shift_buffer
    lda .NUM_SHIFTS,x
    tay
    
    ; Keep decrementing Y until it reaches zero.

.loop_outer
    ldx #0

.loop_inner
    asl sc_v_digits_buffer+28,x
    rol sc_v_digits_buffer+20,x
    rol sc_v_digits_buffer+12,x
    rol sc_v_digits_buffer+4,x
    asl sc_v_digits_buffer+28,x
    rol sc_v_digits_buffer+20,x
    rol sc_v_digits_buffer+12,x
    rol sc_v_digits_buffer+4,x

    asl sc_v_digits_buffer+56,x
    rol sc_v_digits_buffer+48,x
    rol sc_v_digits_buffer+40,x
    rol sc_v_digits_buffer+32,x
    asl sc_v_digits_buffer+56,x
    rol sc_v_digits_buffer+48,x
    rol sc_v_digits_buffer+40,x
    rol sc_v_digits_buffer+32,x

    ; Repair the background color for bit pair 'pushed in' at right.
    lda sc_v_digits_buffer+28,x
    ora #%00000011
    sta sc_v_digits_buffer+28,x
    lda sc_v_digits_buffer+56,x
    ora #%00000011
    sta sc_v_digits_buffer+56,x

    inx
    cpx #4
    bne .loop_inner

    dey
    bne .loop_outer

    rts
; end sub sc_s_shift_buffer
} ; !zone

; **************************************************

; INPUTS:   MATHS2-3 = destination of top-left byte.
!zone {
sc_s_write_buffer_to_bitmap2
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
    lda sc_v_digits_buffer+(4*8),y
    sta (MATHS4),y
    iny
    cpy #(4*8)
    bne -

    rts
; end sub sc_s_write_buffer_to_bitmap2
} ; !zone

; **************************************************

!zone {
sc_s_draw_totals_border
    lda #$aa
    sta sc_v_digits_buffer
    sta sc_v_digits_buffer+8
    sta sc_v_digits_buffer+16
    sta sc_v_digits_buffer+24
    
    sta sc_v_digits_buffer+(4*8)+7
    sta sc_v_digits_buffer+(5*8)+7
    sta sc_v_digits_buffer+(6*8)+7
    sta sc_v_digits_buffer+(7*8)+7

    ldx #6
-
    lda sc_v_digits_buffer+1,x
    and #$3f
    ora #$80
    sta sc_v_digits_buffer+1,x
    lda sc_v_digits_buffer+(4*8),x
    and #$3f
    ora #$80
    sta sc_v_digits_buffer+(4*8),x
    lda sc_v_digits_buffer+(3*8)+1,x
    and #$fc
    ora #$02
    sta sc_v_digits_buffer+(3*8)+1,x
    lda sc_v_digits_buffer+(7*8),x
    and #$fc
    ora #$02
    sta sc_v_digits_buffer+(7*8),x
    dex
    bpl -
    
    rts
; end sub sc_s_draw_totals_border
} ; !zone

; **************************************************

; INPUTS:   X = ascii code
!zone {
sc_s_write_char_to_buffer2
    ; Address of char data into MATHS2-MATHS3.
    lda font_l_CHAR_DATA_LO,x 
    sta MATHS2
    lda font_l_CHAR_DATA_HI,x 
    sta MATHS3

    ; Now copy to buffer.
    ldy #0
    ; Upper half.
    ldx #12
-
    lda (MATHS2),y
    sta sc_v_2x3_box,x
    inx
    iny
    cpy #4
    bne -
    ; Lower half.
    ldx #32
-
    lda (MATHS2),y
    sta sc_v_2x3_box,x
    inx
    iny
    cpy #8
    bne -

    rts
; end sub sc_s_write_char_to_buffer2
} ; !zone

; **************************************************

!zone {
sc_s_clear_box_contents
    lda #$ff
    sta sc_v_2x3_box+12
    sta sc_v_2x3_box+13
    sta sc_v_2x3_box+14
    sta sc_v_2x3_box+15
    sta sc_v_2x3_box+32
    sta sc_v_2x3_box+33
    sta sc_v_2x3_box+34
    sta sc_v_2x3_box+35
    rts
; end sub sc_s_clear_box_contents
} ; !zone

; **************************************************

!zone {
sc_s_draw_cell_border2
    ; NOTE: $ff is the 'open' pattern; $fe 'closed'.
    lda #$ff
    ldx sc_v_is_rightmost_cell  
    beq +
    lda #$fe
+

    ldx #1
-
    sta sc_v_2x3_box+(2*8),x
    inx
    cpx #8
    bne -

    ldx #0
-
    sta sc_v_2x3_box+(5*8),x
    inx
    cpx #7
    bne -

    rts
; end sub sc_s_draw_cell_border2
} ; !zone

; **************************************************

!zone {
sc_s_ghost_digits_buffer
    lda #$bb
    sta sc_v_2x3_box+12
    sta sc_v_2x3_box+13
    sta sc_v_2x3_box+14
    sta sc_v_2x3_box+33
    sta sc_v_2x3_box+34
    sta sc_v_2x3_box+35
    lda #$ef
    sta sc_v_2x3_box+15
    sta sc_v_2x3_box+32
    rts

; end sub sc_s_ghost_digits_buffer
} ; !zone

; **************************************************

!zone {
sc_s_ghost_digits_buffer2
;    lda #$bb
;    sta sc_v_digits_buffer+20
;    sta sc_v_digits_buffer+21
;    sta sc_v_digits_buffer+22
;    sta sc_v_digits_buffer+49
;    sta sc_v_digits_buffer+50
;    sta sc_v_digits_buffer+51
;    lda #$ef
;    sta sc_v_digits_buffer+23
;    sta sc_v_digits_buffer+48
    lda #$fb
    sta sc_v_digits_buffer+12
    sta sc_v_digits_buffer+13
    sta sc_v_digits_buffer+14
    sta sc_v_digits_buffer+41
    sta sc_v_digits_buffer+42
    sta sc_v_digits_buffer+43
    lda #$fe
    sta sc_v_digits_buffer+15
    sta sc_v_digits_buffer+40
    lda #$bf
    sta sc_v_digits_buffer+20
    sta sc_v_digits_buffer+21
    sta sc_v_digits_buffer+22
    sta sc_v_digits_buffer+49
    sta sc_v_digits_buffer+50
    sta sc_v_digits_buffer+51

    rts
; end sub sc_s_ghost_digits_buffer2
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

sctblmp_c_SIZE = *-sctblmp_c_BEGIN


