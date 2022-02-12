; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


stats_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; 'yds','ft'
stats_l_UNITS_TEXT    !raw  "yds",0,"ft",0
; For 'yds' and 'ft' respectively.  Can use 'round_v_must_putt' as an index into
; this table.
stats_l_UNITS_TEXT_OFFSETS  !byte   0,4

stats_l_PAR_STR   !scr    "1-2-3-4-5",0
; I.e. one-past-the-end...
; NOTE: lowest index will be 3.
stats_l_PAR_STR_ENDS = *-3
    !byte   5,7,9
; NOTE: measured from start of row (column 0).
stats_l_PAR_HIGHLIGHT_OFFSETS
    !byte   5,7,9,11,13
stats_l_STRING_MEM_FINAL_SHOT_OFFSET = *-3
    !byte   9,11,13

; Tables for 'balls holed' markers (cf. match play).
stats_l_BALLS_HOLED_PATTERNS
    !byte   $00,$00,$00, $08,$00,$00, $08,$00,$08
    !byte   $00,$00,$00, $80,$00,$00, $80,$00,$80
stats_l_BALLS_HOLED_TEAM_OFFSETS    !byte   0,9
stats_l_BALLS_HOLED_TOTAL_OFFSETS   !byte   0,3,6
stats_l_BALLS_HOLED_MARKERS_ADDR_LO
    !byte   <gfxs_c_BITMAP_BASE+(73*8),<gfxs_c_BITMAP_BASE+(79*8) 
stats_l_BALLS_HOLED_MARKERS_ADDR_HI
    !byte   >gfxs_c_BITMAP_BASE+(73*8),>gfxs_c_BITMAP_BASE+(79*8) 


; *****************
; *** VARIABLES ***
; *****************
stats_v_string_mem  !fill   20,0
; When match play, clear these two bytes at the start of each hole.
stats_v_balls_holed !fill   2


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
.HAVE_NON_ZERO = CAMERA0

stats_s_draw_distance
    ; String will never be > 6 chars, so clear out 7 (?!) and always use this
    ; as length (for call to font_s_draw_text).
    lda #font_c_ASCII_SPACE 
    ldx #0
-
    sta stats_v_string_mem,x
    inx
    cpx #7
    bne -

    ldx #10 ; how many?
-
    lda #BLACK
    sta gfxs_c_DISPLAY_BASE+3*40+1,x
    lda #CYAN
    sta COLOR_RAM+3*40+1,x
    dex
    bpl -

    ; Divide distance (measured in pixels) by correct value to get distance
    ; in either yards or feet (for when putting).
    ldx round_v_current_player
    lda players_v_distance_lo,x
    sta P0
    lda players_v_distance_hi,x
    sta P1

    lda round_v_must_putt
    +branch_if_true +
    lda #hole_c_PIXELS_PER_YARD
    bne ++
+
    lda #hole_c_PIXELS_PER_FOOT
++
    sta P2
    lda #0
    sta P3
    jsr maths_div16
    ; Result of division is in P0-P1.  Also where input to following 
    ; routine should go.
    ; Store this result in case other modules wish to use it (e.g. for
    ; putting assistant).
    lda P0
    sta round_v_current_distance_lo
    lda P1
    sta round_v_current_distance_hi
    jsr utils_s_16bit_hex_to_dec

    ; If putting and very close to the hole, this result may be 0.  In which
    ; case, show the text '<1'.
    lda utils_v_dec_digits
    ora utils_v_dec_digits+1
    ora utils_v_dec_digits+2
    bne +

    ; Distance is less than 1 foot!
    ; TODO: draw '<1 ft'...
    lda #SCR_CODE_LT
    sta stats_v_string_mem
    lda #SCR_CODE_1
    sta stats_v_string_mem+1
    ldx #2
    jmp .call_draw_routine

+
    ; X holds column offset.  
    ; Y is index into actual digits (- decrement this!).
    ldx #0
    stx .HAVE_NON_ZERO
    ldy #2

.loop_top
    lda utils_v_dec_digits,y
    bne .draw_digit
    ; Draw this '0' only if we've already drawn something.
    lda .HAVE_NON_ZERO
    +branch_if_true .draw_zero
    jmp .next

.draw_digit
    sta .HAVE_NON_ZERO
    clc
    adc #SCR_CODE_0
    jmp +
.draw_zero
    lda #SCR_CODE_0
+
    sta stats_v_string_mem,x
    inx
.next
    dey
    bpl .loop_top

.call_draw_routine
    ; Save this for later.
    stx CAMERA1

    ; Add the text 'yds' or 'ft', as appropriate.
    ldy round_v_must_putt
    lda stats_l_UNITS_TEXT_OFFSETS,y
    tay
-
    lda stats_l_UNITS_TEXT,y
    beq .done
    sta stats_v_string_mem,x
    inx
    iny
    bne -

.done
    lda #<stats_v_string_mem
    sta P0
    lda #>stats_v_string_mem
    sta P1
    lda #24 ; row
    sta P2
    lda #4  ; column
    sta P3
    lda #7
    sta P4

    jsr font_s_draw_text
    rts
; end sub stats_s_draw_distance
} ; !zone

; **************************************************

!zone {
stats_s_refresh_hole
    ; Clear string memory to begin with.
    ldx #0
    ; FIXME: code 40 = temporary space.
    lda #font_c_ASCII_SPACE ;40 
-
    sta stats_v_string_mem,x
    inx
    cpx #15
    bne -
;    ; End-of-string marker in position #15.
;    lda #$ff
;    sta stats_string_mem,x

    ; Prepare colors.
    ; FIXME: or use a base color table?!
    ldx #14
-
    lda #GREY1
    sta gfxs_c_DISPLAY_BASE+80,x
    lda #CYAN
    sta COLOR_RAM+80,x
    dex
    bpl -

    ; Hole number - either 1 or 2 digits.
    ldx round_v_current_hole
    inx
    stx P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; Max 2 digits.  X will hold either 1 or 2.  So decrement X and use that
    ; as index into digits - put this in slot 0 of string mem.  If X is now
    ; 0 we're done (there was only one digit).  Otherwise, put 0th digit into
    ; slot 1.
    dex
    lda utils_v_dec_digits,x
    clc
    adc #SCR_CODE_0
    sta stats_v_string_mem+1
    cpx #0
    beq +
    lda utils_v_dec_digits
    clc
    adc #SCR_CODE_0
    sta stats_v_string_mem+2
+

    ; Par & current shot.
    ldx hole_v_par
    lda stats_l_PAR_STR_ENDS,x
    tax
    dex
-
    lda stats_l_PAR_STR,x
    sta stats_v_string_mem+5,x
    dex
    bpl -

    ; Write current shot to string if it's > par.
    ldx round_v_current_player
    lda players_v_current_shots,x
    cmp hole_v_par
    bcc .draw
    ; Add 1 so we have the current shot counting from 1.
    +utils_m_inca
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; Result may be one or two digits.  First look up the offset where we're
    ; going to write the number.  (From beginning of 'stats_mem_string'.)
    ldy hole_v_par
    lda stats_l_STRING_MEM_FINAL_SHOT_OFFSET,y
    tay
    
    ; Decrement X so it's either 0 (1 digit) or 1 (2 digits).
    dex
    ; Write this digit to the leftmost slot.
    lda utils_v_dec_digits,x
    clc
    adc #SCR_CODE_0
    sta stats_v_string_mem,y
    ; If X is now 0, we're done.  Otherwise, write the 0th digit into the
    ; following slot.
    cpx #0
    beq .draw
    lda utils_v_dec_digits
    clc
    adc #SCR_CODE_0
    sta stats_v_string_mem+1,y

.draw
    ; Draw this much.
    lda #<stats_v_string_mem
    sta P0
    lda #>stats_v_string_mem
    sta P1
    lda #16
    sta P2
    lda #0
    sta P3
    lda #15
    sta P4
    jsr font_s_draw_text

    ; Highlight the current shot.
    ldx round_v_current_player
    lda players_v_current_shots,x
    cmp hole_v_par
    bcc .below_or_even
    ldx hole_v_par
    dex
    bne .lookup_slot
.below_or_even
    tax
.lookup_slot
    lda stats_l_PAR_HIGHLIGHT_OFFSETS,x
    tax
    lda #WHITE
    sta gfxs_c_DISPLAY_BASE+80,x
    ; Highlight next char as well if in final slot (- may be two digits).
    bcc .end
    sta gfxs_c_DISPLAY_BASE+81,x

.end
    rts
    
; end sub stats_s_refresh_hole
} ; !zone

; **************************************************

; INPUTS:   string in stats_string_mem, terminated with 0.
;!zone {
;.msg    !byte   3,8,21,8,18,8,14,13,0,11,255
;stats_draw_message
;    lda #<.msg 
;    sta P0
;    lda #>.msg 
;    sta P1
;    lda #(24*8)
;    sta P2
;    lda #4
;    sta P3
;    clc
;;    jsr txtren_s_draw
;    rts
;
;
;    ; Find length of string. 
;    ldx #0
;-
;    lda stats_v_string_mem,x
;    beq +
;    inx
;    jmp -
;+
;    stx P6
;
;    lda #24*8
;    sta P2
;    lda #4
;    sta P3
;    lda #<stats_v_string_mem
;    sta P0
;    lda #>stats_v_string_mem
;    sta P1
;
;    clc
;    jsr dp_s_draw_string
;
;    rts
;; end sub stats_draw_message
;} ; !zone

; **************************************************

; Top three rows (=120 cells) of hi-res screen should have the same b/g
; color as the sky.  This information is stored in the lower nybble of
; screen RAM.
!zone {
stats_s_clear
    lda #CYAN
    ldx #(2*40)-1
-
    sta gfxs_c_DISPLAY_BASE,x
    sta gfxs_c_DISPLAY_BASE+(2*40),x
    dex
    bpl -
    rts
; end sub stats_s_clear
} ; !zone

; **************************************************

!zone {
.holes_to_play_col  !byte   0

stats_s_draw_name
    jsr stats_s_clear_text_row1

    lda shared_v_is_match_play
    pha
    +branch_if_false +

    ldx round_v_current_player
    lda shared_v_team_membership,x
    tax
    bpl ++

+
    ldx round_v_current_player

++
    lda #1
    sta P0
    sta P1
    jsr sc_s_draw_name_and_score
    stx .holes_to_play_col

    pla
    beq .end
    ; It's match play, so we must ghost out the team member who isn't currently
    ; playing (if there are four players).
    ; Offset is relative to team name.  Add length to this to get index
    ; for 'one-past-end'.
    lda shared_v_num_players
    cmp #2
    beq .draw_holes_to_play

    ; We want the team member who ISN'T playing!  If l.s. bit clear, add 1 
    ; (0 -> 1, or 2 -> 3); otherwise subtract 1 (1 -> 0, or 3 -> 2).
    lda round_v_current_player
    tax
    lsr
    bcc .add_one
    ; So subtract 1.
    dex
    +skip_1_byte 
.add_one
    inx

    lda shared_v_team_names_offsets_begin,x   
    tay
    clc
    adc shared_v_player_name_lens,x
    sta MATHS0
    lda #LIGHT_BLUE
-
    sta gfxs_c_DISPLAY_BASE+41,y
    iny
    cpy MATHS0
    bne -

.draw_holes_to_play
    lda .holes_to_play_col
    jsr stats_s_draw_holes_to_play
    
.end
    rts

; end sub stats_s_draw_name
} ; !zone

; **************************************************

!zone {
stats_s_clear_text_row1
    ldx #39
    lda #CYAN
-
    sta gfxs_c_DISPLAY_BASE+40,x
    sta COLOR_RAM+40,x
    dex
    bpl -
    rts
; end sub stats_s_clear_text_row1
} ; !zone

; **************************************************

; NOTE: this routine is for match play only!
!zone {
.buffer     !fill   5
.offsets    !byte   0,4

stats_s_draw_current_shots
    lda shared_v_is_match_play
    +branch_if_true +
;    cmp #shared_c_MATCH_PLAY
;    beq +
    rts ; EXIT POINT.

+
stats_s_draw_current_shots_no_check

    ; Team0.
    lda sc_v_round_scores+2
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    lda utils_v_dec_digits+1
    sta .buffer
    lda utils_v_dec_digits
    sta .buffer+1

    ; Team1.
    lda sc_v_round_scores+3
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    lda utils_v_dec_digits+1
    sta .buffer+3
    lda utils_v_dec_digits
    sta .buffer+4

    lda #SCR_CODE_COLON-SCR_CODE_0
    sta .buffer+2

    ; Add 48 to everything to make them PETSCII codes.
    ldx #4
-
    lda .buffer,x
    clc
    adc #SCR_CODE_0
    sta .buffer,x
    dex
    bpl -

    ; And draw buffer to the screen.
    lda #<.buffer
    sta P0
    lda #>.buffer
    sta P1
    lda #8
    sta P2
    lda #34*4
    sta P3
    lda #5
    sta P4
    jsr font_s_draw_text

    ; FIXME: just set color to grey for now.
    ldx #6
    lda #GREY1
-
    sta gfxs_c_DISPLAY_BASE+40+33,x
    dex
    bpl -

    ; Highlight current team with white.
    ldy round_v_current_player
    ldx shared_v_team_membership,y
    lda .offsets,x
    tax
    lda #WHITE
    sta gfxs_c_DISPLAY_BASE+40+33,x
    sta gfxs_c_DISPLAY_BASE+40+34,x
    sta gfxs_c_DISPLAY_BASE+40+35,x

    rts
; end sub stats_s_draw_current_shots
} ; !zone

; **************************************************

; INPUTS:   Y = current player
!zone {
stats_s_inc_current_shots
    lda shared_v_scoring
    cmp #shared_c_MATCH_PLAY
    beq +
    rts ; EXIT POINT.

+
    ldx shared_v_team_membership,y
    inc sc_v_round_scores+2,x
    jsr stats_s_draw_current_shots_no_check

    rts
; end sub stats_s_inc_current_shots
} ; !zone

; **************************************************

; INPUTS:   A = column (chars)
!zone {
; NOTE: use value of shared_v_holes to index this table.  We will subtract
; from this the current hole to determine how many holes are remaining.
; (0 = 18, 1 = front 9, 2 = back 9)
.minuends   !byte   18,9,18

stats_s_draw_holes_to_play
    pha
    asl
    asl
    sta P3

    ; Put a '/' here.
    lda #SCR_CODE_SLASH  
    sta sc_v_score_str_buffer

    ; FIXME: we may only be playing nine holes!
    ; Calculate number of holes still to play, including this one
    ; (- round_v_current_hole starts at 0).
    ldx shared_v_holes
    lda .minuends,x
;    lda #18
    sec
    sbc round_v_current_hole
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec 
    ; Number of digits is in X - it'll be either 1 or 2.
    ; If it's 2 digits, leftmost will always be '1'.  In any case, we can
    ; get the ASCII code for the least significant digit (and push it onto
    ; the stack).
    lda utils_v_dec_digits
    clc
    adc #SCR_CODE_0

    cpx #1
    bne .two_digits
    ; So one digit.
    sta sc_v_score_str_buffer+1
    beq +

.two_digits
    sta sc_v_score_str_buffer+2
    lda #SCR_CODE_1
    sta sc_v_score_str_buffer+1
     
+
    lda #<sc_v_score_str_buffer
    sta P0
    lda #>sc_v_score_str_buffer
    sta P1
    lda #8
    sta P2
    ; P3 (bitmap column) already set.
    lda #3
    sta P4
    jsr font_s_draw_text

    ; Column (chars) on top of stack.
    pla
    tax
    lda #GREY1
    sta gfxs_c_DISPLAY_BASE+40,x
    sta gfxs_c_DISPLAY_BASE+41,x
    sta gfxs_c_DISPLAY_BASE+42,x
    
    rts
; end sub stats_s_draw_holes_to_play
} ; !zone

; **************************************************

!zone {
.TEAM_ITER = MATHS2

stats_s_refresh_balls_holed_markers
    lda shared_v_is_match_play
    +branch_if_true +
    rts ; EXIT POINT.

+
    ldx #0
.loop_top
    stx .TEAM_ITER

    lda stats_v_balls_holed,x
    tay
    lda stats_l_BALLS_HOLED_TOTAL_OFFSETS,y
    clc
    adc stats_l_BALLS_HOLED_TEAM_OFFSETS,x
    pha

    lda stats_l_BALLS_HOLED_MARKERS_ADDR_LO,x
    sta MATHS0
    lda stats_l_BALLS_HOLED_MARKERS_ADDR_HI,x
    sta MATHS1

    ; X keeps track of source, Y of destination.
    ldy #0
    pla
    tax
-
    lda stats_l_BALLS_HOLED_PATTERNS,x
    sta (MATHS0),y
    inx
    iny
    cpy #3
    bne -

    ldx .TEAM_ITER
    inx
    cpx #2
    bne .loop_top

    rts
; end sub stats_s_refresh_balls_holed_markers
} ; !zone

; **************************************************

; NOTE: only relevant to match play, but won't matter if called during
; stroke play.
!zone {
stats_s_clear_balls_holed
    lda #0
    sta stats_v_balls_holed
    sta stats_v_balls_holed+1
    rts
; end sub stats_s_clear_balls_holed
} ; !zone

; **************************************************

; INPUTS:   X = current player.
!zone {
stats_s_inc_balls_holed
    lda shared_v_is_match_play
    +branch_if_true +
    rts ; EXIT POINT.

+
    lda shared_v_team_membership,x
    tax
    inc stats_v_balls_holed,x

    jsr stats_s_refresh_balls_holed_markers

    rts
; end sub stats_s_inc_balls_holed
} ; !zone

; **************************************************

!zone {
stats_s_draw_all
    jsr stats_s_clear
    jsr stats_s_draw_name
    jsr stats_s_refresh_hole
    jsr stats_s_draw_distance
    jsr stats_s_draw_current_shots
    jsr stats_s_refresh_balls_holed_markers
    rts
; end sub stats_s_draw_all
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

stats_c_SIZE = *-stats_c_BEGIN

