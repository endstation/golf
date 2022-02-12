; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


sc_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************


; *****************
; *** VARIABLES ***
; *****************
; FIXME: for time being, everything is par 3!
; Pars are set as the holes are loaded in.
; FIXME: hard-coded numbers!
sc_v_pars           !fill   18
sc_v_scores         !fill   18*players_c_MAX_N
sc_v_team_scores    !fill   18*2
; NOTE: these are SIGNED bytes...
sc_v_round_scores   !fill   players_c_MAX_N
sc_v_front9_scores  !fill   players_c_MAX_N
sc_v_back9_scores   !fill   players_c_MAX_N
sc_v_scores_end

; Temporary buffer for score string and color.
; NOTE: extra 3 bytes for buffer is when match play - we need to show also
; how many holes are still to play (e.g. '/11').
sc_v_score_str_buffer   !fill   3;+3
sc_v_score_str_color    !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
sc_s_reset_all
    ; TODO: read pars from file (or somewhere)...
    ldx #sc_v_scores_end-sc_v_scores-1
    lda #0
-   sta sc_v_scores,x
    dex
    cpx #$ff
    bne -
    rts
; end sub sc_s_reset_all
} ; !zone

; **************************************************

; FIXME: this won't work for match play!
; Write score for current player and current hole.
; INPUTS:   X = current player.
; NOTE: preserves value of X.
!zone {
sc_s_write
    ; Find offset into table and put it in Y.
    ; Multiply current player by 18 (*16 + *2) then add current hole.
    txa
    pha

    asl
    asl
    asl
    asl
    sta MATHS0
    txa
    asl
    clc
    adc MATHS0
    adc round_v_current_hole
    tay

    ; Load in the number of shots.
    lda players_v_current_shots,x
    ; And store it in the slot we just found.
    sta sc_v_scores,y  
    ; Temp store here for updating front/back 9 totals...
    sta MATHS0
    
    ; Now update this player's score for the whole round.
    ldy round_v_current_hole
    sec
    sbc sc_v_pars,y
    clc
    adc sc_v_round_scores,x
    sta sc_v_round_scores,x

    ; Update values for front/back 9 totals.
    ; Are we at the front or back 9?  Subtract 9 from round_v_current_hole - if
    ; result is negative then we're at the front 9.
    lda round_v_current_hole
    sec
    sbc #9
    bmi +
    ; Back 9 so add 4 to player index.
    inx
    inx
    inx
    inx
+
    lda sc_v_front9_scores,x
    clc
    adc MATHS0
    sta sc_v_front9_scores,x

    ; Restore original value of X.
    pla
    tax

    rts
; end sub sc_s_write
} ; !zone

; **************************************************

; INPUTS:   X = current player
!zone {
sc_s_prepare_score_str
    ldy #5
    lda #font_c_ASCII_SPACE
-
    sta sc_v_score_str_buffer,y
    dey
    bpl -
;    sta sc_v_score_str_buffer+1
;    sta sc_v_score_str_buffer+2

    lda sc_v_round_scores,x
    beq .even_par
    bmi .below_par

    ; So above par...
    pha
    lda #GREEN
    sta sc_v_score_str_color
    lda #font_c_ASCII_PLUS
    sta sc_v_score_str_buffer
    pla
    cmp #10
    bcc .single_digit

.double_digits
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; Most sig. digit first:
    lda utils_v_dec_digits+1
    clc
    adc #font_c_ASCII_0
    sta sc_v_score_str_buffer+1
    lda utils_v_dec_digits
    clc
    adc #font_c_ASCII_0
    sta sc_v_score_str_buffer+2
    rts ; EXIT POINT.

.single_digit
    clc
    adc #font_c_ASCII_0
    sta sc_v_score_str_buffer+1
    rts ; EXIT POINT.

.below_par
    pha
    lda #RED
    sta sc_v_score_str_color
    lda #font_c_ASCII_MINUS
    sta sc_v_score_str_buffer
    pla
    +nega
    cmp #10
    bcc .single_digit
    jmp .double_digits

.even_par
    lda #WHITE
    sta sc_v_score_str_color
    ; An overall score of 0 should be represented differently for stroke and
    ; match play.  For stroke play, display an 'E'; for match play, '='.
    ldx shared_v_scoring    
    lda shared_l_CHARS_FOR_SCORE_ZERO,x   
    sta sc_v_score_str_buffer

    rts
; end sub sc_s_prepare_score_str
} ; !zone

; **************************************************

; INPUTS:   X = player/team #, P0 = row (chars), P1 = column (chars)
; OUTPUTS:  X = column (chars) directly after score
!zone {
.PLAYER_NUM = MATHS0
.char_row   !byte   0
.char_col   !byte   0
.bitmap_row !byte   0
.bitmap_col !byte   0
.name_len   !byte   0
.max_len    !byte   0

sc_s_draw_name_and_score
    stx .PLAYER_NUM

    ; For drawing the actual text, need bitmap row/col coordinates.
    lda P0
    sta .char_row
    asl
    asl
    asl
    sta .bitmap_row
    lda P1
    sta .char_col
    asl
    asl
    sta .bitmap_col

    ; First prepare the score as a string (and a color).
    ; NOTE: X still holds player number.
    jsr sc_s_prepare_score_str

    lda shared_v_is_match_play
    +branch_if_true .match_play

    lda #shared_c_MAX_NAME_LEN
    sta .max_len
    ; Draw the name.  Must get pointer to data (ASCII string).
    ldx .PLAYER_NUM
    lda shared_v_player_name_lens,x
    sta .name_len
    lda shared_v_player_name_indices,x
    tax
    lda shared_l_NAME_ADDR_LO,x
    sta P0
    lda shared_l_NAME_ADDR_HI,x
    sta P1
    jmp .draw_text

.match_play
    ; Must set maximum length for team name (= .max_len).
    ; Team name length into .name_len.
    ; Address of team name text into P0-P1.
    lda #shared_c_MAX_TEAM_NAME_LEN
    sta .max_len
    ldx .PLAYER_NUM
    lda shared_v_team_lens,x
    sta .name_len
    lda shared_l_TEAM_NAME_ADDR_LO,x
    sta P0
    lda shared_l_TEAM_NAME_ADDR_HI,x
    sta P1

.draw_text
    lda .bitmap_row
    sta P2
    lda .bitmap_col
    sta P3

    ; Give text length as max for name so that any chars from previous name
    ; are erased - this may happen during teeing off, for example.
    lda .max_len
    sta P4
    jsr font_s_draw_text
    ; And now the colors for the name.
    lda .char_row
    sta P0
    lda .char_col
    sta P1
    lda #BLUE
    sta P2
    lda #CYAN
    sta P3
    lda .max_len
    sta P4
    jsr font_s_prepare_colors

    ; Player's score.
    ; Destination coordinates must be adjusted accordingly.  The row will be
    ; the same, but the column will be incremented by '.name_len + 1'.
    lda .char_col
    clc
    adc .name_len
    adc #1
    sta .char_col
    asl
    asl
    sta .bitmap_col
    ; Score text.
    lda #<sc_v_score_str_buffer
    sta P0
    lda #>sc_v_score_str_buffer
    sta P1
    lda .bitmap_row
    sta P2
    lda .bitmap_col
    sta P3
    lda #3
    sta P4
    jsr font_s_draw_text
    ; Score colors.
    lda .char_row
    sta P0
    lda .char_col
    sta P1
    lda sc_v_score_str_color
    sta P2
    lda #CYAN
    sta P3
    lda #3
    sta P4
    jsr font_s_prepare_colors
    
    ; In match play, we will want to display also the number of holes still to
    ; play after the score.  So calculate and record the column where this
    ; should be drawn.
    ldx .char_col
    ldy #0
-
    lda sc_v_score_str_buffer,y
    cmp #font_c_ASCII_SPACE 
    beq +
    inx
    iny
    cpy #3
    bne -
+

    rts
; end sub sc_s_draw_name_and_score
} ; !zone

; **************************************************

!zone {
sc_s_calc_team_scores
    ; Nothing to do unless this is match play.
    lda shared_v_is_match_play
    +branch_if_true +
    rts ; EXIT POINT.

+
    ldx round_v_current_hole
    ; Use Y as offset from sc_v_front9_scores.
    ldy #0
    cpx #9
    bcc +
    ldy #4
+
    lda sc_v_round_scores+2
    sec
    sbc sc_v_round_scores+3

    beq .draw
    bcs .team1_wins

    ; So team0 wins...
    lda #2
    sta sc_v_team_scores,x
    clc
    adc sc_v_front9_scores,y
    sta sc_v_front9_scores,y
    bne +

.draw
    inc sc_v_team_scores,x
    inc sc_v_team_scores+18,x
    ; NOTE: easier to put Y into X so can use inc directly.
    tya
    tax
    inc sc_v_front9_scores,x
    inc sc_v_front9_scores+1,x
    bne +

.team1_wins
    lda #2
    sta sc_v_team_scores+18,x
    clc
    adc sc_v_front9_scores+1,y
    sta sc_v_front9_scores+1,y

+
    jsr sc_s_calc_team_position

    rts
; end sub sc_s_calc_team_scores
} ; !zone

; **************************************************

; NOTE: this routine will calculate which team is winning overall, up to
; this point in the round.
; Use the first two bytes of sc_v_round_scores to store these values.
!zone {
sc_s_calc_team_position
    ; Use MATHS0 and MATHS1 to hold total score for team0 and team1 
    ; respectively.
    lda #0
    sta MATHS0
    sta MATHS1

    ; And now get total score (so far) for each team.
    ; FIXME: is this correct?!
    clc ; Only need to do this once.
    ldx #17
-
    lda MATHS0
    adc sc_v_team_scores,x
    sta MATHS0
    lda MATHS1
    adc sc_v_team_scores+18,x
    sta MATHS1
    dex
    bpl -

    lda MATHS0
    sec
    sbc MATHS1
    beq .all_square
    bcs .team0_winning

    ; So team1 is winning...
    +nega
    lsr
    sta sc_v_round_scores+1
    +nega
    sta sc_v_round_scores
    rts ; EXIT POINT.

.team0_winning
    lsr
    sta sc_v_round_scores
    +nega
    sta sc_v_round_scores+1
    rts ; EXIT POINT.

.all_square
    sta sc_v_round_scores
    sta sc_v_round_scores+1

    rts
; end sub sc_s_calc_team_position
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

sc_c_SIZE = *-sc_c_BEGIN

