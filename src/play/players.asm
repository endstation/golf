; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


players_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
players_c_MAX_N = 4
players_c_MALE    =   0
players_c_FEMALE  =   1
players_c_JOY2    =   0
players_c_JOY1    =   1

; Even holes 4 players, even holes 2 players, odd holes 4 players,
; odd holes 2 players.
players_l_MATCH_PLAY_ORDER    !byte   0,1, 0,1,2,3, 1,0, 2,3,0,1


; *****************
; *** VARIABLES ***
; *****************
players_v_playing_order   !fill   players_c_MAX_N
players_v_distance_lo     !fill   players_c_MAX_N
players_v_distance_hi     !fill   players_c_MAX_N
players_v_current_shots   !fill   players_c_MAX_N
players_v_ball_pos_x_lo   !fill   players_c_MAX_N
players_v_ball_pos_x_hi   !fill   players_c_MAX_N
players_v_ball_pos_z_lo   !fill   players_c_MAX_N
players_v_ball_pos_z_hi   !fill   players_c_MAX_N
players_v_terrain         !fill   players_c_MAX_N
;players_v_current_club    !fill   players_c_MAX_N


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
players_s_reset
    ; Default player is white male using joystick #2 (- because that's what
    ; I am and use - no other reason ;).
    ldx #0
-   
;    lda #0
;    sta players_v_current_club,x
    ; Default playing order = 0,1,2,3.
    txa
    sta players_v_playing_order,x
    inx
    cpx #shared_c_MAX_PLAYERS
    bne -

    rts
; end sub players_s_reset
} ; !zone

; **************************************************

; NOTE: this routine is called when a new hole is begun.
; INPUTS:   P0/P1 = address of course data.  Use offset=4.
; TODO: set the initial ball position here as well.  It'll be slightly
;       forward of the world origin.
!zone {
players_s_reset_distance
    ; Use P2 and P3 to store low and high bytes.
    ldy #4
    lda (P0),y
    sta P2
    iny
    lda (P0),y
    sta P3

    ldx #0
-   lda P2
    sta players_v_distance_lo,x
    lda P3
    sta players_v_distance_hi,x
    inx
    cpx #players_c_MAX_N
    bne -

    ; At the start of a hole, all players are on the fairway.
    ldx #0
    lda #ball_c_TERRAIN_GREEN_FWAY
-   sta players_v_terrain,x
    inx
    cpx #players_c_MAX_N
    bne -

    rts
; end sub players_s_reset_distance
} ; !zone

; **************************************************

; Sort playing order based on distance from hole.
; Indices are in P3 and P4.  We want the player furthest from the target
; to be closer to the beginning of the list.
; INPUT:    P3 = index #1, P4 = index #2
; OUTPUT:   set C flag if indices should be swapped; otherwise clear.
!zone {
players_s_compare_distance
    ldx P3
    ldy P4

    ; Look at 'current_shots' value for P3 and P4.  A negative value indicates
    ; that this player has finished the hole.  There are 4 possible scenarios:
    ; i)    P3=$ff, P4=+ve : swap
    ; ii)   P3=$ff, P4=$ff : OK
    ; iii)  P3=+ve, P4=$ff : OK
    ; iv)   P3=+ve, P4=+ve : check distance
    lda players_v_current_shots,y
    bmi .no_swap
    lda players_v_current_shots,x
    bmi .swap

.distance
    lda players_v_distance_lo,x
    sec
    sbc players_v_distance_lo,y
    lda players_v_distance_hi,x
    sbc players_v_distance_hi,y
    +bge_s .no_swap
.swap
    sec
    rts
.no_swap
    clc
    rts
; end sub players_s_compare_distance
} ; !zone

; **************************************************

; Order relative to current score.
; INPUTS:   P3 = index #1, P4 = index #2
; OUTPUTS:  C flag set if need to swap, otherwise clear.
!zone {
players_s_compare_score
    ldx P3
    ldy P4

    ; We will do a signed 8-bit comparison.
    lda sc_v_round_scores,x
    sec
    sbc sc_v_round_scores,y
    beq .no_swap
    bvc +
    eor #$80
+
    bmi .no_swap

    ; So must swap.
    sec
    rts ; EXIT POINT

.no_swap
    clc
    rts
; end sub players_s_compare_score
} ; !zone

; **************************************************

; OUTPUTS:  C flag clear if players ordered; C flag set if all players
;           have now finished the hole.
!zone {
players_s_refresh_order
    lda round_v_teeing_off
    +branch_if_false +
    lda shared_v_scoring
    cmp #shared_c_MATCH_PLAY
    bne +
    ; Match play & teeing off, so call separate routine to deal with
    ; playing order.
    jsr players_s_refresh_order_match_play
    rts ; EXIT POINT.

+
    ; Nothing to do if there's only one player!
    lda shared_v_num_players
    cmp #2
    bcs +
    rts ; EXIT POINT.
+
    sta P2
    ; Prepare call to bubble sort.
    lda #<players_v_playing_order
    sta P0
    lda #>players_v_playing_order
    sta P1
;    lda players_n
;    sta P2

    ; Now install the 'compare' routine.
    ; It won't be the start of the round but is it the start of the current
    ; hole?
    lda round_v_teeing_off
    +branch_if_true .start_of_hole
    ; Hole's already started, so order by distance.
    lda #<players_s_compare_distance
    sta utils_s_comp+1
    lda #>players_s_compare_distance
    sta utils_s_comp+2
    jmp +

.start_of_hole
    lda #<players_s_compare_score
    sta utils_s_comp+1
    lda #>players_s_compare_score
    sta utils_s_comp+2

+
    jsr utils_s_bubble_sort

    rts
; end sub players_s_refresh_order
} ; !zone

; **************************************************

; NOTE: if this routine is called, players are teeing off (and it's 
; match play).
!zone {
players_s_refresh_order_match_play
    ; Use X as source index and Y as destination.  Stop when Y = 2 or 4 
    ; (depending on number of players).
    ldx #0
    ldy #0
    lda round_v_current_hole
    and #$01
    beq +
    ; Odd so start at X=6.
    ldx #6
+
    lda shared_v_num_players
    ; NOTE: self-modifying code!!!
    sta .mod1+1
    cmp #4
    bne +
    inx
    inx
+
-
    lda players_l_MATCH_PLAY_ORDER,x
    sta players_v_playing_order,y
    inx
    iny
.mod1
    cpy #0
    bne -

    rts
; end sub players_s_refresh_order_match_play
} ; !zone

; **************************************************

!zone {
players_s_prepare_next_hole
    ; Reset 'current_shots' and 'ball_pos' to 0.
    ldx shared_v_num_players
    dex
-   lda #0
    sta players_v_current_shots,x
    sta players_v_ball_pos_x_lo,x
    sta players_v_ball_pos_x_hi,x
    sta players_v_ball_pos_z_lo,x
    sta players_v_ball_pos_z_hi,x
;    sta players_v_current_club,x
    lda #ball_c_TERRAIN_GREEN_FWAY
    sta players_v_terrain,x
    dex
    bpl -

    lda shared_v_scoring
    cmp #shared_c_MATCH_PLAY
    bne .end
    ; Use last two bytes of sc_v_round_scores table to keep running total
    ; of each team's shots during hole.
    lda #0
    sta sc_v_round_scores+2
    sta sc_v_round_scores+3

.end
    rts
; end sub players_s_prepare_next_hole
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

players_c_SIZE = *-players_c_BEGIN


