; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


round_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; State machine.
round_c_STATE_SHOT_IN_PROGRESS  = 0
round_c_STATE_SHOT_COMPLETE     = 1
round_c_STATE_SCORE_CARDS       = 2
round_c_STATE_WIPING_SCREEN     = 3

round_c_MAX_PUTT_DISTANCE_LO = <(20*hole_c_PIXELS_PER_YARD)
round_c_MAX_PUTT_DISTANCE_HI = >(20*hole_c_PIXELS_PER_YARD)

round_c_LOADING_MSG !raw    "Loading hole ",0,0
round_c_LOADING_MSG_LEN = 15

; Index into these two tables is 'shared_v_holes'.
; Cf. 0 = 18, 1 = front nine, 2 = back nine.
round_l_FIRST_HOLE_TO_PLAY          !byte   0,0,9
round_l_LAST_HOLE_TO_PLAY_PLUS_ONE  !byte   18,9,18

; Messages that can be sent to round manager from other modules
; (e.g. ball and golfer).
round_c_MSG_NONE            = 0
round_c_MSG_CUT_SHORT       = 1
round_c_MSG_NEXT_SHOT       = 2
round_c_MSG_CONCEDE_HOLE    = 3

round_c_SCORE_TABLE_CODE_STROKE_PLAY_FILENAME   !pet "sctblsp.prg",0
round_c_SCORE_TABLE_CODE_MATCH_PLAY_FILENAME    !pet "sctblmp.prg",0

!if _DEBUG_TEST_HOLE_ {
round_c_TRY_HOLE_FILENAME   !pet    "tryhole.prg",0
} ; !if


; *****************
; *** VARIABLES ***
; *****************
; Count from 0 to 17.
round_v_current_hole      !byte   0
round_v_current_state     !byte   0
round_v_current_player    !byte   0
; NOTE: this updated after each shot (inside 'golfer_record_distance').
; It's then used by 'ball_s_display_distance_of_shot' to report on current lie.
round_v_current_player_is_on_green  !byte   0
; NOTE: at the start of each hole, we don't refresh the playing order between
; shots.  First, everyone tees off, either in the order they signed in (for
; hole #1) or according to their score (hole #2 onwards) - best player first.
; The variable 'round_v_current_tee_index' keeps track of where we are in that
; initial teeing-off stage (- use it as an index into the
; 'players_playing_order' array).
round_v_teeing_off          !byte   0
round_v_current_tee_index   !byte   0
round_v_must_putt           !byte   0
round_v_must_render         !byte   0
round_v_must_retake_shot    !byte   0
; Use this to find the correct sprite data.
round_current_gender_offset !byte   0
; For your convenience:
round_v_current_sky_color     !byte   CYAN
round_v_current_ground_color    !byte   ORANGE

; FIXME: temporary code!!!
round_v_hole_filename   !pet    "h0000.prg",0
round_v_allow_water     !byte   0
; Stored here for your convenience after being calculated in 'stats' module
; for current shot.  Either in feet or yards, depending on whether putting
; or not.
round_v_current_distance_lo !byte   0
round_v_current_distance_hi !byte   0

round_v_msg_received    !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
round_s_init
    jsr gfxs_s_init_colors
    jsr sc_s_reset_all
    jsr golfer_init2
    jsr sstore_init
    jsr players_s_reset
    jsr bdrop_s_init

    lda #0
    sta round_v_current_player
    ldx shared_v_holes
    lda round_l_FIRST_HOLE_TO_PLAY,x
    sta round_v_current_hole

    lda #0
    sta transtn_v_is_active

    lda #1
    sta round_v_must_render

    jsr msg_s_clear

    rts
; end sub round_s_init
} ; !zone

; **************************************************

!zone {
.OFFSET_TO_QUADS = 18

round_s_load_hole

!if _DEBUG_TEST_HOLE_ {
    ldx #<round_c_TRY_HOLE_FILENAME
    ldy #>round_c_TRY_HOLE_FILENAME

} else {
    ; Is the round complete?
    ldx shared_v_holes
    lda round_v_current_hole
    cmp round_l_LAST_HOLE_TO_PLAY_PLUS_ONE,x
    bne +
    ; YES, round is complete!!!
-   inc EXTCOL
    jmp -

    ; Turn the current hole number (counting from 0) into decimal digits.
;    lda round_v_current_hole
+
    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; Convert these digits into PETSCII (same as ASCII for '0' to '9') and
    ; write those bytes directly into the template filename.
    ; Now we have the filename that we want to load.
    lda utils_v_dec_digits    
    clc
    adc #font_c_ASCII_0 
    sta round_v_hole_filename+4
    lda utils_v_dec_digits+1    
    clc
    adc #font_c_ASCII_0 
    sta round_v_hole_filename+3
    ; And we must do the same again for the course index.
    lda shared_v_course_index
    ; FIXME: only one course at the moment!
;    lda #1

    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    lda utils_v_dec_digits    
    clc
    adc #font_c_ASCII_0 
    sta round_v_hole_filename+2
    lda utils_v_dec_digits+1    
    clc
    adc #font_c_ASCII_0 
    sta round_v_hole_filename+1

    ldx #<round_v_hole_filename
    ldy #>round_v_hole_filename
} ; !if ... !else

    sei
    lda #BLACK
    sta BGCOL0
    jsr CB_LOADFILE_EXOMIZER
    cli
    ; Turn SuperCPU back on, if present.  (Multiload routines turn it off!)
    +utils_m_turn_on_supercpu

    lda #$00
    sta P0
    lda #$e0
    sta P1

    ; Start loading from $e000-$ffff.
    sei
    lda R6510
    and #%11111101
    sta R6510

    ; NOTE: input to target_init is the address held in P0/P1.  It records
    ; the first four bytes for the position of the target.
    jsr target_init
    jsr players_s_reset_distance
    ; 'hole_init' routine records width, length and par for current hole.
    jsr hole_s_init

    ; Now jump over the first fourteen bytes (which are the coordinates of the 
    ; target, distance to target, boundaries, par and hazard type).
    lda P0
    clc
    adc #.OFFSET_TO_QUADS
    sta P0
    lda P1
    adc #0
    sta P1
    jsr quads_s_load

    ; Zero-page locations P0-P1 should now point to the byte AFTER the quads
    ; data.  That's the number of waypoints.  Same holds after waypoints have
    ; loaded...
    jsr waypts_load
    jsr trees_s_load

    ; P0-P1 now holds location of overhead map icon.
    jsr hole_s_load_overhead_map

    ; Finished loading.
    lda R6510
    ora #$02
    sta R6510
    cli

    lda #1
    sta round_v_teeing_off
    ; Set this index to -1, so when ordering players and we're 'teeing off',
    ; will correctly be incremented to 0.
    lda #(-1)
    sta round_v_current_tee_index
    ; If this is the first (i.e. 0th) hole, playing order doesn't need to
    ; be refreshed.
    lda round_v_current_hole
    beq +
    jsr players_s_refresh_order 
+

    rts
; end sub round_s_load_hole
} ; !zone

; **************************************************

; NOTE: call this after the new hole has been loaded!
!zone {
round_s_init_hole
    jsr wind_s_init
    jsr slope_s_init
    jsr players_s_prepare_next_hole
    jsr stats_s_clear_balls_holed
    jsr bdrop_s_rotate_objects
    jsr round_s_set_bg_colors
    ; NOTE: OK to set state directly here because interrupts are turned off.
    lda #round_c_STATE_SHOT_IN_PROGRESS
    sta round_v_current_state
    rts
; end sub round_s_init_hole
} ; !zone

; **************************************************

!zone {
round_s_loop
    jsr round_s_prepare_hole
    
.loop_top
    ; Do nothing if waiting for interrupts to deal with a 'change of
    ; state' request.
    lda interrupts_v_state_change_request
    bpl .loop_top

    lda round_v_current_state
    cmp #round_c_STATE_SHOT_IN_PROGRESS
    beq .shot_in_progress
    cmp #round_c_STATE_SHOT_COMPLETE
    beq .shot_complete
    cmp #round_c_STATE_SCORE_CARDS
    beq .score_cards
    cmp #round_c_STATE_WIPING_SCREEN
    beq .wiping_screen
    jmp .loop_top

.shot_in_progress
    jsr ball_s_update_full_tilt
    jsr round_s_check_if_shot_complete
    jmp .loop_top

.shot_complete
;    lda wash_v_active
;    +branch_if_true .loop_top
    ; NOTE: won't get this message unless wash has stopped!
    lda round_v_msg_received
    cmp #round_c_MSG_NEXT_SHOT
    bne .loop_top
    jsr round_s_handle_request_next_shot
    jmp .loop_top
    
.score_cards
    jsr round_s_check_score_cards_exit
    jmp .loop_top

.wiping_screen
    lda transtn_v_is_active
    +branch_if_true .loop_top
    jsr round_s_prepare_hole
    bcs .end_of_round
    lda #round_c_STATE_SHOT_IN_PROGRESS
    sta interrupts_v_state_change_request
    jmp .loop_top

.end_of_round
    jsr interrupts_s_uninstall
    rts

; end sub round_s_loop
} ; !zone

; **************************************************

!zone {
round_s_handle_request_next_shot
    ; Ball has stopped moving.  Current player's position and distance
    ; from target have been updated and recorded.  And they've pressed the
    ; fire button to signal that they're finished contemplating the shot...

    ; TODO: turn off any particle effect - don't want it to continue and
    ; mess up sprite colors.
    jsr partsys_s_deactivate
    jsr golfer_s_hide_shadow

    jsr round_s_check_if_hole_complete
    bcc +

    ; Hole is complete.  Before score cards are displayed, players should be 
    ; ordered as per their round scores.  We can enforce this by setting the
    ; 'round_v_teeing_off' flag before routine call.
    +set round_v_teeing_off
    jsr players_s_refresh_order
    jsr round_s_load_score_table
    rts ; EXIT POINT.

+
    lda round_v_must_retake_shot
    +branch_if_false +
    jsr round_s_set_ball_position
    ; NOTE: need to clear this explicitly because we're not calling
    ; 'round_s_order_players'.
    +clr round_v_must_render
    beq ++
+
    jsr round_s_order_players
    jsr round_s_set_joystick
    jsr round_s_swing_or_putt
    jsr round_s_set_ball_position
    lda round_v_must_render
    +branch_if_false ++
    jsr round_s_render_3D
++
    jsr round_s_prepare_for_shot
    ldx joy_v_current_port
    +joy_m_lock_fire
    lda #round_c_STATE_SHOT_IN_PROGRESS
    sta interrupts_v_state_change_request
    rts
; end sub round_s_handle_request_next_shot
} ; !zone

; **************************************************

; The object of the routine is to get the correct index into
; round_v_current_player.  I.e. who's the next player up?
!zone {
round_s_order_players
    lda #1
    sta round_v_must_render

    lda round_v_teeing_off
    +branch_if_false .mid_hole
    inc round_v_current_tee_index
    lda round_v_current_tee_index
    cmp shared_v_num_players
    bne .still_teeing_off

    +clr round_v_teeing_off
    jmp .mid_hole

.still_teeing_off
    ; NOTE: round_v_current_tee_index is still in .A.
    tax
    beq +
    dec round_v_must_render
+   
;    ldx round_v_current_tee_index
    lda players_v_playing_order,x
    sta round_v_current_player
    rts ; EXIT POINT.

.mid_hole
    jsr players_s_refresh_order
    lda players_v_playing_order
    sta round_v_current_player
;    ; TODO: check if current player has finished hole ('current shots' value
;    ; will be negative).  If so, all players have finished the hole...
;    tax
;    lda players_current_shots,x
;    bpl +
;    jsr round_next_hole
;+
    rts
; end sub round_s_order_players
} ; !zone

; **************************************************

!zone {
round_s_rotate_world
;    ; Set ball's world position for current player.
;    ldx round_v_current_player
;    lda players_v_ball_pos_x_lo,x
;    sta ball_world_x_lo
;    lda players_v_ball_pos_x_hi,x
;    sta ball_world_x_hi
;    lda players_v_ball_pos_z_lo,x
;    sta ball_world_z_lo
;    lda players_v_ball_pos_z_hi,x
;    sta ball_world_z_hi

;    lda #$e2
;    sta ball_world_x_lo
;    lda #$ff
;    sta ball_world_x_hi
;    lda #$93
;    sta ball_world_z_lo
;    lda #$12
;    sta ball_world_z_hi

    ; Move quads, target & trees relative to ball's (world) position.
    jsr quads_set_aa_coords
    jsr target_s_recalculate_aa_coords
    jsr trees_s_set_aa_coords
    jsr hole_s_set_aa_boundaries

    ; Work out rotation based on current waypoint.
    jsr waypts_find_index
    jsr waypts_calc_rotation
    ; NOTE: we still want to rotate the world even if waypoint dictates there
    ; should be no rotation - otherwise the wind indicator doesn't get 
    ; initialized!
;    bcs +

    ; Rotate world accordingly...
    jsr quads_rotate
    jsr target_rotate
    jsr trees_s_rotate
    jsr round_windslope_rotate
;    jsr bdrop_s_rotate_tower
    jsr golfer_s_rotate_direction

;+
    ; Finally, place ball at the (transformed) origin.
;    +target_reset_hw_sprite
    ;jsr ball_set_at_origin

    rts
; end sub round_s_rotate_world
} ; !zone

; **************************************************

; Determine whether the current player should prepare a swing (with wood,
; iron or wedge) or a putt.  Depends on both distance from target and current
; terrain.
!zone {
round_s_swing_or_putt
    +clr round_v_must_putt
    ldx round_v_current_player

    ; First check the terrain - anything other than green/fairway and it's
    ; a swing.
    lda players_v_terrain,x
    ; Put this in MATHS0 - we might use it again later.
    sta MATHS0
    cmp #ball_c_TERRAIN_GREEN_FWAY
    bne .swing

    ; Terrain OK so check distance...
    lda players_v_distance_lo,x
    cmp #round_c_MAX_PUTT_DISTANCE_LO
    lda players_v_distance_hi,x
    sbc #round_c_MAX_PUTT_DISTANCE_HI
    bcs .swing

    ; Putt.
    ; Force golf club selection - when putter is selected, this isn't recorded
    ; in 'players' module.
    lda #CLUBS_PUTTER_I
    sta clubs_v_current_selection
    inc round_v_must_putt
    ; FIXME: better off in a separate routine (also below).
    ; Set correct X-position for golfer.
    lda #GOLFER_POS_X_LO+GOLFER_POS_PUTTING_X_OFFSET 
    sta spr_v_x_lo
    sta spr_v_x_lo+1
    rts ; EXIT POINT.

.swing
    ; NOTE: we previously stored terrain type in MATHS0.
    jsr clubs_s_set_min_allowed
    ; If we're retaking a shot, player keeps previous club.  Otherwise auto-
    ; select the best one with respect to distance.
    lda round_v_must_retake_shot
    +branch_if_true .end
    jsr clubs_s_select_best_club

.end
    rts
; end sub round_s_swing_or_putt
} ; !zone

; **************************************************

; OUTPUT:   C flag clear if hole still being played; C flag set if it's
;           finished.
!zone {
round_s_check_if_hole_complete
    ldx #0
-   
    lda players_v_current_shots,x
    bpl .still_playing_hole
    inx
    cpx shared_v_num_players
    bne -

    ; The hole is finished - negative bit is set for all players'
    ; 'current_shots' values.
    jsr sc_s_calc_team_scores
    inc round_v_current_hole
    sec
    rts ; EXIT POINT.

.still_playing_hole
    clc
    rts
; end sub round_s_check_if_hole_complete
} ; !zone

; **************************************************

!zone {
round_windslope_rotate
    lda round_v_must_putt
    +branch_if_true .putt

    ; It's a swing, so rotate the wind.
    jsr wind_s_rotate
    rts ; EXIT POINT.

.putt
    jsr slope_s_rotate
    rts
; end sub round_windslope_rotate
} ; !zone

; **************************************************

; NOTE: we will draw the direction only if there is some wind/slope!
!zone {
round_s_draw_direction
;    jsr winslp_s_draw_direction

    lda round_v_must_putt
    +branch_if_true .putt

    ; So swing/pitch...
    jsr wind_s_draw_speed
    lda wind_v_index
    bne .draw_direction
    rts ; EXIT POINT.

.putt
    jsr slope_s_draw
    lda slope_v_index
    beq .end
.draw_direction
    jsr winslp_s_draw_direction

.end
    rts
; end sub round_s_draw_direction
} ; !zone

; **************************************************

!zone {
round_s_set_joystick
    ldx round_v_current_player
    lda shared_v_player_joysticks,x
    sta joy_v_current_port
    rts
; end sub round_s_set_joystick
} ; !zone

; **************************************************

; OUTPUT:   Z flag set if true!
;!zone {
;round_is_game_over
;    lda round_v_current_hole
;    cmp #ROUND_HOLES_TO_PLAY
;    rts
;; end sub round_is_game_over
;} ; !zone

; **************************************************

!zone {
round_s_set_bg_colors
    lda #CYAN
    sta round_v_current_sky_color
    lda #ORANGE
    sta round_v_current_ground_color
    rts
; end sub round_s_set_bg_colors
} ; !zone

; **************************************************

; OUTPUTS:  C flag clear if more holes to play, or set if round complete.
!zone {
round_s_prepare_loading_msg
    ; A different message will be displayed if the round is complete.
    ldx shared_v_holes
    lda round_l_LAST_HOLE_TO_PLAY_PLUS_ONE,x
    cmp round_v_current_hole
    bne +
;    lda round_v_current_hole
;    cmp #10
;    bne +
    ldx #msg_c_WAIT
    jsr msg_s_display_stock_msg
    sec
    rts ; EXIT POINT.

+
    lda #font_c_ASCII_SPACE 
    sta round_c_LOADING_MSG+14

    ldx round_v_current_hole
    inx
    stx P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    lda #<round_c_LOADING_MSG+13
    sta P0
    lda #>round_c_LOADING_MSG+13
    sta P1
    jsr utils_s_write_digits_to_buffer

    lda #<round_c_LOADING_MSG
    sta P0
    lda #>round_c_LOADING_MSG
    sta P1
    lda #round_c_LOADING_MSG_LEN 
    sta P4
    jsr msg_s_display

    clc
    rts
; end sub round_s_prepare_loading_msg
} ; !zone

; **************************************************

!zone {
round_s_clear_non_scenery_sprites
    ; Turn off all sprites except hole (= target!).
    ldx target_hw_spr_num
    lda utils_l_BIT_LOOKUP,x        
    ; FIXME: flicker?!
    sta SPENA
    jsr bdrop_s_find_active_hw_sprites
    lda MATHS0
    ora SPENA
    sta SPENA
    rts
; end sub round_s_clear_non_scenery_sprites
} ; !zone

; **************************************************

; NOTE: assume interrupts are disabled when this routine is called.
!zone {
round_s_render_3D
    jsr tweeter_s_turn_off
    jsr snd_s_clear_all

    sei

    +clr SPENA

    lda #CYAN
    sta BGCOL0
    jsr gfxs_s_clear_bitmap
    jsr round_s_rotate_world

    jsr stats_s_draw_all
    jsr gfxs_s_init_colors
    jsr ingm_s_draw_rough
    jsr quads_s_project_and_draw
    jsr bdrop_s_draw_all
    jsr target_s_project
    jsr target_s_draw_shadow
    jsr trees_s_draw
    jsr target_s_draw
    jsr winslp_s_draw_box
    jsr round_s_draw_direction
    jsr golfer_s_init_shadow

    lda round_v_must_putt
    +branch_if_true +
    jsr hole_s_draw_overhead_map
    lda round_v_teeing_off
    +branch_if_false +
    jsr tmarkers_s_draw
+

    jsr powarc_s_draw_dummy
    +utils_m_kernal_out
    jsr quads_s_add_textures
    +utils_m_kernal_in

    cli
    rts
; end sub round_s_render_3D
} ; !zone

; **************************************************

!zone {
round_s_prepare_for_shot
    lda #0
    sta round_v_must_retake_shot
    sta round_v_msg_received

    ; If we rendered the 3D, stats have already been drawn!
    lda round_v_must_render
    +branch_if_true +
    jsr stats_s_draw_name
    jsr stats_s_refresh_hole
    jsr stats_s_draw_current_shots
+

    jsr target_s_reset
    jsr ball_s_set_at_origin
    ; NOTE: this will cause some flicker because have to turn off interrupts
    ; while sprites are loaded in (written in I/O area!).
    jsr golfer_s_setup_draw
   
    lda round_v_must_putt
    +branch_if_true +
    jsr hole_s_init_overhead_ball_sprite
+

    sec
    jsr partsys_s_clear_sprite

    jsr golfer_s_restore_shadow
    jsr powarc_s_reset
    jsr gfxs_s_clear_msg_area
    jsr terrain_s_draw
    jsr playmsg_s_display_alert
    jsr clubs_s_draw2
    jsr tweeter_s_turn_on

    ldx #%11101110
    lda round_v_must_putt
    +branch_if_true +
    inx
+
    stx SPENA
    lda #0
    sta XXPAND

!if _DEBUG_PHYSICS_VARS_ {
    jsr golfer_s_redraw_physics_vars
} ; !if

    rts
; end sub round_s_prepare_for_shot
} ; !zone

; **************************************************

!zone {
round_s_check_if_shot_complete
    lda ball_v_current_state
    bne .end

    ; NOTE: must check this before golfer state, because state will be
    ; 'IN_LIMBO' rather than 'FINISHED'.
    lda round_v_msg_received
    cmp #round_c_MSG_CONCEDE_HOLE
    bne +
    jsr round_s_handle_team_concede_hole
    rts ; EXIT POINT.

+
    lda golfer_v_current_state
    cmp #golfer_c_STATE_FINISHED
    bne .end

    ; Process the shot!
    ; NOTE: this only for match play.
    ldy round_v_current_player
    jsr stats_s_inc_current_shots

    jsr golfer_s_update_score_card
    bcs .holed
+
    jsr round_s_check_if_must_retake_shot
    bcs .complete

    ; A valid shot has been taken though the ball has not yet been holed.
    ; Therefore distance & terrain must be recorded.  (A shot from water will 
    ; be allowed in this case!)
    jsr ball_s_do_final_terrain_check
    jsr ball_s_calc_distance_of_shot
    jsr ball_s_calc_new_world_position
    jsr golfer_s_store_new_ball_position
    jsr target_s_recalculate_aa_coords
    jsr target_s_rotate_to_pos_z_axis
    jsr golfer_s_record_distance2
    jsr golfer_s_record_terrain
.complete
    jsr ball_s_display_distance_of_shot
.holed

    lda round_v_msg_received
    cmp #round_c_MSG_CUT_SHORT
    bne +
    jsr round_s_handle_request_next_shot
    rts ; EXIT POINT.

+
    lda #round_c_STATE_SHOT_COMPLETE
    sta interrupts_v_state_change_request

.end
    rts
; end sub round_s_check_if_shot_complete
} ; !zone

; **************************************************

; OUTPUTS:  C flag set if must retake; else clear.
!zone {
round_s_check_if_must_retake_shot
    lda ball_v_is_in_water
    +branch_if_false .check_bounds
    ; Force terrain.
    lda #ball_c_TERRAIN_WATER
    sta ball_v_current_terrain
.retake
    sta round_v_must_retake_shot
    sec
    rts ; EXIT POINT.

.check_bounds
    lda ball_v_out_of_bounds
    +branch_if_true .retake
    clc
    rts
; end sub round_s_check_if_must_retake_shot
} ; !zone

; **************************************************

!zone {
round_s_set_ball_position
    ; Set ball's world position for current player.
    ldx round_v_current_player
    lda players_v_ball_pos_x_lo,x
    sta ball_world_x_lo
    lda players_v_ball_pos_x_hi,x
    sta ball_world_x_hi
    lda players_v_ball_pos_z_lo,x
    sta ball_world_z_lo
    lda players_v_ball_pos_z_hi,x
    sta ball_world_z_hi
    rts
; end sub round_s_set_ball_position
} ; !zone

; **************************************************

!zone {
round_s_load_score_table
    jsr round_s_clear_non_scenery_sprites
    jsr golfer_s_push_off_screen

    ldx #msg_c_LOADING_SCORE_CARDS
    jsr msg_s_display_stock_msg 

    lda shared_v_is_match_play 
    +branch_if_true .match_play
    ldx #<round_c_SCORE_TABLE_CODE_STROKE_PLAY_FILENAME
    ldy #>round_c_SCORE_TABLE_CODE_STROKE_PLAY_FILENAME
    bne +
.match_play
    ldx #<round_c_SCORE_TABLE_CODE_MATCH_PLAY_FILENAME
    ldy #>round_c_SCORE_TABLE_CODE_MATCH_PLAY_FILENAME
+
    jsr CB_LOADFILE_EXOMIZER
    +utils_m_turn_on_supercpu
    ; NOTE: the score_table module is loaded in here (@ quads_n) and the 
    ; subroutine we need to call (= sc_s_init) is at the very top of the file.
    jsr quads_n
    jsr msg_s_clear

    lda #round_c_STATE_SCORE_CARDS
    sta interrupts_v_state_change_request

    rts
; end sub round_s_load_score_table
} ; !zone

; **************************************************

!zone {
round_s_check_score_cards_exit
    ; Just listen out for a button press to resume play...
    ldx #joy_c_PORT2
    +joy_m_is_fire
    bne .release_lock

    +joy_m_is_locked_fire
    +branch_if_true .end
    +joy_m_lock_fire
    jsr round_s_init_screen_wipe
    rts ; EXIT POINT.

.release_lock
    +joy_m_release_fire

.end
    rts
; end sub round_s_check_score_cards_exit
} ; !zone

; **************************************************

!zone {
round_s_init_screen_wipe
    jsr transtn_s_init
    jsr tweeter_s_turn_off
    jsr snd_s_clear_all
    lda #round_c_STATE_WIPING_SCREEN
    sta interrupts_v_state_change_request
    rts
; end sub round_s_init_screen_wipe
} ; !zone

; **************************************************

; OUTPUTS:  C flag set if end of round; else clear.
!zone {
round_s_prepare_hole
    jsr interrupts_s_uninstall
    jsr round_s_prepare_loading_msg
    bcs .end_of_round
    jsr round_s_load_hole
    jsr round_s_init_hole
    jsr round_s_order_players
    jsr round_s_set_joystick
    jsr round_s_swing_or_putt
    jsr round_s_set_ball_position
    jsr round_s_render_3D
    jsr round_s_prepare_for_shot
    jsr interrupts_s_install
    clc
    rts ; EXIT POINT.

.end_of_round
    rts
; end sub round_s_prepare_hole
} ; !zone

; **************************************************

; INPUTS:   X = msg (see round_c_MSG_ constants above).
; OUTPUTS:  C flag set if msg accepted; otherwise clear.
!zone {
round_s_record_msg
    lda round_v_msg_received
    bne .reject ; already an unprocessed message (does this ever happen?!)

    cpx #round_c_MSG_CUT_SHORT
    beq .accept
    cpx #round_c_MSG_CONCEDE_HOLE
    beq .accept
    cpx #round_c_MSG_NEXT_SHOT
    bne .reject ; invalid msg
    ; Wash must be completed before can go to next shot.
    lda wash_v_active
    +branch_if_true .reject

.accept
    stx round_v_msg_received
    sec
    rts ; EXIT POINT.

.reject
    clc
    rts
; end sub round_s_record_msg
} ; !zone

; **************************************************

!zone {
round_s_handle_team_concede_hole
    ; Ensure that the other team wins.
    ; First set negative bit on all 'current_shots' variables to indicate
    ; that they've finished the hole.
    ldx #players_c_MAX_N-1
-
    lda players_v_current_shots,x
    ora #$80
    sta players_v_current_shots,x
    dex
    bpl -
    ; Indices 2 and 3 of 'sc_v_round_scores' are used to store running total
    ; of teams' shots.  Set both to 0, then increment for current team.
    lda #0
    sta sc_v_round_scores+2
    sta sc_v_round_scores+3
    ldy round_v_current_player
    ldx shared_v_team_membership,y
    inc sc_v_round_scores+2,x
    ; Can now call this routine to end the hole with appropriate score.
    jsr round_s_handle_request_next_shot
    rts
; end sub round_s_handle_team_concede_hole
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

round_c_SIZE = *-round_c_BEGIN

