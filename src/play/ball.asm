; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


ball_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
ball_c_GRAVITY = 10
ball_c_SW_SPR_NUM = 5
ball_c_SHADOW_SW_SPR_NUM = 6

; If ball is to bounce but vy is below this value, bounce will end.
; NOTE: this is the low byte - high byte is assumed to be zero.
ball_c_MIN_VY_ON_BOUNCE !byte $30

; FIXME: probably not the best place to store these...
ball_c_TERRAIN_ROUGH        = 0
ball_c_TERRAIN_BUNKER       = 1
ball_c_TERRAIN_GREEN_FWAY   = 2
ball_c_TERRAIN_WATER        = 3

; Depending on terrain (see above), multiply power by these values (each a 
; fraction of 256).  If 0, leave alone!
ball_c_ROUGH_POWER_REDUCTION = 170
; Index this table with 'shared_v_sand_traps_difficulty'.
ball_l_BUNKER_POWER_REDUCTION   !byte   170,128,85   

; In both cases, how much of the ball's velocity is preserved, on y (elasticity) and
; x/z (friction) axes.
ball_l_ELASTICITY           !byte   10,5,50,0
ball_l_FRICTION_ON_BOUNCE   !byte   130,20,230,0

ball_c_STATE_STATIONARY = 0
ball_c_STATE_MUST_INIT  = 1
ball_c_STATE_IN_FLIGHT  = 2
ball_c_STATE_ROLLING    = 3
ball_c_STATE_SPLASHING  = (-1) 

ball_c_PUTT_SUCCESSFUL = 1
ball_c_PUTT_TOO_FAST   = 2

ball_c_BUMP_VY_LO = $50
ball_c_BUMP_VY_HI = $00

; States for mutex  - these work for ball's position and velocity.
; READY - main thread can read.
; WANT_NEW - raster interrupt must write latest value.
ball_c_MUTEX_READY      = 0
ball_c_MUTEX_WANT_NEW   = 1

BALL_SHADOW_MAX_Y_LO = <(8*hole_c_PIXELS_PER_FOOT) 
BALL_SHADOW_MAX_Y_HI = >(8*hole_c_PIXELS_PER_FOOT) 
BALL_SHADOW_COLORS  !byte   BLACK,GREY1,GREY2,GREY3
BALL_SHADOW_HEIGHTS_LO  !byte   <(2*hole_c_PIXELS_PER_FOOT),<(4*hole_c_PIXELS_PER_FOOT),<(6*hole_c_PIXELS_PER_FOOT)
BALL_SHADOW_HEIGHTS_HI  !byte   >(2*hole_c_PIXELS_PER_FOOT),>(4*hole_c_PIXELS_PER_FOOT),>(6*hole_c_PIXELS_PER_FOOT)
ball_c_NUM_SHADOW_COLORS = 4

; NOTE: this affects the ball's velocity on xz-plane.  Value is a fraction
; of 256.
ball_c_SLOW_DOWN_ON_BUMP_LO = <150 

; NOTE: there can be only one tree-collision event per shot.
BALL_TREE_COLLISION_STATE_NONE      = 0
BALL_TREE_COLLISION_STATE_ACTIVE    = 1
BALL_TREE_COLLISION_STATE_HANDLED   = 2

BALL_SCUFFED_SHOT_VX_LO = 48
BALL_SCUFFED_SHOT_VX_HI = 0
BALL_SCUFFED_SHOT_VY_LO = 64
BALL_SCUFFED_SHOT_VY_HI = 0
BALL_SCUFFED_SHOT_VZ_LO = 90
BALL_SCUFFED_SHOT_VZ_HI = 0

; To be interpreted as fraction of 2^16.
ball_c_DRAG_COEFFICIENT = 12
ball_c_SPIN_COEFFICIENT = 150

; 17 steps (centre and 8 either side).
; Values should be interpreted as fractions of 256.
ball_l_SPIN_AXIS_X_LO
    !byte <(-181),<(-181),<(-181),<(-203),<(-221),<(-236),<(-247),<(-253)
    !byte <255
    !byte <253,<247,<236,<221,<203,<181,<181,<181
ball_l_SPIN_AXIS_X_HI
    !byte >(-181),>(-181),>(-181),>(-203),>(-221),>(-236),>(-247),>(-253)
    !byte >255
    !byte >253,>247,>236,>221,>203,>181,>181,>181
ball_l_SPIN_AXIS_Z_LO
    !byte <181,<181,<181,<155,<127,<97,<66,<33,<0,<33,<66,<97,<127,<155,<181,<181,<181
ball_l_SPIN_AXIS_Z_HI
    !byte >181,>181,>181,>155,>127,>97,>66,>33,>0,>33,>66,>97,>127,>155,>181,>181,>181

ball_c_ROLLING_MIN_VZ !byte $2a
ball_c_PUTTING_MIN_VZ = $70
ball_l_PUTTING_FRICTION_COEFFICIENTS    !byte   200,200,12,0
ball_l_FRICTION_COEFFICIENTS    !byte   80,150,$18,0
ball_c_MAX_PUTTING_SPEED = 1

ball_c_SPIN_TYPE_NONE   = 0
ball_c_SPIN_TYPE_HOOK   = 1
ball_c_SPIN_TYPE_SLICE  = 2

ball_c_SPR_PATTERN_FULL_SIZE    = 0
ball_c_SPR_ADDRESS = $c000+(sprd_c_BALL*64)                   
ball_c_SHADOW_SPR_ADDRESS = $c000+(sprd_c_BALL_SHADOW*64)
; NOTE: for full-size ball, data is same for both rows (offsets 0 and 3).
ball_l_SPR_DATA_EVEN_ROW0   !byte   $c0,$00,$c0,$00
ball_l_SPR_DATA_EVEN_ROW1   !byte   $c0,$00,$c0,$00
ball_l_COLLISION_EVEN       !byte   0,1,0,1
ball_l_SPR_DATA_ODD_ROW0    !byte   $c0,$40,$80,$00   
ball_l_SPR_DATA_ODD_ROW1    !byte   $c0,$40,$80,$00   
ball_l_COLLISION_ODD        !byte   0,1,1,1
ball_l_SPR_DATA_SMALL_ROW0  !byte   $80,$00,$80,$00
ball_l_SPR_DATA_SMALL_ROW1  !byte   $00,$00,$00,$00
ball_l_COLLISION_SMALL      !byte   0,0,0,0
; For smaller ball sprite, data is set for 0th byte only.
; Index this table with 'pattern'&1: this will be 1 if behind a trunk,
; otherwise 0.
ball_l_SPR_DATA_SMALL !byte $80,$00

ball_c_DISTANT_Z = 100*hole_c_PIXELS_PER_YARD 

ball_c_LIE_STRINGS  !raw    "(rough)",0,"(sand)",0,"(fairway)",0
                    !raw    "(water)",0,"(green)",0
ball_l_LIE_OFFSETS  !byte   0,8,15,25,33
ball_c_LIE_STRING_LEN = 10
ball_l_SPLASH_ANIM_DATA !byte   $30,$78,$fc,$78,$30,$00
ball_c_SPLASH_ANIM_FRAME_RATE = 4

; NOTE: these aren't the only values corresponding to these states, but they
; can be used to set 'ball_v_z_pos_rel_to_hole_old/new' in a more readable way.
ball_c_IN_FRONT_OF_HOLE = (-1)
ball_c_BEHIND_HOLE      = 1


; *****************
; *** VARIABLES ***
; *****************
; PHYSICS VARIABLES.  All these must be zeroed out before the ball is launched.
; Ball is generally positioned at the origin of the coordinate system.
; However, immediately after the user has played a shot, we must track the
; ball's offset from the origin as well as record its final position.
ball_x_lo           !byte   0
ball_x_hi           !byte   0
ball_x_fraction     !byte   0
ball_z_lo           !byte   0
ball_z_hi           !byte   0
ball_z_fraction     !byte   0
ball_y_lo           !byte   0
ball_y_hi           !byte   0
ball_y_fraction     !byte   0
; Ball velocity vector.
ball_vx_lo          !byte   0
ball_vx_hi          !byte   0
ball_vy_lo          !byte   0
ball_vy_hi          !byte   0
ball_vz_lo          !byte   0
ball_vz_hi          !byte   0
; Axis-aligned ball coordinates, relative to current origin.
; Use these for checking ball's position against quads, to determine what
; type of terrain it's landed on.
ball_temp_aa_x_lo   !byte   0
ball_temp_aa_x_hi   !byte   0
ball_temp_aa_z_lo   !byte   0
ball_temp_aa_z_hi   !byte   0
; Copy of sprite position variables.
ball_v_mutex_spr_x_lo   !byte   0
ball_v_mutex_spr_x_hi   !byte   0
ball_v_mutex_spr_y      !byte   0
; Access to these variables controlled by mutex lock.
; Ball position for finding current terrain.
; FIXME: only 'z_lo/hi' are used!!!
ball_mutex_x_lo     !byte   0
ball_mutex_x_hi     !byte   0
ball_mutex_z_lo     !byte   0
ball_mutex_z_hi     !byte   0
ball_mutex_y_lo     !byte   0
ball_mutex_y_hi     !byte   0
; Ball velocity for calculating friction vector.
; Write back result to same location?!
; FIXME: eventually may not need the lo bytes!
ball_v_mutex_vx_lo    !byte   0
ball_v_mutex_vx_hi    !byte   0
ball_v_mutex_vy_lo    !byte   0
ball_v_mutex_vy_hi    !byte   0
ball_v_mutex_vz_lo    !byte   0
ball_v_mutex_vz_hi    !byte   0
; Drag.
ball_v_drag_x_lo        !byte   0
ball_v_drag_x_hi        !byte   0
ball_v_drag_y_lo        !byte   0
ball_v_drag_y_hi        !byte   0
ball_v_drag_z_lo        !byte   0
ball_v_drag_z_hi        !byte   0
ball_v_mutex_drag_x_lo  !byte   0
ball_v_mutex_drag_x_hi  !byte   0
ball_v_mutex_drag_y_lo  !byte   0
ball_v_mutex_drag_y_hi  !byte   0
ball_v_mutex_drag_z_lo  !byte   0
ball_v_mutex_drag_z_hi  !byte   0
; Spin.
ball_v_spin_x_lo    !byte   0
ball_v_spin_x_hi    !byte   0
ball_v_spin_y_lo    !byte   0
ball_v_spin_y_hi    !byte   0
ball_v_spin_z_lo    !byte   0
ball_v_spin_z_hi    !byte   0
ball_v_mutex_spin_x_lo    !byte   0
ball_v_mutex_spin_x_hi    !byte   0
ball_v_mutex_spin_y_lo    !byte   0
ball_v_mutex_spin_y_hi    !byte   0
ball_v_mutex_spin_z_lo    !byte   0
ball_v_mutex_spin_z_hi    !byte   0
; Speed (= a scalar!).
ball_v_current_speed            !byte   0
ball_v_current_speed_squared_lo !byte   0
ball_v_current_speed_squared_hi !byte   0
ball_v_current_ground_speed     !byte   0
; Ball's apparent velocity (unless there's no wind in which case it's no
; different from the ball's actual velocity).
ball_v_apparent_vx_lo       !byte   0
ball_v_apparent_vx_hi       !byte   0
ball_v_apparent_vy_lo       !byte   0
ball_v_apparent_vy_hi       !byte   0
ball_v_apparent_vz_lo       !byte   0
ball_v_apparent_vz_hi       !byte   0
ball_c_PHYSICS_VARIABLES_END

; Ball's position in absolute world coordinates.  Begins at (0,0).
ball_world_x_lo     !byte   0
ball_world_x_hi     !byte   0
ball_world_z_lo     !byte   0
ball_world_z_hi     !byte   0

ball_v_current_state  !byte   0
ball_v_collision_type !byte   0
ball_v_is_in_hole     !byte   0
; NOTE: the mutex lock is seen from the point of view of the raster thread.
; So if it's locked, that means it's not available to the 'full-tilt' thread
; (- in other words, the raster has locked it!).
ball_v_mutex_lock     !byte   0
; NOTE: this will change if ball goes behind flag.
ball_v_hw_spr_num         !byte   ball_c_SW_SPR_NUM
ball_v_shadow_hw_spr_num  !byte   ball_c_SHADOW_SW_SPR_NUM    
ball_v_is_in_trees    !byte   0
ball_v_is_in_water    !byte   0
; Temporary storage for ball's x-position in terms of m/c bitmap.
; Calculated during ball_check_tree_collision, but also needed by
; ball_s_check_trunk_collision. 
ball_v_mc_x     !byte   0
ball_v_mc_y     !byte   0
ball_v_last_mc_x    !byte   0
ball_v_spr_x_parity !byte   0
; Was the ball in front of trunk (if applicable) in last frame?
; If there was no trunk, this value has no application.
ball_v_last_in_front_of_trunk   !byte   0
ball_v_spin_type    !byte   0
ball_v_applying_spin    !byte   0
ball_v_out_of_bounds    !byte   0
ball_v_must_launch  !byte   0
; NOTE: 0 = nothing to do, 1 = do a deflection, -ve = deflected already!
ball_v_must_deflect !byte   0
ball_v_force_target_redraw  !byte   0
ball_v_last_in_front_of_pole    !byte   0

; Tables to determine ball's visual appearance.
ball_distance_z_lo  !byte   $52,$a4,$48,$90,$20,$41
ball_distance_z_hi  !byte   $00,$00,$01,$02,$05,$0a
ball_colors         !byte   WHITE,WHITE,YELLOW,LIGHT_GREEN,LIGHT_BLUE,GREY2

ball_v_current_terrain    !byte   0

ball_v_spin_axis_x_lo   !byte   0
ball_v_spin_axis_x_hi   !byte   0
ball_v_spin_axis_z_lo   !byte   0
ball_v_spin_axis_z_hi   !byte   0

; 'Full-tilt' thread writes to these variables.
ball_v_mutex_vx_lo_on_bounce  !byte   0
ball_v_mutex_vx_hi_on_bounce  !byte   0
ball_v_mutex_vy_lo_on_bounce  !byte   0
ball_v_mutex_vy_hi_on_bounce  !byte   0
ball_v_mutex_vz_lo_on_bounce  !byte   0
ball_v_mutex_vz_hi_on_bounce  !byte   0
; 'Time-critical' thread reads these:
ball_v_vx_lo_on_bounce  !byte   0
ball_v_vx_hi_on_bounce  !byte   0
ball_v_vy_lo_on_bounce  !byte   0
ball_v_vy_hi_on_bounce  !byte   0
ball_v_vz_lo_on_bounce  !byte   0
ball_v_vz_hi_on_bounce  !byte   0

; NOTE: use this variable to record ball velocity sign (on x-axis) BEFORE
; friction and slope have been applied.
ball_v_putting_vx_hi_before !byte   0

!if _DEBUG_APEX_ {
ball_v_apex_set         !byte   0
ball_v_apex_lo          !byte   0
ball_v_apex_hi          !byte   0
} ; !if

ball_v_current_spr_pattern  !byte   0
ball_v_is_distant   !byte   0

ball_v_splash_anim_iter     !byte   0
ball_v_splash_anim_count    !byte   0
; NOTE: if ball BOUNCED into water, use shadow s/w sprite as base for position;
; or if it ROLLED into water, use ball s/w sprite.
ball_v_splash_anim_base_sw_sprite   !byte   0

ball_v_is_drag_invalid_for_roll !byte   0

; Where old = last frame, new = current frame.
; Values are to be interpreted as:
; 0 = 'at', >=1 = 'behind', <0 = 'in front'.
ball_v_z_pos_rel_to_hole_old    !byte   0
ball_v_z_pos_rel_to_hole_new    !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
ball_s_set_at_origin
    lda #ball_c_STATE_STATIONARY
    sta ball_v_current_state
    lda #1
    sta ball_v_applying_spin
    lda #0
    sta ball_v_is_in_hole
    sta ball_v_is_in_water
    sta ball_v_must_launch
    sta ball_v_out_of_bounds
    sta ball_v_is_distant   
    sta ball_v_last_in_front_of_trunk   
    sta ball_v_must_deflect 
    sta ball_v_force_target_redraw  
    sta ball_v_last_in_front_of_pole    
    sta ball_v_is_drag_invalid_for_roll
    ldx #ball_c_SPR_PATTERN_FULL_SIZE    
    jsr ball_s_write_spr_data

    ; Zero out all the physics variables.
    ldx #0
    lda #0
-
    sta ball_x_lo,x
    inx
    cpx #ball_c_PHYSICS_VARIABLES_END-ball_x_lo
    bne -

    lda #ball_c_SW_SPR_NUM 
    sta ball_v_hw_spr_num
    lda #ball_c_SHADOW_SW_SPR_NUM
    sta ball_v_shadow_hw_spr_num
    lda #1
    sta spr_v_hires+ball_c_SW_SPR_NUM
    sta spr_v_hires+ball_c_SHADOW_SW_SPR_NUM

    lda #WHITE
    sta spr_v_color+ball_c_SW_SPR_NUM
    lda #sprd_c_BALL
    sta spr_v_current_ptr+ball_c_SW_SPR_NUM

    lda #0
    sta spr_v_bg_priority+ball_c_SW_SPR_NUM
    sta spr_v_bg_priority+ball_c_SHADOW_SW_SPR_NUM
    ; NOTE: a 'splash' may have expanded this sprite!
    sta spr_v_xxpand+ball_c_SHADOW_SW_SPR_NUM
    sta spr_v_yxpand+ball_c_SHADOW_SW_SPR_NUM

    ldx round_v_current_player    
    lda players_v_terrain,x
    sta ball_v_current_terrain

    sec
    jsr ball_s_draw

    lda #ball_c_IN_FRONT_OF_HOLE
    sta ball_v_z_pos_rel_to_hole_old

    rts
; end sub ball_s_set_at_origin
} ; !zone

; **************************************************

; INPUTS:   set C flag to force drawing, whether or not ball is in flight.
!zone {
ball_s_draw
    bcs +
    lda ball_v_current_state
    beq .dont_draw
    bpl +
.dont_draw
    rts ; EXIT POINT.

+
    ; Projection onto x-axis of screen.
    lda ball_x_lo
    sta CAMERA0
    lda ball_x_hi
    sta CAMERA1
    lda ball_z_lo
    sta CAMERA2
    lda ball_z_hi
    sta CAMERA3
    jsr camera_project_onto_plate_x
    ; FIXME: assume x-position always in bounds for time being...
    lda CAMERA0
    clc
    adc #spr_c_VISIBLE_ALL_L 
    sta spr_v_x_lo+ball_c_SW_SPR_NUM
    lda CAMERA1
    adc #0
    sta spr_v_x_hi+ball_c_SW_SPR_NUM

    ; Projection onto y-axis of screen.
    lda ball_y_lo
    sta P0
    lda ball_y_hi
    sta P1
    lda ball_z_lo
    sta P2
    lda ball_z_hi
    sta P3
    jsr camera_project_onto_plate_y
    ; FIXME: should check for negative (?!) values... ?!
    lda P1
    bpl +
    lda #0
    sta spr_v_y+ball_c_SW_SPR_NUM
    jmp ++
+
    lda P0
    clc
    adc #spr_c_VISIBLE_ALL_T
    sta spr_v_y+ball_c_SW_SPR_NUM
++
    ldy #ball_c_SW_SPR_NUM
    ldx ball_v_hw_spr_num
    jsr spr_s_write_to_vic_ii
    
    jsr ball_s_draw_shadow

    lda #0
    cmp ball_v_force_target_redraw  
    beq .end
    sta ball_v_force_target_redraw  
    jsr target_s_refresh_draw

.end    
    rts
; end sub ball_s_draw
} ; !zone

; **************************************************

!zone {
ball_s_check_depth
    ; Don't need any of this if we're putting...
    lda round_v_must_putt
    +branch_if_true .end

    lda ball_v_is_distant
    +branch_if_true .check_target
    lda ball_z_lo
    cmp #<ball_c_DISTANT_Z
    lda ball_z_hi
    sbc #>ball_c_DISTANT_Z
    +blt_s .check_target
    inc ball_v_is_distant

.check_target
    ; We will now check if the ball should be behind the flag.
    ; First see if the sprite number has already been changed, in which case
    ; there's no need to check anything again.
    lda ball_v_hw_spr_num
    cmp #ball_c_SW_SPR_NUM 
    bne .end
    ; Do target-z - ball-z.
    lda target_z_lo
    cmp ball_z_lo
    lda target_z_hi
    sbc ball_z_hi
    +bge_s .end
    ; Target and ball must swap h/w sprite numbers here!
    jsr ball_s_swap_hw_spr_num
    
.end
    rts
; end sub ball_s_check_depth
} ; !zone

; **************************************************

!zone {
ball_s_set_direction_vector
    jsr maths_s_install_fine_trig

    ldx golfer_v_marker_index
    stx MATHS4
    cpx #golfer_c_MARKER_INITIAL_INDEX
    beq .no_adjust
    lda #<ball_vx_lo
    sta MATHS0
    lda #>ball_vx_lo
    sta MATHS1
    lda #<ball_vz_lo
    sta MATHS2
    lda #>ball_vz_lo
    sta MATHS3
    clc
    jsr maths_s_rotate

.no_adjust
    ; If swing, may need also to rotate spin axis vector...
    lda round_v_must_putt
    +branch_if_true .end
    ; FIXME: first 'valid' offset is 12.  Need to check for this!
    ldx powarc_v_precision_offset 
    jsr ball_s_set_spin_type
    lda ball_l_SPIN_AXIS_X_LO-powarc_c_PRECISION_BEGIN,x 
    sta ball_v_spin_axis_x_lo
    lda ball_l_SPIN_AXIS_X_HI-powarc_c_PRECISION_BEGIN,x 
    sta ball_v_spin_axis_x_hi
    lda ball_l_SPIN_AXIS_Z_LO-powarc_c_PRECISION_BEGIN,x 
    sta ball_v_spin_axis_z_lo
    lda ball_l_SPIN_AXIS_Z_HI-powarc_c_PRECISION_BEGIN,x 
    sta ball_v_spin_axis_z_hi
   
    ; NOTE: trig index in MATHS4 should still be valid.  It would have been
    ; modified in maths_s_rotate only if rotation were cw, which it wasn't.
    ; Still no need to do this rotation if marker was centered.
    lda MATHS4
    cmp #golfer_c_MARKER_INITIAL_INDEX
    beq .end
    lda #<ball_v_spin_axis_x_lo
    sta MATHS0
    lda #>ball_v_spin_axis_x_lo
    sta MATHS1
    lda #<ball_v_spin_axis_z_lo
    sta MATHS2
    lda #>ball_v_spin_axis_z_lo
    sta MATHS3
    clc
    jsr maths_s_rotate

.end
    jsr maths_s_restore_std_trig
    rts
; end sub ball_s_set_direction_vector
} ; !zone

; **************************************************

; Reset ball's position at the origin of the world.
!zone {
ball_reset
    ldx #12
    lda #0
-   sta ball_x_lo,x
    dex
    bpl -
    rts
; end sub ball_reset
} ; !zone

; **************************************************

!zone {
ball_s_calc_new_world_position
    ; The position in ball_x_lo, etc. is relative to the current origin,
    ; which, unless this is the player's first shot for the hole, is probably
    ; rotated from the original coordinate system.  So our first job is to
    ; undo that rotation.
    lda hole_current_rotation_angle
    beq .skip_rotation
    sta MATHS4
    lda #<ball_x_lo
    sta MATHS0
    lda #>ball_x_lo
    sta MATHS1
    lda #<ball_z_lo
    sta MATHS2
    lda #>ball_z_lo
    sta MATHS3
    ; NOTE: remember to SET C flag for CLOCKWISE rotation!!!
    sec
    jsr maths_s_rotate

.skip_rotation
    ; Now add this offset to the world position.
    lda ball_world_x_lo
    clc
    adc ball_x_lo
    sta ball_world_x_lo
    lda ball_world_x_hi
    adc ball_x_hi
    sta ball_world_x_hi
    lda ball_world_z_lo
    clc
    adc ball_z_lo
    sta ball_world_z_lo
    lda ball_world_z_hi
    adc ball_z_hi
    sta ball_world_z_hi

    rts
; end sub ball_s_calc_new_world_position
} ; !zone

; **************************************************

; INPUTS:   max power of selected club, 'golfer_shot_power', and position
;           of crosshair...
!zone {
.POWER_COEFFICIENT = LINE_X0_LO 
.club_selection_index   !byte   0

ball_s_calc_initial_velocity
    ; We will make a local copy of the current club.  In the event that the 
    ; player has requested a 'punch' shot, an offset must be added to the
    ; club 'index'.
    lda clubs_v_current_selection
    ldx golfer_v_punch_selected
    beq +
    clc
    adc #clubs_c_OFFSET_TO_PUNCH_VELOCITIES 
+
    sta .club_selection_index

    ; Clear out vx.  May be adjusted when we calculate the direction
    ; vector.
    lda #0
    sta ball_vx_lo
    sta ball_vx_hi
    
    lda powarc_v_power_offset
    cmp #powarc_c_FULL_POWER_OFFSET
    beq .full_power
    bcc +
    ; In the last part of the arc so reduce power accordingly...
    tax
    lda powarc_l_POWER_REDUCTIONS,x
    sta powarc_v_power_offset
+
    ; Prepare args for multiplication.
    ldx .club_selection_index
    ; NOTE: accumulator holds power setting.
    sta P0
    lda #0
    sta P1
    lda clubs_l_VZ_LO,x
    sta P2
    lda clubs_l_VZ_HI,x
    sta P3
    jsr maths_mul16
    ; Divide this result by 32 and put in ball_vz_lo/hi.
    ldx #5
-
    lsr P6
    ror P5
    ror P4
    dex
    bne -
    lda P4
    sta ball_vz_lo
    lda P5
    sta ball_vz_hi

    ; Now vy.
    ldx .club_selection_index
    lda powarc_v_power_offset
    sta P0
    lda #0
    sta P1
    lda clubs_l_VY_LO,x
    sta P2
    lda clubs_l_VY_HI,x
    sta P3
    jsr maths_mul16
    ldx #5
-
    lsr P6
    ror P5
    ror P4
    dex
    bne -
    lda P4
    sta ball_vy_lo
    lda P5
    sta ball_vy_hi
    jmp .terrain

.full_power
    ldx .club_selection_index
    lda clubs_l_VY_LO,x
    sta ball_vy_lo
    lda clubs_l_VY_HI,x
    sta ball_vy_hi
    lda clubs_l_VZ_LO,x
    sta ball_vz_lo
    lda clubs_l_VZ_HI,x
    sta ball_vz_hi

.terrain
    ; Depending on terrain type, we may reduce the power.
    ldx round_v_current_player
    lda players_v_terrain,x
    cmp #ball_c_TERRAIN_ROUGH        
    beq .reduce_rough
    cmp #ball_c_TERRAIN_BUNKER
    bne .direction  ; Fairway/green so don't reduce power.
    ; In bunker so look up reduction factor based on difficulty.
    ldx shared_v_sand_traps_difficulty
    lda ball_l_BUNKER_POWER_REDUCTION,x
    ; If 0 (= easy setting), no reduction.
    beq .direction
    +skip_2_bytes 
.reduce_rough
    lda #ball_c_ROUGH_POWER_REDUCTION
    jsr ball_s_reduce_power

.direction
    jsr ball_s_set_direction_vector

    ; Initialize particle system here as well.
    lda powarc_v_power_offset
    sta P0
    clc
    jsr partsys_s_init

    rts
; end sub ball_s_calc_initial_velocity
} ; !zone

; **************************************************

; FIXME: this routine should be called once only.
!zone {
ball_s_launch
    lda ball_v_must_launch
    +branch_if_true +
    rts ; EXIT POINT.

+
    lda #ball_c_STATE_IN_FLIGHT
    cmp ball_v_current_state
    beq +
    sta ball_v_current_state

!if _DEBUG_APEX_ {
    lda #0
    sta ball_v_apex_set         
} ; !if

    lda #0
    sta ball_v_is_in_trees
    sta ball_v_mc_x     
    sta ball_v_last_in_front_of_trunk   
    sta ball_v_must_launch

    lda #ball_c_MUTEX_WANT_NEW 
    sta ball_v_mutex_lock 

    jsr ball_s_check_for_scuffed_shot
    jsr partsys_s_start
+
    rts
; end sub ball_s_launch
} ; !zone

; **************************************************

!zone {
ball_s_undo_rotation
    lda hole_current_rotation_angle
    beq .end
    sta MATHS4
    lda #<ball_temp_aa_x_lo
    sta MATHS0
    lda #>ball_temp_aa_x_lo
    sta MATHS1
    lda #<ball_temp_aa_z_lo
    sta MATHS2
    lda #>ball_temp_aa_z_lo
    sta MATHS3
    ; NOTE: remember to SET C flag for CLOCKWISE rotation!!!
    sec
    jsr maths_s_rotate

.end
    rts
; end sub ball_s_undo_rotation
} ; !zone

; **************************************************

!zone {
.VX_SUPER_HI    = MATHS0
.VZ_SUPER_HI    = MATHS1

ball_update_on_xz_plane
    ; Sign-extend vx and vz so they're valid 24-bit numbers.
    lda #0
    sta .VX_SUPER_HI
    sta .VZ_SUPER_HI
    lda ball_vx_hi
    bpl +
    dec .VX_SUPER_HI
+
    lda ball_vz_hi
    bpl +
    dec .VZ_SUPER_HI
+

    ; Two 24-bit additions.
    ; X:
    lda ball_x_fraction
    clc
    adc ball_vx_lo
    sta ball_x_fraction
    lda ball_x_lo
    adc ball_vx_hi
    sta ball_x_lo
    lda ball_x_hi
    adc .VX_SUPER_HI
    sta ball_x_hi
    ; Z:
    lda ball_z_fraction
    clc
    adc ball_vz_lo
    sta ball_z_fraction
    lda ball_z_lo
    adc ball_vz_hi
    sta ball_z_lo
    lda ball_z_hi
    adc .VZ_SUPER_HI
    sta ball_z_hi

    rts
; end sub ball_update_on_xz_plane
} ; !zone

; **************************************************

!zone {
.addend_hi  !byte   0

ball_s_update_in_flight
    ; Store previous frame's column.
    lda ball_v_mc_x     
    sta ball_v_last_mc_x     

    ; Does a tree-collision need handling?
    lda ball_v_is_in_trees
    cmp #BALL_TREE_COLLISION_STATE_ACTIVE
    bne +
    jsr ball_s_handle_tree_collision
+
    ; Subtract gravity from vy.
    lda ball_vy_lo
    sec
    sbc #ball_c_GRAVITY
    sta ball_vy_lo
    lda ball_vy_hi
    sbc #0
    sta ball_vy_hi

!if _DEBUG_APEX_ {
    lda ball_v_apex_set
    bne +
    bit ball_vy_hi
    bpl +
    inc ball_v_apex_set
    lda ball_y_lo
    sta ball_v_apex_lo
    lda ball_y_hi
    sta ball_v_apex_hi
+   
} ; !if

    ; Now adjust the ball's position.
    jsr ball_s_apply_drag
    jsr ball_s_apply_spin
    jsr ball_update_on_xz_plane

    ldx #0
    bit ball_vy_hi
    bpl +
    dex
+   stx .addend_hi

    lda ball_y_fraction
    clc
    adc ball_vy_lo
    sta ball_y_fraction
    lda ball_y_lo
    adc ball_vy_hi
    sta ball_y_lo
    lda ball_y_hi
    adc .addend_hi
    sta ball_y_hi

    ; Enter 'bouncing' state if ball_y has become negative.
    lda ball_y_hi
    bpl .end
    lda #0
    sta ball_y_lo
    sta ball_y_hi
    jsr ball_s_execute_bounce

.end
    rts

; end sub ball_s_update_in_flight
} ; !zone

; **************************************************

!zone {
.quad_iter  !byte   0

ball_s_check_collision
    lda #0
    sta .quad_iter
.loop_top
    ; Index into X - must multiply by 4 to get correct offset.
    asl
    asl
    tax

    ; Our first test is the ball's z-position against the max-z of the 
    ; current quad.  Quad vertices are recorded ccw starting at the 
    ; top-left.  So for this first test we use vertex #0.
    lda ball_temp_aa_z_lo
    sec 
    sbc quads_aa_z_lo,x
    lda ball_temp_aa_z_hi
    sbc quads_aa_z_hi,x
    +bge_s .rough
    ; Now check if the ball's z is > quad's min-z.  If so, we can go on to
    ; check the horizontal (=x) axis.
    lda ball_temp_aa_z_lo
    sec
    sbc quads_aa_z_lo+1,x
    lda ball_temp_aa_z_hi
    sbc quads_aa_z_hi+1,x
    +bge_s .check_x_axis
    
.next_quad
    ; No collision here, so move on to the next quad if appropriate.
    inc .quad_iter
    lda .quad_iter
    cmp quads_n
    beq .rough  ; All quads checked and no collision.
    bne .loop_top

.check_x_axis
    lda ball_temp_aa_x_lo
    sec
    sbc quads_aa_x_lo,x
    lda ball_temp_aa_x_hi
    sbc quads_aa_x_hi,x
    +blt_s .next_quad
    lda ball_temp_aa_x_lo
    sec
    sbc quads_aa_x_lo+2,x
    lda ball_temp_aa_x_hi
    sbc quads_aa_x_hi+2,x
    +bge_s .next_quad

    ; We have detected a collision with the current quad.
    ldx .quad_iter
    ; Is this a triangle?
    lda quads_triangle_indices,x  
    bmi +
    tay
    jsr ball_s_check_triangle
    bcc .rough
    ldx .quad_iter

+
    lda quads_type,x
    jmp +

.rough
    lda #ball_c_TERRAIN_ROUGH
+   
    sta ball_v_current_terrain
    rts
; end sub ball_s_check_collision
} ; !zone

; **************************************************

!zone {
ball_s_execute_bounce
    lda ball_v_current_terrain
    cmp #ball_c_TERRAIN_WATER
    bne .not_water
    lda #ball_c_SHADOW_SW_SPR_NUM
    sta ball_v_splash_anim_base_sw_sprite
    jsr ball_s_handle_water_splash
    rts ; EXIT POINT.

.not_water
    ldy #sfx_c_BOUNCE    
    jsr snd_s_init_sfx

    ldx #5
-
    lda ball_v_vx_lo_on_bounce,x
    sta ball_vx_lo,x
    dex
    bpl -

    lda ball_v_vy_lo_on_bounce
    cmp ball_c_MIN_VY_ON_BOUNCE
    lda ball_v_vy_hi_on_bounce
    sbc #0
    bcc .no_bounce

    lda #ball_c_STATE_IN_FLIGHT
    sta ball_v_current_state
    rts ; EXIT POINT.

.no_bounce
    lda #0
    sta ball_vy_lo
    sta ball_vy_hi
    lda #ball_c_STATE_ROLLING
    sta ball_v_current_state
    sta ball_v_is_drag_invalid_for_roll
    rts

; end sub ball_s_execute_bounce
} ; !zone

; **************************************************

!zone {
ball_s_launch_putt
    lda #ball_c_STATE_ROLLING
    sta ball_v_current_state
    lda #sprd_c_PARTICLES_SHOT               
    sta spr_v_current_ptr+GOLFER_POS_MARKER_SPR_NUM 

    ; NOTE: when ball is rolling, reuse the 'drag' vector variables.  We need
    ; only the x and z components!
    ; FIXME: initially 0?!
    lda #0
    ldx #11
-
    sta ball_v_drag_x_lo,x
    dex
    bpl -

    lda #ball_c_MUTEX_WANT_NEW 
    sta ball_v_mutex_lock 

    ldy #sfx_c_BALL_CLUB
    jsr snd_s_init_sfx

    rts
; end sub ball_s_launch_putt
} ; !zone

; **************************************************

!zone {
ball_s_update_roll
    lda ball_v_current_terrain
    cmp #ball_c_TERRAIN_WATER
    bne +
    lda #ball_c_SW_SPR_NUM
    sta ball_v_splash_anim_base_sw_sprite
    jsr ball_s_handle_water_splash
    rts ; EXIT POINT.

+
    ; Apply slope if putting.
    lda round_v_must_putt
    +branch_if_false +

    lda ball_vx_hi
    sta ball_v_putting_vx_hi_before  
    jsr winslp_s_apply

+
    lda ball_v_is_drag_invalid_for_roll
    +branch_if_true +
    +sbc16 ball_vx_lo,ball_v_drag_x_lo
    +sbc16 ball_vz_lo,ball_v_drag_z_lo

+
    jsr ball_s_check_stop_roll
    bcc .move
    ; Ball must stop rolling.
    jsr ball_s_end_shot
    rts ; EXIT POINT.

.move
    jsr ball_update_on_xz_plane
    jsr ball_s_check_target
    bcc .end
    jsr ball_s_check_collision_type

.end
    rts
; end sub ball_s_update_roll
} ; !zone

; **************************************************

; OUTPUTS:  C flag clear = carry on rolling; C flag set = stop.
!zone {
.MIN_VZ = LINE_Y0_LO 

ball_s_check_stop_roll
    ; FIXME: a bit messy.
    lda ball_c_ROLLING_MIN_VZ
    sta .MIN_VZ
    lda round_v_must_putt
    +branch_if_false .check_vz
    lda #ball_c_PUTTING_MIN_VZ
    sta .MIN_VZ

    lda ball_vx_hi
    eor ball_v_putting_vx_hi_before
    bpl .check_vz
    ; vx sign has changed.  If ball's now going down the slope it's OK to
    ; continue, otherwise it must stop.
    ; FIXME: what if there's no slope?!
    lda ball_vx_hi
    eor winslp_v_final_vx_hi    
    bmi .stop_roll

.check_vz
    lda ball_vz_hi
    bmi .stop_roll
    bne .keep_rolling
    lda ball_vz_lo
    cmp .MIN_VZ
    bcs .keep_rolling

.stop_roll
    sec
    rts ; EXIT POINT.
    
.keep_rolling
    clc
    rts
; end sub ball_s_check_stop_roll
} ; !zone

; **************************************************

; OUTPUT:   C flag clear if no collision.  Otherwise check value of
;           ball_collision_type to see if putt was successful or if ball
;           was moving too fast at the time.
!zone {
ball_s_check_target
    ; TODO: we will handle ball-target collisions differently if the ball was
    ; hit with something other than the putter.
    lda round_v_must_putt
    +branch_if_false .exit_no_collision

    jsr target_s_update_hole_color

    ; If the ball was behind the hole in the last frame, there's nothing for 
    ; us to do here.  NOTE: >=1 = 'behind'.  Assume signed 8-bit value with no
    ; possibility of overflow.
    lda ball_v_z_pos_rel_to_hole_old
    beq +
    bmi +
.exit_no_collision
    clc
    rts ; EXIT POINT.

+
    ; Find out where the ball is relative to the hole on the z-axis for 
    ; this frame and record result.
    ; Result of subtraction is decremented by 1 so that 3 scenarios are
    ; easily identifiable with branch instructions:
    ; 0 = 'at', <0 = 'in front', >=1 = 'behind'
    lda spr_v_y+target_c_SW_SPR_NUM
    clc
    adc #20
    sec
    sbc spr_v_y+ball_c_SW_SPR_NUM
    tax
    dex
    stx ball_v_z_pos_rel_to_hole_new
    bpl +
    ; Negative, so we're still in front of the hole.
    ; (This handles scenario #5: last='in front' && current='in front')
    stx ball_v_z_pos_rel_to_hole_old
    bmi .exit_no_collision

+
    ; Is the ball 'on target' with reference to the horizontal axis?
    lda spr_v_x_hi+ball_c_SW_SPR_NUM
    bne .exit_no_collision
    lda spr_v_x_lo+target_c_SW_SPR_NUM
    sec
    sbc spr_v_x_lo+ball_c_SW_SPR_NUM
    clc
    adc #3
    cmp #5
    bcc +
    ; No collision.  But let's record current z-pos for the next frame.
    ; FIXME: if 'at' the hole, putt should now be impossible. (?!)
    lda ball_v_z_pos_rel_to_hole_new
    bne .not_at
    lda #ball_c_BEHIND_HOLE      
.not_at
    sta ball_v_z_pos_rel_to_hole_old
    jmp .exit_no_collision

+
    ; We now have the ball's position (relative to z-axis) for the previous and
    ; current frames, and we know that the ball is correctly aligned on the 
    ; x-axis.  So here we just need to 'bump' or 'putt', depending on ball
    ; speed.
    ; Also, set old pos to 'behind' so we won't check target again.
    lda #ball_c_BEHIND_HOLE
    sta ball_v_z_pos_rel_to_hole_old
    
    lda ball_v_current_speed
    cmp #ball_c_MAX_PUTTING_SPEED+1
    bcs .bump
    lda #ball_c_PUTT_SUCCESSFUL
    +skip_2_bytes
.bump
    lda #ball_c_PUTT_TOO_FAST
    sta ball_v_collision_type
    sec
    rts

; end sub ball_s_check_target
} ; !zone

; **************************************************

!zone {
ball_s_check_collision_type
    lda ball_v_collision_type
    cmp #ball_c_PUTT_SUCCESSFUL
    bne .bump

    ; Successful putt!
    ldy #sfx_c_BALL_CUP
    jsr snd_s_init_sfx

    ldx ball_v_hw_spr_num
    +spr_m_disable
    +set ball_v_is_in_hole
    jsr ball_s_end_shot
    rts ; EXIT POINT.

.bump
    ; Ball moving too fast so 'bump' it across the hole...
    ; Give the ball a small (fixed) amount of vertical velocity.
    lda #ball_c_BUMP_VY_LO
    sta ball_vy_lo
    lda #ball_c_BUMP_VY_HI
    sta ball_vy_hi
    lda #ball_c_STATE_IN_FLIGHT
    sta ball_v_current_state
    ; Slow ball down a bit.
    lda #ball_c_SLOW_DOWN_ON_BUMP_LO 
    sta MATHS0
    jsr ball_s_reduce_velocity_xz

    ldy #sfx_c_BOUNCE
    jsr snd_s_init_sfx

    rts
; end sub ball_s_check_collision_type
} ; !zone

; **************************************************

!zone {
ball_s_handle_water_splash
    ; TODO: particles (if close enough) and sound effect...
    +clr ball_v_current_state

    ldx ball_v_hw_spr_num
    stx ball_v_is_in_water
    ldx ball_v_shadow_hw_spr_num
    +spr_m_disable

    ; Not enough raster time to call this here!  Must wait for animation
    ; to finish.
    ldy #sfx_c_SPLASH
    jsr snd_s_init_sfx

    jsr ball_s_init_water_splash_animation

    rts
; end sub ball_s_handle_water_splash
} ; !zone

; **************************************************

; For your convenience...
!zone {
ball_s_end_shot
    +clr ball_v_current_state

    ; If we don't lock the fire button here, and player is holding down fire,
    ; the second the ball stops game will go to the next shot and you won't 
    ; see the distance the ball was hit.
    ldx joy_v_current_port  
    +joy_m_lock_fire 

    rts
; end sub ball_s_end_shot
} ; !zone

; **************************************************

!zone {
ball_s_update_time_critical
    ; Nothing to do if the ball is stationary.
    lda ball_v_current_state  
    bne +
    rts ; EXIT POINT.

+
    cmp #ball_c_STATE_SPLASHING
    bne +
    jsr ball_s_update_splash_animation
    rts ; EXIT POINT.

+
    ; Does main thread want latest position & velocity?
    lda ball_v_mutex_lock
    beq .do_updates

    ; FIXME: why two copies of some variables?!
    lda ball_x_lo
    sta ball_temp_aa_x_lo
    lda ball_x_hi
    sta ball_temp_aa_x_hi
    lda ball_z_lo
    sta ball_mutex_z_lo
    sta ball_temp_aa_z_lo
    lda ball_z_hi
    sta ball_mutex_z_hi
    sta ball_temp_aa_z_hi
    lda ball_y_lo
    sta ball_mutex_y_lo
    lda ball_y_hi
    sta ball_mutex_y_hi

    lda ball_vx_lo
    sta ball_v_mutex_vx_lo
    lda ball_vx_hi
    sta ball_v_mutex_vx_hi
    lda ball_vz_lo
    sta ball_v_mutex_vz_lo
    lda ball_vz_hi
    sta ball_v_mutex_vz_hi
    lda ball_vy_lo
    sta ball_v_mutex_vy_lo
    lda ball_vy_hi
    sta ball_v_mutex_vy_hi

    lda spr_v_x_lo+ball_c_SW_SPR_NUM
    sta ball_v_mutex_spr_x_lo
    lda spr_v_x_hi+ball_c_SW_SPR_NUM
    sta ball_v_mutex_spr_x_hi
    lda spr_v_y+ball_c_SW_SPR_NUM
    sta ball_v_mutex_spr_y

    ; Collect spin data.
    ; FIXME: collect all these variables together so they can be copied over in 
    ; one go?!
    ldx #5
-
    lda ball_v_mutex_drag_x_lo,x
    sta ball_v_drag_x_lo,x
    lda ball_v_mutex_spin_x_lo,x
    sta ball_v_spin_x_lo,x
    lda ball_v_mutex_vx_lo_on_bounce,x
    sta ball_v_vx_lo_on_bounce,x
    dex
    bpl -

    +clr ball_v_mutex_lock

.do_updates
    ; TODO: if ball is out of bounds at this point, should listen out for 
    ; fire button and short-circuit ball flight if detected...
    lda ball_v_out_of_bounds
    +branch_if_false .check_deflect
    ldx joy_v_current_port
    +joy_m_is_fire
    bne .unlock_fire
    +joy_m_is_locked_fire
    bne .check_deflect
    +joy_m_lock_fire
    jsr ball_s_end_shot
    ; NOTE: msg should never be rejected!
    ldx #round_c_MSG_CUT_SHORT
    jsr round_s_record_msg
    rts ; EXIT POINT.

.unlock_fire
    +joy_m_release_fire

.check_deflect
    lda ball_v_must_deflect 
    cmp #1
    bne +
    jsr ball_s_deflect

+
    ; Actual updates to the ball depending on its state.
    lda ball_v_current_state  
    cmp #ball_c_STATE_IN_FLIGHT
    beq .in_flight
    cmp #ball_c_STATE_ROLLING
    ; FIXME: so what other states could the ball be in?!
    bne .end

    ; So ball must be rolling...
    jsr ball_s_update_roll
    rts ; EXIT POINT.

.in_flight
    jsr ball_s_update_in_flight

.end
    rts
; end sub ball_s_update_time_critical
} ; !zone

; **************************************************

!zone {
ball_s_update_full_tilt
    lda ball_v_current_state
    beq .nothing_to_do
    bpl +
.nothing_to_do
    ; Nothing to do if ball is stationary or running splash animation.
    rts ; EXIT POINT.
    
+
    cmp #ball_c_STATE_MUST_INIT
    bne +
    jsr ball_s_calc_initial_velocity
    lda #ball_c_STATE_STATIONARY
    sta ball_v_current_state
    rts ; EXIT POINT.

+
    ; Nothing to do if mutex is locked (because then we can't read it).
    lda ball_v_mutex_lock
    beq +
    rts ; EXIT POINT.

+
    jsr ball_s_calc_current_row_and_column

    ; Calculate new vectors for air drag and spin.  Should find ball's speed
    ; (and speed^2) only once per routine call and then store for future use.
    lda ball_v_mutex_vx_hi
    sta P0
    lda ball_v_mutex_vy_hi
    sta P1
    lda ball_v_mutex_vz_hi
    sta P2
    jsr pythag_s_calc_magnitude
    stx ball_v_current_speed
    lda MATHS0
    sta ball_v_current_speed_squared_lo
    lda MATHS1
    sta ball_v_current_speed_squared_hi

    lda ball_v_current_state
    cmp #ball_c_STATE_ROLLING
    beq .rolling

    ; So the ball is in flight...
    jsr ball_s_calc_air_drag
    lda ball_v_applying_spin
    +branch_if_false +
    jsr ball_s_calc_spin
+
    jsr ball_s_precalculate_bounce

    jsr ball_s_check_tree_collision
    jsr ball_s_check_trunk_collision
    jmp .terrain_check

.rolling
    jsr ball_s_update_rolling_friction
    jsr ball_s_check_trunk_collision

.terrain_check
    jsr ball_s_check_depth
    jsr ball_s_check_pole_collision
    ; FIXME: for some reason, this must come after the other routines (at least
    ; when ball is in flight).
    jsr ball_s_undo_rotation
    jsr ball_s_check_collision
    jsr ball_s_check_bounds

    ; TODO: could update overhead ball position here!
    lda round_v_must_putt
    +branch_if_true +
    jsr ball_s_update_overhead_ball
+

    ; Lock the mutex.
    lda #ball_c_MUTEX_WANT_NEW
    sta ball_v_mutex_lock

    rts
; end sub ball_s_update_full_tilt
} ; !zone

; **************************************************

!zone {
ball_s_check_tree_collision
    ; Assume to begin with that the ball is in front of any foliage.
    lda #0
    sta spr_v_bg_priority+ball_c_SW_SPR_NUM

    ; Think of the screen in its text-mode format: 40*25 cells.
    ; Where's the ball sprite?
    lda ball_v_mc_y
    lsr
    lsr
    lsr
    cmp #trees_c_COLLISION_MATRIX_TOP_ROW
    bcs +
    rts ; EXIT POINT - no collision.
+
    cmp #trees_c_COLLISION_MATRIX_BOTTOM_ROW +1
    bcc +
    rts ; EXIT POINT - no collision.
+
    sta MATHS0

    ; X-position is a 9-bit value, so make sure we get the m.s. bit out
    ; of the high byte first.
    ; BUG: sprite number for ball isn't always ball_c_SW_SPR_NUM!!!
    lda ball_v_mc_x
    lsr 
    lsr 
    sta MATHS1

    ; BUG: FIXME: check value of MATHS1 is valid!

    ; Find out if there's some tree foliage in this 'cell' and what its 
    ; z-position is.
    ; Addresses of collision matrix into P0-P1 (low byte) and
    ; P2-P3 (high byte).
    ; NOTE: remember to subtract offset from row number!
    lda MATHS0
    sec
    sbc #trees_c_COLLISION_MATRIX_TOP_ROW
    tay
    lda trees_l_CMLO_ADDR_LO,y
    sta P0
    lda trees_l_CMLO_ADDR_HI,y
    sta P1
    lda trees_l_CMHI_ADDR_LO,y
    sta P2
    lda trees_l_CMHI_ADDR_HI,y
    sta P3

    ; Now column-offset into Y.
    ldy MATHS1
    ; Put result of subtraction into MATHS2-MATHS3.
    ; Quick check for 0 (= no foliage).
    lda (P0),y
    ora (P2),y
    beq .no_collision

    lda (P0),y
    sec
    sbc ball_mutex_z_lo
    sta MATHS2
    lda (P2),y
    sbc ball_mutex_z_hi
    sta MATHS3

    ; First let's see if the ball is behind or in front of the foliage.
    ; This determines value of sprite-background priority.
    +bge_s .check_sign
    ; Ball is behind foliage.
    inc spr_v_bg_priority+ball_c_SW_SPR_NUM

.check_sign
    ; Change sign if negative.
    lda MATHS3
    bpl +
    lda #<MATHS2
    sta P0
    lda #>MATHS2
    sta P1
    jsr maths_adjust_vec_signs
+
    lda MATHS3
    bne .no_collision
    lda MATHS2
    cmp trees_v_depth_in_feet
    bcs .no_collision

    ; TODO: handle collision.
    lda ball_v_is_in_trees
    bne .no_collision
    inc ball_v_is_in_trees

.no_collision
    rts
; end sub ball_s_check_tree_collision
} ; !zone

; **************************************************

!zone {
ball_s_draw_shadow
    lda ball_v_current_state
    cmp #ball_c_STATE_IN_FLIGHT
    beq .in_flight
    cmp #ball_c_STATE_ROLLING
    beq .rolling
    rts ; EXIT POINT - nothing to do if stationary.

.rolling
    lda #0
    sta ball_c_SHADOW_SPR_ADDRESS 
    sta ball_c_SHADOW_SPR_ADDRESS+3
    jmp .do_draw

.in_flight
    ; Project ball's position onto y-axis at ball-y=0.
    lda #0
    sta P0
    sta P1
    lda ball_z_lo
    sta P2
    lda ball_z_hi
    sta P3
    jsr camera_project_onto_plate_y
    ; Result is in P0.
    lda P0
    clc
    adc #spr_c_VISIBLE_ALL_T
    sta spr_v_y+ball_c_SHADOW_SW_SPR_NUM 
    lda spr_v_x_lo+ball_c_SW_SPR_NUM 
    sta spr_v_x_lo+ball_c_SHADOW_SW_SPR_NUM 
    lda spr_v_x_hi+ball_c_SW_SPR_NUM 
    sta spr_v_x_hi+ball_c_SHADOW_SW_SPR_NUM 
    ; Enable sprite, set data ptr & color.
    lda #sprd_c_BALL_SHADOW                   
    sta spr_v_current_ptr+ball_c_SHADOW_SW_SPR_NUM

    ; Find the correct color.
    ldx #0
-
    lda ball_y_lo
    sec
    sbc BALL_SHADOW_HEIGHTS_LO,x
    lda ball_y_hi
    sbc BALL_SHADOW_HEIGHTS_HI,x
    +blt_s .found
    inx
    cpx #(ball_c_NUM_SHADOW_COLORS-1)
    bne -
.found

    lda BALL_SHADOW_COLORS,x
    sta spr_v_color+ball_c_SHADOW_SW_SPR_NUM

.do_draw
    ldy #ball_c_SHADOW_SW_SPR_NUM
    ldx ball_v_shadow_hw_spr_num
    jsr spr_s_write_to_vic_ii

    rts
; end sub ball_s_draw_shadow
} ; !zone

; **************************************************

!zone {
ball_s_swap_hw_spr_num
    lda #ball_c_SW_SPR_NUM
    sta target_hw_spr_num
    inc ball_v_hw_spr_num
    inc ball_v_shadow_hw_spr_num

    ; NOTE: don't redraw target straight away because we don't know where the
    ; raster beam is!
    lda #1
    sta ball_v_force_target_redraw  

    rts
; end sub ball_s_swap_hw_spr_num
} ; !zone

; **************************************************

; INPUTS:   MATHS0 = fraction of 256 by which to multiply.
!zone {
.vx_neg !byte   0

ball_s_reduce_velocity_xz
    ; Negate vx if necessary and record the fact.
    lda ball_vx_hi
    sta .vx_neg
    bpl +
    lda #<ball_vx_lo
    sta P0
    lda #>ball_vx_lo
    sta P1
    jsr maths_adjust_vec_signs
+

    lda ball_vx_lo
    sta P0
    lda ball_vx_hi
    sta P1
    lda MATHS0
    sta P2
    lda #0
    sta P3
    jsr maths_mul16 

    ; Result in P4-P7.  To divide by 256, take P5-P6 as low/high byte.
    lda P5
    sta ball_vx_lo
    lda P6
    sta ball_vx_hi

    ; Now vz.
    lda ball_vz_lo
    sta P0
    lda ball_vz_hi
    sta P1
    lda MATHS0
    sta P2
    lda #0
    sta P3
    jsr maths_mul16 
    lda P5
    sta ball_vz_lo
    lda P6
    sta ball_vz_hi

    ; Correct vx sign if necessary.
    lda .vx_neg
    bpl +
    lda #<ball_vx_lo
    sta P0
    lda #>ball_vx_lo
    sta P1
    jsr maths_adjust_vec_signs
+
    
    rts
; end sub ball_s_reduce_velocity_xz
} ; !zone

; **************************************************

!zone {
ball_s_handle_tree_collision
    lda #BALL_TREE_COLLISION_STATE_HANDLED   
    sta ball_v_is_in_trees

    ; Ball loses 3/4 of its horizontal velocity and all lift.
    lda #64
    sta MATHS0
    jsr ball_s_reduce_velocity_xz
    lda ball_vy_hi
    bmi +
    lda #0
    sta ball_vy_hi
+

    rts
; end sub ball_s_handle_tree_collision
} ; !zone

; **************************************************

; INPUTS:   A = reduction factor (fraction of 256).
!zone {
ball_s_reduce_power
    ; Ready for call to maths_mul16:
    sta P2

    ; FIXME: what if vy (or vz!) are negative?!
    ; First, vy.
    lda ball_vy_lo
    sta P0
    lda ball_vy_hi
    sta P1
    lda #0
    sta P3
    jsr maths_mul16
    ; Result in P4-P7.  We must divide by 256, so take P5-P6 as low/high
    ; bytes respectively.
    lda P5
    sta ball_vy_lo
    lda P6
    sta ball_vy_hi

    ; Now vz.
    lda ball_vz_lo
    sta P0
    lda ball_vz_hi
    sta P1
    jsr maths_mul16
    lda P5
    sta ball_vz_lo
    lda P6
    sta ball_vz_hi

    rts
; end sub ball_s_reduce_power
} ; !zone

; **************************************************

!zone {
ball_s_check_for_scuffed_shot
    ; If spin not set, shot will always be scuffed.
    lda golfer_v_shot_spin_set
    beq .scuff

    ; FIXME: provide easy access to current terrain for each shot?!
    ldx round_v_current_player
    lda players_v_terrain,x
    cmp #ball_c_TERRAIN_BUNKER
    beq .bunker

    ; So fairway or rough.
    lda powarc_v_precision_offset
    cmp #powarc_c_PRECISION_BEGIN
    bcc .scuff
    cmp #powarc_c_PRECISION_END+1
    bcs .scuff
    rts ; EXIT POINT.

.bunker
    lda powarc_v_precision_offset 
    cmp #powarc_c_PRECISION_BUNKER_BEGIN
    bcc .scuff
    cmp #powarc_c_PRECISION_BUNKER_END+1
    bcc .end

.scuff
    ; vx may be positive or negative...
    jsr rand_s_get
    bmi .neg_vx
    lda #BALL_SCUFFED_SHOT_VX_LO 
    sta ball_vx_lo
    lda #BALL_SCUFFED_SHOT_VX_HI 
    sta ball_vx_hi
    jmp +
.neg_vx
    lda #(-BALL_SCUFFED_SHOT_VX_LO)
    sta ball_vx_lo
    lda #$ff
    sta ball_vx_hi
+
    lda #BALL_SCUFFED_SHOT_VY_LO 
    sta ball_vy_lo
    lda #BALL_SCUFFED_SHOT_VY_HI 
    sta ball_vy_hi
    lda #BALL_SCUFFED_SHOT_VZ_LO 
    sta ball_vz_lo
    lda #BALL_SCUFFED_SHOT_VZ_HI 
    sta ball_vz_hi
    
.end
    rts
; end sub ball_s_check_for_scuffed_shot
} ; !zone

; **************************************************

; INPUTS:   X = quad number, Y = triangle index.
!zone {
ball_s_check_triangle
    ; NOTE: REMEMBER to multiply by 4 to get correct index into arrays!!!
    txa
    asl
    asl
    tax

    ; If this routine is called, ball is definitely over a 'triangle' quad.
    ; We're going to evaluate the equation z=mx+c for ball's x (relative to
    ; bottom-left corner of this quad).
    lda ball_temp_aa_x_lo
    sec
    sbc quads_aa_x_lo+1,x
    sta BALL_TRI_X_LO 
    lda ball_temp_aa_x_hi
    sbc quads_aa_x_hi+1,x
    sta BALL_TRI_X_HI 
    lda ball_temp_aa_z_lo
    sec
    sbc quads_aa_z_lo+1,x
    sta BALL_TRI_Z_LO 
    lda ball_temp_aa_z_hi
    sbc quads_aa_z_hi+1,x
    sta BALL_TRI_Z_HI 

    ; Calculate the product mx (for ball's x) - 16-bit multiplication followed
    ; by division by 256 (i.e. lose l.s. byte).
    lda quads_triangles_slope_lo,y
    sta P0
    lda quads_triangles_slope_hi,y
    sta P1
    lda BALL_TRI_X_LO
    sta P2
    lda BALL_TRI_X_HI
    sta P3
    jsr maths_mul16
    ; Result is in P4-P7.  We'll assume a 16-bit result (?!) so take P5-P6 as
    ; final product.
    
    ; If z-intercept is 0, the calculation's done.  If not, we must SUBTRACT
    ; this product from z-intercept.
    lda quads_triangles_z_intercept_lo,y
    ora quads_triangles_z_intercept_hi,y
    beq .calculation_done

    lda quads_triangles_z_intercept_lo,y
    sec
    sbc P5
    sta P5
    lda quads_triangles_z_intercept_hi,y
    sbc P6
    sta P6

.calculation_done
    ; Result of z=mx+c is in P5-P6.  If vertex-to-omit was 0 or 3, ball's z
    ; should be <= this for a collision; if 1 or 2, should be >=...
    lda quads_triangles_vertex_to_omit,y
    beq .check_if_below
    cmp #3
    beq .check_if_below

    ; Should be above (i.e. >=).
    lda BALL_TRI_Z_LO
    sec
    sbc P5
    lda BALL_TRI_Z_HI
    sbc P6
    +bge_s .collision

.no_collision
    clc
    rts

.check_if_below
    lda P5
    sec
    sbc BALL_TRI_Z_LO
    lda P6
    sbc BALL_TRI_Z_HI
    +blt_s .no_collision

.collision
    sec
    rts
; end sub ball_s_check_triangle
} ; !zone

; **************************************************

!zone {
ball_s_apply_drag
    ldx #4

.loop_top
    ; Put drag for each component in MATHS0-MATHS1.  Bail out early if drag
    ; is zero.
    ; BUG: wasn't extending sign into high byte!
    ; NOTE: high byte 0 by default (- decrement to $ff if drag component is
    ; negative).
    lda #0
    sta MATHS1
    lda ball_v_drag_x_hi,x
    beq .next
    sta MATHS0
    bpl +
    dec MATHS1
+

    ; BUG: following is wrong (- subtraction should just work with neg. value).
    ; If corresponding velocity component is negative, make drag negative as
    ; well (- as we're doing a subtraction).  Remember that high byte must
    ; become $ff! (- because 16-bit arithmetic).

+
    ; Now do the subtraction.
    lda ball_vx_lo,x
    sec
    sbc MATHS0
    sta ball_vx_lo,x
    lda ball_vx_hi,x
    sbc MATHS1
    sta ball_vx_hi,x

.next
    dex
    dex
    bpl .loop_top

    rts
; end sub ball_s_apply_drag
} ; !zone

; **************************************************

!zone {
.COLUMN1 = 1 
.COLUMN2 = 2
.COLUMN1_MASK = 255-.COLUMN1
.COLUMN2_MASK = 255-.COLUMN2

ball_s_check_trunk_collision
    lda #0
    sta MATHS1
    ; STEP 1: are there tree trunks in these two columns?
    ; NOTE: depth_z_hi will be negative if there's no trunk here.
    ldx ball_v_mc_x
    ldy ttrunks_v_depths_z_hi,x
    bmi +
    ora #.COLUMN1
+
    ldy ttrunks_v_depths_z_hi+1,x
    bmi +
    ora #.COLUMN2
+
    sta MATHS0
    ; If there are no trunks in the current columns we can skip
    ; steps 2 and 3 altogether.
    beq .step4

+
    ; STEP 2: is the ball behind a tree trunk?  Check depths against ball's
    ; position on z-axis.
    lda MATHS0
    sta MATHS1
    and #.COLUMN1
    beq .column2
    lda ball_mutex_z_lo
    cmp ttrunks_v_depths_z_lo,x   
    lda ball_mutex_z_hi
    sbc ttrunks_v_depths_z_hi,x   
    +bge_s .column2
    ; Column #1 is clear.
    lda MATHS1
    and #.COLUMN1_MASK
    sta MATHS1
.column2
    lda MATHS1
    and #.COLUMN2
    beq .step3
    lda ball_mutex_z_lo
    cmp ttrunks_v_depths_z_lo+1,x   
    lda ball_mutex_z_hi
    sbc ttrunks_v_depths_z_hi+1,x   
    +bge_s .step3
    ; Column #2 is clear.
    lda MATHS1
    and #.COLUMN2_MASK
    sta MATHS1

.step3
    ; STEP 3: record whether the ball is behind the trunk on the y-axis.
    ; NOTE: a 'y0' value of 0 means there's no trunk here.
    ; trunk - ball will be <=0 if collision.
    lda ttrunks_v_y0,x
    beq +
    cmp ball_v_mc_y
    beq .collision_y
    bcc .collision_y
+
    lda ttrunks_v_y0+1,x
    beq .step4
    cmp ball_v_mc_y
    beq .collision_y
    bcs .step4
.collision_y
    lda MATHS1
    ora #$80
    sta MATHS1

.step4
    ; STEP 4: select the correct sprite data pattern depending on what's 
    ; in MATHS0 and spr-x parity.
    ldx MATHS1
    jsr ball_s_write_spr_data

    ; STEP 5: check for a ball-trunk collision (and subsequent deflection!)...
    jsr ball_s_check_trunk_deflection

    rts
; end sub ball_s_check_trunk_collision
} ; !zone

; **************************************************

!zone {
.V_LEN      = MATHS2
.DRAG_LO    = MATHS3
.DRAG_HI    = MATHS4
.ITER       = MATHS5
.APPARENT_V = MATHS6
.MUST_NEG   = MATHS7

ball_s_calc_air_drag
    ; Recalculate the apparent vector (ball-v - wind).
    ; NOTE: because wind is active only across the xz-plane, the y-component
    ; of the ball's velocity is not affected.
    ; X:
    lda ball_v_mutex_vx_lo
    sec
    sbc winslp_v_final_vx_lo    
    sta ball_v_apparent_vx_lo
    lda ball_v_mutex_vx_hi
    sbc winslp_v_final_vx_hi
    sta ball_v_apparent_vx_hi
    ; Z:
    lda ball_v_mutex_vz_lo
    sec
    sbc winslp_v_final_vz_lo
    sta ball_v_apparent_vz_lo
    lda ball_v_mutex_vz_hi
    sbc winslp_v_final_vz_hi
    sta ball_v_apparent_vz_hi
    ; Y:
    lda ball_v_mutex_vy_lo
    sta ball_v_apparent_vy_lo
    lda ball_v_mutex_vy_hi
    sta ball_v_apparent_vy_hi

    ; And now we need |v| and |v^2| for this new vector.  Pass in high bytes 
    ; for all three components to 'pythag' routine.
    lda ball_v_apparent_vx_hi
    sta P0
    lda ball_v_apparent_vy_hi
    sta P1
    lda ball_v_apparent_vz_hi
    sta P2
    jsr pythag_s_calc_magnitude
    stx .APPARENT_V

    ; Everything initially zero.
    ldx #5
    lda #0
-
    sta ball_v_mutex_drag_x_lo,x
    dex
    bpl -

    ; NOTE: MATHS0-1 hold v^2.
    lda MATHS0
    sta P0
    lda MATHS1
    sta P1
    lda #<ball_c_DRAG_COEFFICIENT
    sta P2
    lda #>ball_c_DRAG_COEFFICIENT
    sta P3
    jsr maths_mul16
    
    ; Result in P4-P7.  We need only the two l.s. bytes.
    ; Put straight into P0-P1 ready for the next multiplication.
    lda P4
    sta P0
    lda P5
    sta P1

    ; Must be distributed appropriately across three components of velocity 
    ; vector.  First divide drag by |v|; store result (16-bit); then multiply
    ; by each component of vector in turn (high-byte only).
    ; (vx.hi*drag)/|v|, etc...
    ; NOTE: first operand is already prepared (see above).
    lda .APPARENT_V
    sta P2
    lda #0
    sta P3
    jsr maths_div16
    ; Store for future use.
    lda P0
    sta .DRAG_LO
    lda P1
    sta .DRAG_HI

    ldx #0
.loop_top
    stx .ITER

    ; NOTE: check for zero here!
    lda #0
    sta .MUST_NEG
    lda ball_v_apparent_vx_lo+1,x
    beq .next
+
    ; Might be negative.
    bpl +
    sta .MUST_NEG
    +nega
+
    sta P0
    lda #0
    sta P1
    lda .DRAG_LO
    sta P2
    lda .DRAG_HI
    sta P3
    jsr maths_mul16
    ; Result in P4-P5.  Negate here if required (before writing to mutex
    ; variables).
    lda .MUST_NEG
    +branch_if_false +
    +neg16 P4
+
    ; NOTE: .ITER holds 0, 2 and 4 in turn.
    ldx .ITER
    lda P4
    sta ball_v_mutex_drag_x_lo,x
    lda P5
    sta ball_v_mutex_drag_x_hi,x

.next
    ldx .ITER
    cpx #4
    beq .done
    inx
    inx
    jmp .loop_top
    
.done
    rts
; end sub ball_s_calc_air_drag
} ; !zone

; **************************************************

!zone {
ball_s_apply_spin
    lda ball_v_applying_spin
    +branch_if_false .end

    ; NOTE: apply only to x-axis!
    +adc16 ball_vx_lo,ball_v_spin_x_lo
    +adc16 ball_vz_lo,ball_v_spin_z_lo

.end
    rts
; end sub ball_s_apply_spin
} ; !zone

; **************************************************

!zone {
.SIGN_CHANGE = MATHS0
.ITER = MATHS1

ball_s_calc_spin
    jsr fmag_s_cross_product
    stx .SIGN_CHANGE
    stx P0

    ; Check if spin is valid.
    lda ball_v_spin_type
    beq .ok
    cmp #ball_c_SPIN_TYPE_HOOK
    beq .hook
    ; So slice...  Spin-x should be positive.  
    lsr P0
    bcc .ok
-
    +clr ball_v_applying_spin
    rts ; EXIT POINT.
.hook
    ; Hook so spin-x should be negative.
    lsr P0
    bcc -
    
.ok
    ; First do: (v^2 * C-spin) and store for future use (in MATHS2/3).
    lda ball_v_current_speed_squared_lo
    sta P0
    lda ball_v_current_speed_squared_hi
    sta P1
spin_here
    lda #<ball_c_SPIN_COEFFICIENT  
    sta P2
    lda #>ball_c_SPIN_COEFFICIENT  
    sta P3
    jsr maths_mul16
    ; FIXME: preserve low and high bytes and interpret as fraction of 2^16?!
    ; NOTE: ball_v_mutex_spin_x_lo, etc. are all positive values with high byte
    ; set to zero.
    lda P4
    sta MATHS2
    lda P5
    sta MATHS3

    ; Scale each component of spin (unit) vector using this coefficient 
    ; (in MATHS2-3), which should be interpreted as a fraction of 2^16.
    ; So we will take only the 3rd m.s. byte of the result (in P6).
    ; NOTE: X = 4,2,0.
    ldx #4

.loop_top
    stx .ITER

    lda ball_v_mutex_spin_x_lo,x
    sta P0
    lda ball_v_mutex_spin_x_hi,x
    sta P1
    lda MATHS2
    sta P2
    lda MATHS3
    sta P3
    jsr maths_mul16
    
    ldx .ITER
    lda P6
    sta ball_v_mutex_spin_x_lo,x
    lda #0
    sta ball_v_mutex_spin_x_hi,x

    dex
    dex
    bpl .loop_top

    ; Dampen force on y-axis.
    ; Multiplying by 0.25 is the same as dividing by 4 (which is the same as
    ; shifting right twice!).
    ; NOTE: y-hi will always be 0 at this point.
    lsr ball_v_mutex_spin_y_lo
    lsr ball_v_mutex_spin_y_lo

    lda .SIGN_CHANGE
    sta P0

    ; Check if any vector components need to be negated.
    ldx .SIGN_CHANGE
    txa
    and #fmag_c_X_MASK 
    beq +
    +neg16 ball_v_mutex_spin_x_lo
+
    txa
    and #fmag_c_Y_MASK 
    beq +
    +neg16 ball_v_mutex_spin_y_lo
+
    txa
    and #fmag_c_Z_MASK 
    beq +
    +neg16 ball_v_mutex_spin_z_lo
+

    rts
; end sub ball_s_calc_spin
} ; !zone

; **************************************************

!zone {
.sign_change_x  !byte   0
.sign_change_z  !byte   0
.COEFFICIENT = LINE_X0_LO 

ball_s_update_rolling_friction
    ; NOTE:
    ; If ball has just started to roll, can't use drag values because they
    ; were last calculated to handle air drag, not rolling friction.  Test
    ; for that flag (i.e. ball_v_is_drag_invalid_for_roll) being set.  If it is,
    ; zero out the drag values (OK because they're not being used at the moment)
    ; and then clear the flag.  For a few frames the ball will apply zeroed out
    ; drag values until these ones have been calculated and are ready for use.
    lda ball_v_is_drag_invalid_for_roll
    +branch_if_false .drag_ok
    ; Don't need to worry about y-axis!
    lda #0
    sta ball_v_drag_x_lo
    sta ball_v_drag_x_hi
    sta ball_v_drag_z_lo
    sta ball_v_drag_z_hi
    sta ball_v_is_drag_invalid_for_roll

.drag_ok
    ; Copy over the latest ball velocity values (x- and z-components only).
    lda ball_v_mutex_vx_lo
    sta ball_v_mutex_drag_x_lo
    lda ball_v_mutex_vx_hi
    sta ball_v_mutex_drag_x_hi
    lda ball_v_mutex_vz_lo
    sta ball_v_mutex_drag_z_lo
    lda ball_v_mutex_vz_hi
    sta ball_v_mutex_drag_z_hi

    ; Need a unit vector.
    lda ball_v_mutex_drag_x_lo
    sta P0
    lda ball_v_mutex_drag_x_hi
    sta P1
    lda ball_v_current_speed
    sta P2
    lda #0
    sta P3
    jsr maths_div16s
    lda SIGN_CHANGED
    sta .sign_change_x
    ; Positive result in P0-P1.  This should be a fraction of 256.  If the
    ; high byte is >0, set low byte to maximum value of 255.
    lda #0
    sta ball_v_mutex_drag_x_hi
    lda P1
    beq +
    lda #255
    +skip_2_bytes 
+
    lda P0
    sta ball_v_mutex_drag_x_lo

    lda ball_v_mutex_drag_z_lo
    sta P0
    lda ball_v_mutex_drag_z_hi
    sta P1
    lda ball_v_current_speed
    sta P2
    lda #0
    sta P3
    jsr maths_div16s
    lda SIGN_CHANGED
    sta .sign_change_z
    lda #0
    sta ball_v_mutex_drag_z_hi
    lda P1
    beq +
    lda #255
    +skip_2_bytes 
+
    lda P0
    sta ball_v_mutex_drag_z_lo

    ; Scale up unit vector by some factor.
    ; FIXME: this value should come from a lookup table indexed by terrain.
    ldx ball_v_current_terrain
    lda round_v_must_putt
    +branch_if_true +
    lda ball_l_FRICTION_COEFFICIENTS,x    
    jmp ++
+
    lda ball_l_PUTTING_FRICTION_COEFFICIENTS,x
++
    sta .COEFFICIENT
    sta P2
    lda #0
    sta P3
    lda ball_v_mutex_drag_x_lo
    sta P0
    lda ball_v_mutex_drag_x_hi
    sta P1
    jsr maths_mul16
    ; Result in P4-P7 but will use up only the first two bytes.  Take higher
    ; byte of these and copy to drag's x-lo.
    lda P5
    sta ball_v_mutex_drag_x_lo

    lda ball_v_mutex_drag_z_lo
    sta P0
    lda ball_v_mutex_drag_z_hi
    sta P1
    lda .COEFFICIENT
    sta P2
    lda #0
    sta P3
    jsr maths_mul16
    lda P5
    sta ball_v_mutex_drag_z_lo

    ; Negate final values if necessary.
    lda .sign_change_x
    beq +
    +utils_m_prepare_zp ball_v_mutex_drag_x_lo,P0
    jsr maths_adjust_vec_signs
+
    lda .sign_change_z
    beq .end
    +utils_m_prepare_zp ball_v_mutex_drag_z_lo,P0
    jsr maths_adjust_vec_signs
    
.end
    rts
; end sub ball_s_update_rolling_friction
} ; !zone

; **************************************************

; INPUTS:   X = 'precision' index.
!zone {
ball_s_set_spin_type
    lda #ball_c_SPIN_TYPE_NONE
    cpx #powarc_c_PRECISION_CENTER   
    bcc .hook
    beq .none
    ; So must be slice...
    lda #ball_c_SPIN_TYPE_SLICE
    +skip_2_bytes 
.hook
    lda #ball_c_SPIN_TYPE_HOOK
.none
    sta ball_v_spin_type
    rts
; end sub ball_s_set_spin_type
} ; !zone

; **************************************************

!zone {
ball_s_check_bounds
    lda ball_v_out_of_bounds
    +branch_if_true .end

    ; Top:
    lda ball_temp_aa_z_lo
    cmp hole_v_boundary_aa_top_lo      
    lda ball_temp_aa_z_hi
    sbc hole_v_boundary_aa_top_hi       
    +blt_s .bottom
    jmp .handle_out_of_bounds
.bottom
    ; Bottom:
    lda ball_temp_aa_z_lo
    cmp hole_v_boundary_aa_bottom_lo
    lda ball_temp_aa_z_hi
    sbc hole_v_boundary_aa_bottom_hi
    +bge_s .left
    jmp .handle_out_of_bounds
.left
    ; Left:
    lda ball_temp_aa_x_lo
    cmp hole_v_boundary_aa_left_lo
    lda ball_temp_aa_x_hi
    sbc hole_v_boundary_aa_left_hi
    +bge_s .right
    jmp .handle_out_of_bounds
.right
    ; Right.
    lda ball_temp_aa_x_lo
    cmp hole_v_boundary_aa_right_lo
    lda ball_temp_aa_x_hi
    sbc hole_v_boundary_aa_right_hi
    +blt_s .end

.handle_out_of_bounds
    inc ball_v_out_of_bounds
    ldx #msg_c_OUT_OF_BOUNDS
    jsr msg_s_display_stock_msg
    ; NOTE: once ball is out-of-bounds, we give the current player the 
    ; opportunity to 'short-circuit' the rest of the ball's flight and go 
    ; straight to their next (retaken) shot.  So make sure to lock joyfire
    ; here otherwise that short-circuit will happen immediately if fire
    ; button still held from precision stage...
    ldx joy_v_current_port
    +joy_m_lock_fire

.end
    rts
; end sub ball_s_check_bounds
} ; !zone

; **************************************************

!zone {
ball_s_calc_distance_of_shot
    lda ball_x_lo
    sta maths_mod08
    lda ball_x_hi
    sta maths_mod09
    lda ball_z_lo
    sta maths_mod10
    lda ball_z_hi
    sta maths_mod11

    lda ball_x_lo
    sta ball_temp_aa_x_lo   
    lda ball_x_hi
    sta ball_temp_aa_x_hi   
    lda ball_z_lo
    sta ball_temp_aa_z_lo   
    lda ball_z_hi
    sta ball_temp_aa_z_hi   

    lda #<ball_temp_aa_x_lo
    sta maths_mod12
    lda #>ball_temp_aa_x_lo
    sta maths_mod13
    lda #<ball_temp_aa_z_lo
    sta maths_mod14
    lda #>ball_temp_aa_z_lo
    sta maths_mod15
    sec
    jsr maths_s_rotate_vec_to_pos_z_axis

    rts
; end sub ball_s_calc_distance_of_shot
} ; !zone

; **************************************************

!zone {
; NOTE: use round_v_must_putt as an index into this table.
.BUFFER_LENS    !byte 16+ball_c_LIE_STRING_LEN,16
.buffer         !raw    "Last shot:"
.buffer2        !fill   16

ball_s_display_distance_of_shot
    lda ball_v_out_of_bounds
    +branch_if_true .exit1
    lda ball_v_is_in_water
    +branch_if_false +
    ldx #msg_c_FISHING
    jsr msg_s_display_stock_msg
.exit1
    rts ; EXIT POINT.

+
    lda #font_c_ASCII_SPACE
    ldx #15
-
    sta .buffer2,x
    dex
    bpl -

    lda ball_temp_aa_z_lo 
    sta P0
    lda ball_temp_aa_z_hi 
    sta P1
    ; NOTE: the divisor depends on whether we're putting or not...
    lda round_v_must_putt
    +branch_if_true +
    lda #<hole_c_PIXELS_PER_YARD 
    sta P2
    lda #>hole_c_PIXELS_PER_YARD 
    sta P3
    beq ++
+
    lda #<hole_c_PIXELS_PER_FOOT
    sta P2
    lda #>hole_c_PIXELS_PER_FOOT
    sta P3
++
    jsr maths_div16
    ; NOTE: result (quotient) already in P0-P1!
    jsr utils_s_16bit_hex_to_dec
    lda #<.buffer2
    sta P0
    lda #>.buffer2
    sta P1
    jsr utils_s_write_digits_to_buffer
    ; NOTE: Y holds number of bytes written.
    ldx round_v_must_putt
    lda stats_l_UNITS_TEXT_OFFSETS,x
    tax
-
    lda stats_l_UNITS_TEXT,x
    beq +
    sta .buffer2,y
    inx
    iny
    bne -

+
    ; Y is index immediately after 'yds'/'ft' string.  Increment once more -
    ; this is where we'll write the current lie.
    iny 
        
    ; But don't display the lie if we're putting and still on the green.
    lda round_v_must_putt 
    +branch_if_false .lie
    lda ball_v_current_terrain
    cmp #ball_c_TERRAIN_GREEN_FWAY   
    beq .skip_lie

.lie
    ; Now set the string for current lie/terrain.
    ; Terrain should never be WATER!!!
    ldx ball_v_current_terrain
    ; This variable set at end of each shot in 'golfer_s_record_distance'.
    ; Takes into account terrain and distance.  So if it's true, add one to
    ; index X which will go from 'fairway' string to 'green' string...
    lda round_v_current_player_is_on_green
    +branch_if_false +
    inx
    inx
+
    lda ball_l_LIE_OFFSETS,x
    tax

-
    lda ball_c_LIE_STRINGS,x
    beq +
    sta .buffer2,y
    inx
    iny
    bne -

+
.skip_lie

    lda #<.buffer
    sta P0
    lda #>.buffer
    sta P1

    lda #26
    sta P4
    jsr msg_s_display

.end
    rts
; end sub ball_s_display_distance_of_shot
} ; !zone

; **************************************************

; INPUTS:   X = sprite pattern in range [0,4); negative bit set if ball is
;           behind trunk (i.e. not just the shadow)...
!zone {
ball_s_write_spr_data
    stx MATHS2
    txa
    and #$7f

    ; Adjust index if ball's x-pos is odd (+12) or ball is 'distant' (+24).
    ldy ball_v_is_distant
    beq +
    clc
    adc #24
    bne .ready
+
    ldy ball_v_spr_x_parity
    beq .ready
    clc
    adc #12

.ready
    tax
    lda ball_l_SPR_DATA_EVEN_ROW0,x
    sta ball_c_SPR_ADDRESS 
    sta ball_c_SHADOW_SPR_ADDRESS
    lda ball_l_SPR_DATA_EVEN_ROW0+4,x
    sta ball_c_SPR_ADDRESS+3
    sta ball_c_SHADOW_SPR_ADDRESS+3

    ; If ball is above tree trunks, make sure it's fully visible.
    lda MATHS2
    bmi .end
    lda ball_v_is_distant
    +branch_if_true +
    lda #$c0
    sta ball_c_SPR_ADDRESS
    sta ball_c_SPR_ADDRESS+3
    bne .end
+
    lda #$80
    sta ball_c_SPR_ADDRESS
    lda #0
    sta ball_c_SPR_ADDRESS+3

.end
    ; FIXME: messy!
    ; If ball is rolling, make sure shadow sprite data is cleared.
    lda ball_v_current_state
    cmp #ball_c_STATE_IN_FLIGHT
    beq +
    lda #0
    sta ball_c_SHADOW_SPR_ADDRESS
    sta ball_c_SHADOW_SPR_ADDRESS+3
+
    rts
; end sub ball_s_write_spr_data
} ; !zone

; **************************************************

!zone {
ball_s_calc_current_row_and_column
    lda ball_v_mc_x
    sta ball_v_last_mc_x    

    lda ball_v_mutex_spr_x_lo
    pha
    sec
    sbc #spr_c_VISIBLE_ALL_L 
    sta MATHS0
    lda ball_v_mutex_spr_x_hi
    sbc #0
    lsr 
    lda MATHS0
    ror
    sta ball_v_mc_x 

    lda ball_v_mutex_spr_y
    sec
    sbc #spr_c_VISIBLE_ALL_T 
    ; NOTE: watch out for this becoming negative!
    bcs +
    lda #0
+
    sta ball_v_mc_y

    pla
    and #$01
    sta ball_v_spr_x_parity

    rts
; end sub ball_s_calc_current_row_and_column
} ; !zone

; **************************************************

; INPUTS:   X = index into 'pattern' tables (adjusted for even/odd/small).
;           MATHS0 = trunks in two current columns
;           MATHS1 = trunks ball is behind; bit #7 set if collision
!zone {
ball_s_check_trunk_deflection
    lda ball_v_must_deflect 
    bmi .end

    lda ball_v_last_in_front_of_trunk
    +branch_if_false .check_if_in_front

    lda MATHS1
    cmp #$81
    bcc .check_if_in_front

    lda ball_v_mc_x
    cmp ball_v_last_mc_x    
    bne .check_if_in_front

    lda ball_l_SPR_DATA_EVEN_ROW0+8,x
    beq .check_if_in_front

    ; DEFLECT!!!
    lda #1
    sta ball_v_must_deflect 
    rts ; EXIT POINT.

.check_if_in_front
    ldx #0
    lda MATHS0
    beq .no
    lda MATHS1
    cmp #$80
    bne .no
    inx
.no
    stx ball_v_last_in_front_of_trunk
.end
    rts
; end sub ball_s_check_trunk_deflection
} ; !zone

; **************************************************

!zone {
ball_s_deflect
    ; Make sure vx isn't negative - which would make vz negative 
    ; after they're swapped!!!
    lda ball_vx_hi
    bpl +
    +neg16 ball_vx_lo

+
    lda ball_vx_lo
    pha
    lda ball_vx_hi
    pha
    lda ball_vz_lo
    sta ball_vx_lo
    lda ball_vz_hi
    sta ball_vx_hi
    pla
    sta ball_vz_hi
    pla
    sta ball_vz_lo

    ; Randomly deflected left or right.
    jsr rand_s_get
    bmi +
    +neg16 ball_vx_lo
+
    +clr ball_v_applying_spin

    ; Scale down velocity!
    +scale_down ball_vx_lo,230
    lda P5
    sta ball_vx_lo
    lda P6
    sta ball_vx_hi
    +scale_down ball_vy_lo,230
    lda P5
    sta ball_vy_lo
    lda P6
    sta ball_vy_hi
    +scale_down ball_vz_lo,230
    lda P5
    sta ball_vz_lo
    lda P6
    sta ball_vz_hi

    ; Once deflection is complete, set negative bit of flag so can't be
    ; deflected again.
    lda ball_v_must_deflect 
    ora #$80
    sta ball_v_must_deflect 
    ldy #sfx_c_BALL_TREE
    jsr snd_s_init_sfx

    rts
; end sub ball_s_deflect
} ; !zone

; **************************************************

!zone {
ball_s_check_pole_collision
    ; Check for collisions only if depth=2.
    lda target_v_current_depth
    cmp #target_c_POLE_COLLISION_MIN_DEPTH
    bcc .end
    cmp #target_c_POLE_COLLISION_MAX_DEPTH+1
    bcs .end
    ; And there hasn't already been a deflection.
    lda ball_v_must_deflect 
    bne .end

    ; Collision with flagpole (with respect to sprite coordinates)?
    lda spr_v_y+ball_c_SW_SPR_NUM                    
    cmp target_v_top_of_pole
    bcc .no_collision
    lda spr_v_x_hi+ball_c_SW_SPR_NUM
    bne .no_collision
    lda spr_v_x_lo+ball_c_SW_SPR_NUM
    clc
    adc #1
    sec
    sbc spr_v_x_lo+target_c_SW_SPR_NUM
    cmp #3
    bcs .no_collision

    ; Collision detected.  If ball is in front of flagpole, record the fact.
    ; If ball is behind flagpole and there was a collision in the previous 
    ; 'frame', DEFLECT!
    ; FIXME: if behind flagpole but no collision previous frame, don't need 
    ; to check for ball-pole collisions any more during this shot...
    lda ball_v_hw_spr_num         
    cmp #ball_c_SW_SPR_NUM 
    beq .in_front
    ; So behind...
    lda ball_v_last_in_front_of_pole
    beq .end
    lda #1
    sta ball_v_must_deflect
    rts ; EXIT POINT.

.in_front
    lda #1
    sta ball_v_last_in_front_of_pole
    rts ; EXIT POINT.
    
.no_collision
    +clr ball_v_last_in_front_of_pole

.end
    rts
; end sub ball_s_check_pole_collision
} ; !zone

; **************************************************

!zone {
ball_s_update_overhead_ball
    ; First we want the world coordinates of the ball.
    ; Then scale this down so it has the same resolution as the overhead map.
    ; Add that value to the overhead map origin, taking care of the sign of 
    ; the quotient.
    lda ball_world_x_lo
    clc
    adc ball_temp_aa_x_lo
    sta P0
    lda ball_world_x_hi
    adc ball_temp_aa_x_hi
    sta P1
    lda hole_v_overhead_scale_lo
    sta P2
    lda hole_v_overhead_scale_hi
    sta P3
    jsr maths_div16s

    ; Result is in P0-P1.
    lda SIGN_CHANGED
    beq .add
    ; Subtract.
    lda hole_v_overhead_origin_x_lo
    sec
    sbc P0
    sta hole_v_overhead_ball_x_lo
    lda hole_v_overhead_origin_x_hi
    sbc P1
    sta hole_v_overhead_ball_x_hi
    jmp .z_axis
.add
    lda hole_v_overhead_origin_x_lo
    clc
    adc P0
    sta hole_v_overhead_ball_x_lo
    lda hole_v_overhead_origin_x_hi
    adc P1
    sta hole_v_overhead_ball_x_hi

.z_axis
    lda ball_world_z_lo
    clc
    adc ball_temp_aa_z_lo
    sta P0
    lda ball_world_z_hi
    adc ball_temp_aa_z_hi
    sta P1
    lda hole_v_overhead_scale_lo
    sta P2
    lda hole_v_overhead_scale_hi
    sta P3
    jsr maths_div16s
    
    ; Result is in P0-P1.
    lda SIGN_CHANGED
    beq .subtract
    ; We would normally subtract, but if result is negative, add instead.
    lda hole_v_overhead_origin_y
    clc
    adc P0
    sta hole_v_overhead_ball_y
    jmp .end
.subtract
    lda hole_v_overhead_origin_y
    sec
    sbc P0
    sta hole_v_overhead_ball_y

.end
    rts
; end sub ball_s_update_overhead_ball
} ; !zone

; **************************************************

!zone {
ball_s_init_water_splash_animation
    ; TODO: check distance and position - shouldn't be behind a tree!
    lda ball_v_is_distant
    +branch_if_false +
    ldx ball_v_hw_spr_num
    +spr_m_disable
    jsr ball_s_end_shot
    ; FIXME: this can cause flickering sometimes!
    rts ; EXIT POINT.

+
    ; Move the ball sprite's x-position two pixels to the left.
    ; NOTE: whether we use the ball sprite or shadow sprite as basis for
    ; position depends on how the ball entered the water (i.e. did it
    ; bounce or roll?).
    ldx ball_v_splash_anim_base_sw_sprite
    lda spr_v_x_lo,x
    sec
    sbc #2
    sta spr_v_x_lo+ball_c_SW_SPR_NUM
    lda spr_v_x_hi,x
    sbc #0
    sta spr_v_x_hi+ball_c_SW_SPR_NUM
    ldy spr_v_y,x
    iny
    sty spr_v_y+ball_c_SW_SPR_NUM

    ldx #0
    stx ball_v_splash_anim_iter
    stx ball_c_SPR_ADDRESS+3
    lda ball_l_SPLASH_ANIM_DATA,x
    sta ball_c_SPR_ADDRESS
    lda #ball_c_SPLASH_ANIM_FRAME_RATE
    sta ball_v_splash_anim_count

    lda #ball_c_STATE_SPLASHING
    sta ball_v_current_state

    ldy #ball_c_SW_SPR_NUM
    ldx ball_v_hw_spr_num
    jsr spr_s_write_to_vic_ii
    rts
; end sub ball_s_init_water_splash_animation
} ; !zone

; **************************************************

!zone {
ball_s_update_splash_animation
    dec ball_v_splash_anim_count
    bne .end

    ldx ball_v_splash_anim_iter
    inx
    lda ball_l_SPLASH_ANIM_DATA,x
    beq .end_shot
    stx ball_v_splash_anim_iter
    sta ball_c_SPR_ADDRESS

    ; Prepare the next delay.
    lda #ball_c_SPLASH_ANIM_FRAME_RATE
    sta ball_v_splash_anim_count
    rts ; EXIT POINT.

.end_shot
    ldx ball_v_hw_spr_num
    +spr_m_disable
    jsr ball_s_end_shot

.end
    rts
; end sub ball_s_update_splash_animation
} ; !zone

; **************************************************

!zone {
ball_s_do_final_terrain_check
    lda ball_x_lo
    sta ball_temp_aa_x_lo
    lda ball_x_hi
    sta ball_temp_aa_x_hi
    lda ball_z_lo
    sta ball_temp_aa_z_lo
    lda ball_z_hi
    sta ball_temp_aa_z_hi

    jsr ball_s_undo_rotation
    jsr ball_s_check_collision

    rts
; end sub ball_s_do_final_terrain_check
} ; !zone

; **************************************************

!zone {
.ENERGY_CONSERVE_FACTOR = FADE_CR_LO

ball_s_precalculate_bounce
    ; Don't bother unless ball is falling.
    ; FIXME: also don't bother if terrain is WATER!
    lda ball_v_mutex_vy_hi
    bmi +
    ; FIXME: probably don't need this!
    rts ; EXIT POINT.

+
    ldx ball_v_current_terrain
    lda ball_l_ELASTICITY,x
    sta P0
    lda #0
    sta P1
    lda ball_v_mutex_vy_lo
    sta P2
    lda ball_v_mutex_vy_hi
    sta P3
    jsr maths_mul16s
    ; Product in P4-P7.  Take P5-P6 as lo/hi bytes because dividing by 256.
    ; Sign will have changed but that's OK - we want positive value on y-axis.
    lda P5
    sta ball_v_mutex_vy_lo_on_bounce
    lda P6
    sta ball_v_mutex_vy_hi_on_bounce

    ldx ball_v_current_terrain
    lda ball_l_FRICTION_ON_BOUNCE,x
    pha
    sta P0
    lda #0
    sta P1
    lda ball_v_mutex_vx_lo
    sta P2
    lda ball_v_mutex_vx_hi
    sta P3
    jsr maths_mul16s
    lda P5
    sta ball_v_mutex_vx_lo_on_bounce
    lda P6
    sta ball_v_mutex_vx_hi_on_bounce
    lda SIGN_CHANGED
    +branch_if_false +
    +neg16 ball_v_mutex_vx_lo_on_bounce
+
    ; Friction value off of stack.
    pla
    sta P0
    lda #0
    sta P1
    lda ball_v_mutex_vz_lo
    sta P2
    lda ball_v_mutex_vz_hi
    sta P3
    ; NOTE: assume vz is always positive!
    jsr maths_mul16
    lda P5
    sta ball_v_mutex_vz_lo_on_bounce
    lda P6
    sta ball_v_mutex_vz_hi_on_bounce

    rts
; end sub ball_s_precalculate_bounce
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

ball_c_SIZE = *-ball_c_BEGIN 

