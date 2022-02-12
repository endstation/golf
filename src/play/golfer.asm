; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


golfer_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
GOLFER_CLUB_SHAFT_SPR_NUM   = 1
GOLFER_UPPER_BODY_SPR_NUM   = 2
GOLFER_LOWER_BODY_SPR_NUM   = 3

;GOLFER_READY                = 0
;GOLFER_LOOKING_UP           = 1
;GOLFER_SWINGING_BACK        = 2
;GOLFER_TOP_OF_SWING         = 3
;GOLFER_SWINGING_FORWARD     = 4
;GOLFER_SWING_COMPLETE       = 5
;GOLFER_WATCHING_SHOT       = 6
;GOLFER_ACKNOWLEDGING_CROWD  = 8
;GOLFER_FINISHED             = 10
;GOLFER_PUTT_READY           = 11
;GOLFER_PUTT_LOOKING_UP      = 12
;GOLFER_PUTT_POWERING_UP     = 13
;GOLFER_PUTT_POISED          = 14
;GOLFER_PUTT_FORE_STROKE     = 15
;GOLFER_PUTT_HOLDING         = 16
;GOLFER_IN_LIMBO             = 17
;GOLFER_PUTT_FORE_STROKE_0   = 18
;GOLFER_PUTT_FORE_STROKE_1   = 19

GOLFER_POWERING_UP = 20
GOLFER_SETTING_SPIN = 21


GOLFER_POS_X_LO = spr_c_VISIBLE_ALL_L+116
GOLFER_POS_X_HI = 0
GOLFER_POS_Y_UPPER = spr_c_VISIBLE_ALL_T+140
GOLFER_POS_Y_LOWER = spr_c_VISIBLE_ALL_T+161
GOLFER_SHADOW_POS_Y = spr_c_VISIBLE_ALL_T+182
GOLFER_SHADOW_POS_X_LO = spr_c_VISIBLE_ALL_L+110
; Add this to golfer and shadow sprites when putting.
GOLFER_POS_PUTTING_X_OFFSET = 24

GOLFER_HOLD_TIME = 6
golfer_c_HOLD_TIME_SWING_COMPLETE = 40

; When animation sequence gets to this frame, initiate ball flight.
GOLFER_STRIKE_FRAME = 174

GOLFER_POS_MARKER_SPR_NUM = 6
GOLFER_POS_MARKER_SPR_PTR = 162
GOLFER_POS_MARKER_Y = spr_c_VISIBLE_ALL_T+150-2;-24
GOLFER_POS_MARKER_X = spr_c_VISIBLE_ALL_L+160-2
GOLFER_POS_MARKER_MIN_X = GOLFER_POS_MARKER_X-50
GOLFER_POS_MARKER_MAX_X = GOLFER_POS_MARKER_X+50
GOLFER_POS_MARKER_VX_LO = 200
GOLFER_POS_MARKER_SLOW_VX_LO = 32
GOLFER_POS_MARKER_VX_HI = 0
; NOTE: 0th value is a dummy to make sure everything's in the right place.  If
; offset is 0, no adjustment to the velocity vectors is necessary...
GOLFER_POS_MARKER_TRIG_INDICES !byte 0,3,7,10,13,17,20,23,26,29,32,35,38,42,45,48,51,53,56,59,62,65,68,70,73,76,78,81,83,85,88,90,93,95,97,99,101,104,106,108,110,112,114,116,117,119,121,123,125,126,128

!zone {
.BX = spr_c_VISIBLE_ALL_L-2 ; minus 2 because sprite is 5 pixels wide. 
.BY = spr_c_VISIBLE_ALL_T 
golfer_l_MARKER_X_POS !byte .BX+125,.BX+131,.BX+137,.BX+143,.BX+148,.BX+154,.BX+160,.BX+166,.BX+172,.BX+177,.BX+183,.BX+189,.BX+195
golfer_l_MARKER_Y_POS !byte .BY+150,.BY+149,.BY+149,.BY+148,.BY+148,.BY+147,.BY+147,.BY+147,.BY+148,.BY+148,.BY+149,.BY+149,.BY+150
golfer_l_MARKER_X_POS2
    !byte   .BX+130,.BX+132,.BX+134,.BX+136,.BX+138,.BX+140,.BX+142,.BX+144,.BX+146,.BX+148,.BX+149,.BX+151,.BX+153,.BX+155,.BX+157,.BX+159,.BX+160
    !byte   .BX+161,.BX+163,.BX+165,.BX+167,.BX+169,.BX+171,.BX+172,.BX+174,.BX+176,.BX+178,.BX+180,.BX+182,.BX+184,.BX+186,.BX+188,.BX+190
golfer_l_MARKER_Y_POS2
    !byte   .BY+150,.BY+149,.BY+149,.BY+149,.BY+149,.BY+148,.BY+148,.BY+148,.BY+148,.BY+148,.BY+148,.BY+148,.BY+147,.BY+147,.BY+147,.BY+147,.BY+147
    !byte   .BY+147,.BY+147,.BY+147,.BY+147,.BY+148,.BY+148,.BY+148,.BY+148,.BY+148,.BY+148,.BY+148,.BY+149,.BY+149,.BY+149,.BY+149,.BY+150
} ; !zone
golfer_l_TRIG_INDICES !byte 6,5,4,3,2,1,0,63,62,61,60,59,58
golfer_c_MARKER_MOVE_DELAY_NORMAL   = 2
golfer_c_MARKER_MOVE_DELAY_SLOW     = 8
; NOTE: index this table with direction (spr_c_LEFT/RIGHT).
golfer_l_MARKER_BOUNDS !byte 0,32
golfer_l_MARKER_DELTA !byte (-1),1

GOLFER_BACKSWING_FRAME_RATE = 7
GOLFER_FORESWING_FRAME_RATE = 7
GOLFER_PUTTING_FRAME_RATE = 8 

; NOTE: INTEGER division!
;golfer_c_POWER_INCREMENT = 256 / (SPRD_GOLFER_BACKSWING_FRAMES*GOLFER_BACKSWING_FRAME_RATE)
GOLFER_PUTT_POWER_INCREMENT = 256 / (sprd_c_GOLFER_PUTT_BACKSWING_FRAMES*GOLFER_PUTTING_FRAME_RATE)

; Number of frames either side of 'ball-contact' frame that determine ball
; spin.  If button was pressed beyond or at this threshold, that's max spin
; (hook or slice). 
GOLFER_SPIN_THRESHOLD = 6
; (For full-swing) there are 28 frames from top-of-swing to ball contact.
GOLFER_FRAMES_TO_CONTACT = 28
GOLFER_PRECISION_ZONE_START = GOLFER_FRAMES_TO_CONTACT - GOLFER_SPIN_THRESHOLD
GOLFER_PRECISION_ZONE_END   = GOLFER_FRAMES_TO_CONTACT + GOLFER_SPIN_THRESHOLD
GOLFER_CLUB_NORMAL_COLOR = BLACK
GOLFER_CLUB_HIGHLIGHT_COLOR = GREY1

GOLFER_CROSSHAIR_SPR_NUM = 6
;GOLFER_CROSSHAIR_COLORS !byte   0,6,11,4,14,5,3,13,1,13,3,5,14,4,11,6
;GOLFER_CROSSHAIR_COLORS_N = 16
;golfer_c_CROSSHAIR_PULSE_DELAY = 10

; Where to position sprites in swing animation sequence.
GOLFER_BASE_X = spr_c_VISIBLE_ALL_L+128 ;124
GOLFER_BASE_Y = spr_c_VISIBLE_ALL_T+140;-24

GOLFER_UPPER_BODY_POS_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X

    !byte   GOLFER_BASE_X

    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X-2
    !byte   GOLFER_BASE_X-3
GOLFER_UPPER_BODY_LOOKING_UP_POS_Y = GOLFER_BASE_Y+5
GOLFER_UPPER_BODY_POS_Y
    !byte   GOLFER_BASE_Y+6
    !byte   GOLFER_BASE_Y+6

    !byte   GOLFER_BASE_Y+6

    !byte   GOLFER_BASE_Y+6
    !byte   GOLFER_BASE_Y+6
    !byte   GOLFER_BASE_Y+3
    !byte   GOLFER_BASE_Y+9
    !byte   GOLFER_BASE_Y+7
    !byte   GOLFER_BASE_Y+5
    !byte   GOLFER_BASE_Y+6
    !byte   GOLFER_BASE_Y+8
    !byte   GOLFER_BASE_Y+7
    !byte   GOLFER_BASE_Y
    !byte   GOLFER_BASE_Y
GOLFER_LOWER_BODY_POS_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X

    !byte   GOLFER_BASE_X

    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X+2
    !byte   GOLFER_BASE_X
    !byte   GOLFER_BASE_X-2
    !byte   GOLFER_BASE_X-3
; NOTE: lower body y is always GOLFER_BASE_Y+21.
GOLFER_CLUB_X
    !byte   GOLFER_BASE_X+12
    !byte   GOLFER_BASE_X+14

    !byte   GOLFER_BASE_X-2

    !byte   GOLFER_BASE_X-12 ;8
    !byte   GOLFER_BASE_X-18 ;14
    !byte   GOLFER_BASE_X-2
    !byte   GOLFER_BASE_X-12 ;6
    !byte   GOLFER_BASE_X-16 ;12
    !byte   GOLFER_BASE_X+0 ;2
    !byte   GOLFER_BASE_X+18
    !byte   GOLFER_BASE_X+22
    !byte   GOLFER_BASE_X-10 ;6
    !byte   GOLFER_BASE_X-6 ;2
    !byte   GOLFER_BASE_X
GOLFER_CLUB_Y
    !byte   GOLFER_BASE_Y+23
    !byte   GOLFER_BASE_Y+23

    !byte   GOLFER_BASE_Y+21

    !byte   GOLFER_BASE_Y+7
    !byte   GOLFER_BASE_Y-7 ;3
    !byte   GOLFER_BASE_Y-6 ;3
    !byte   GOLFER_BASE_Y+3 ;7
    !byte   GOLFER_BASE_Y+6
    !byte   GOLFER_BASE_Y+21
    !byte   GOLFER_BASE_Y+24
    !byte   GOLFER_BASE_Y+21
    !byte   GOLFER_BASE_Y-7 ;5
    !byte   GOLFER_BASE_Y-8 ;6
    !byte   GOLFER_BASE_Y-2

GOLFER_PUTT_POS_X = GOLFER_BASE_X+12
GOLFER_PUTT_POS_Y = GOLFER_BASE_Y
; FIXME: NOTE: always the same offsets!!!
GOLFER_PUTT_CLUB_SHAFT_OFFSETS_X = GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
;    !byte   GOLFER_PUTT_POS_X+10
GOLFER_PUTT_CLUB_SHAFT_OFFSETS_Y = GOLFER_BASE_Y+21+3
;    !byte   GOLFER_BASE_Y+21+3
;    !byte   GOLFER_BASE_Y+21+3
;    !byte   GOLFER_BASE_Y+21+3
;    !byte   GOLFER_BASE_Y+21+3
;
;    !byte   GOLFER_BASE_Y+21+3
;    !byte   GOLFER_BASE_Y+21+3
;    !byte   GOLFER_BASE_Y+21+3

GOLFER_WATCHING_UPPER_OFFSET_X = GOLFER_BASE_X-4
GOLFER_WATCHING_UPPER_OFFSET_Y = GOLFER_BASE_Y+1
GOLFER_WATCHING_CLUB_OFFSET_X = GOLFER_BASE_X-2
GOLFER_WATCHING_CLUB_OFFSET_Y = GOLFER_BASE_Y-20

golfer_c_STATE_READY                    = 0
golfer_c_STATE_LOOKING_UP               = 1
golfer_c_STATE_POWERING_UP              = 2
golfer_c_STATE_HOLD                     = 3
golfer_c_STATE_PRECISION                = 4
golfer_c_STATE_HOLD2                    = 5
golfer_c_STATE_ANIMATE_BACKSWING        = 6
golfer_c_STATE_ANIMATE_HOLD             = 7
golfer_c_STATE_ANIMATE_FORESWING        = 8
golfer_c_STATE_ANIMATE_HOLD2            = 9
golfer_c_STATE_FINISHED                 = 10
golfer_c_STATE_ANIMATE_BACKSWING_PUTT   = 11
golfer_c_STATE_ANIMATE_HOLD_PUTT        = 12
golfer_c_STATE_ANIMATE_FORESWING_PUTT   = 13
;golfer_c_STATE_CONCEDE_QUERY            = 14
golfer_c_STATE_IN_LIMBO                 = 14

golfer_c_HOLD_DURATION = 20
golfer_c_HOLD_DURATION_SWING = 10

golfer_c_BASE_DIRECTION_X_LO = 0
golfer_c_BASE_DIRECTION_X_HI = 0
golfer_c_BASE_DIRECTION_Z_LO = 255
golfer_c_BASE_DIRECTION_Z_HI = 0

golfer_c_MARKER_INITIAL_INDEX = 16

golfer_l_SHADOW_PATTERN !byte $ff,$ff,$ff,$f3,$fc,$f3,$fc,$f3
                        !byte $ff,$ff,$ff,$33,$cf,$33,$cc,$33
golfer_l_SHADOW_POS_LO  !byte <gfxs_c_BITMAP_BASE+(22*40*8+15*8),<gfxs_c_BITMAP_BASE+(22*40*8+16*8)
golfer_l_SHADOW_POS_HI  !byte >gfxs_c_BITMAP_BASE+(22*40*8+15*8),>gfxs_c_BITMAP_BASE+(22*40*8+16*8)
golfer_l_ANTI_SHADOW_BUFFER !fill   16
golfer_l_SHADOW_BUFFER      !fill   16

golfer_c_MIN_PRAISEWORTHY_PUTT_FT = 15*hole_c_PIXELS_PER_FOOT 

golfer_c_RESPONSE_NO    = 0
golfer_c_RESPONSE_YES   = 1
golfer_l_RESPONSE_OFFSET_BEGIN  !byte   14,17
; NOTE: actually one-past-end!
golfer_l_RESPONSE_OFFSET_END    !byte   16,20
golfer_c_RESPONSE_GHOST_COLOR       = GREY1
golfer_c_RESPONSE_HIGHLIGHT_COLOR   = YELLOW

;golfer_l_CROSSHAIR_PULSE_COLORS
;    !byte   BLACK,GREY1,GREY2,GREY3,WHITE,GREY3,GREY2,GREY1,$ff


; *****************
; *** VARIABLES ***
; *****************
golfer_v_current_state        !byte   0
golfer_look_up_count        !byte   0
golfer_v_frame_count          !byte   0
golfer_has_hit_ball         !byte   0
; NOTE: negative value indicates that it isn't moving.
golfer_pos_marker_direction !byte   $ff
golfer_v_shot_power           !byte   0
golfer_shot_power_finalized !byte   0
golfer_v_shot_spin_set        !byte   0
;golfer_v_crosshair_pulse_count    !byte   0
;golfer_v_crosshair_pulse_index    !byte   0
golfer_v_crosshair_active     !byte   0
; Keep track of this so we can re-position sprites during animation.
golfer_anim_frame   !byte   0
; FIXME: HACK!!!
golfer_keyboard_locked  !byte   0

; NOTE: a unit vector.
golfer_v_direction_x_lo !byte   0
golfer_v_direction_x_hi !byte   0
golfer_v_direction_z_lo !byte   0
golfer_v_direction_z_hi !byte   0

;golfer_v_slow_crosshair !byte   0

golfer_v_marker_index       !byte   0
golfer_v_marker_move_count  !byte   0

golfer_v_is_hidden  !byte   0
; When golfer hidden, record previous pointers here so they can later be
; restored.
; FIXME: necessary?  Not always the same?!
golfer_v_spr_pointer_buffer !fill   3

golfer_v_current_skin_tone  !byte   0
; For concede requests.
golfer_v_current_response       !byte   0
golfer_v_concede_query_active   !byte   0

golfer_v_marker_move_delay  !byte   0

; 'P' key is used to toggle punch shot.  Needs to be locked after being 
; pressed to prevent continuous processing.
golfer_v_p_key_locked   !byte   0
golfer_v_punch_selected !byte   0
golfer_v_kb_locked      !byte   0


; *******************
; ****** MACROS *****
; *******************
!macro golfer_set_club_color .color {
    lda #.color
    sta spr_v_color+GOLFER_CLUB_SHAFT_SPR_NUM
} ; golfer_set_club_color

!macro golfer_m_init_hold .frames,.new_state {
    lda #.frames
    sta golfer_v_frame_count
    lda #.new_state
    sta golfer_v_current_state
} ; golfer_m_init_hold

!macro golfer_m_update_hold {
    dec golfer_v_frame_count
} ; golfer_m_update_hold


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
golfer_s_draw
    ldx #GOLFER_CLUB_SHAFT_SPR_NUM
    ldy #GOLFER_CLUB_SHAFT_SPR_NUM
    jsr spr_s_write_to_vic_ii

    ldx #GOLFER_UPPER_BODY_SPR_NUM
    ldy #GOLFER_UPPER_BODY_SPR_NUM
    jsr spr_s_write_to_vic_ii

;    ldx #GOLFER_LOWER_BODY_SPR_NUM
;    ldy #GOLFER_LOWER_BODY_SPR_NUM
;    jsr spr_s_write_to_vic_ii

    lda golfer_v_crosshair_active
    +branch_if_false +
    ldx #GOLFER_CROSSHAIR_SPR_NUM
    ldy #GOLFER_CROSSHAIR_SPR_NUM
    jsr spr_s_write_to_vic_ii

+
    ldx #partsys_c_SPR_NUM
    ldy #partsys_c_SPR_NUM
    jsr spr_s_write_to_vic_ii

    rts
; end sub golfer_s_draw
} ; !zone

; **************************************************

!zone {
golfer_s_draw_lower
    ldx #GOLFER_LOWER_BODY_SPR_NUM
    ldy #GOLFER_LOWER_BODY_SPR_NUM
    jsr spr_s_write_to_vic_ii
    rts
; end sub golfer_s_draw_lower
} ; !zone

; **************************************************

!zone {
golfer_s_init_swing_animation
    ; Torso.
    lda #sprd_c_GOLFER_SWING_UPPER 
;    clc
;    adc round_current_gender_offset 
    sta spr_v_anim_start_ptr+GOLFER_UPPER_BODY_SPR_NUM
    ; Make sure we're not looking up!
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM

    lda GOLFER_UPPER_BODY_POS_Y
    sta spr_v_y+GOLFER_UPPER_BODY_SPR_NUM

    lda #sprd_c_GOLFER_SWING_UPPER+sprd_c_GOLFER_BACKSWING_FRAMES-1  
;    clc
;    adc round_current_gender_offset 
    sta spr_v_anim_end_ptr+GOLFER_UPPER_BODY_SPR_NUM
    ; Legs.
    lda #sprd_c_GOLFER_SWING_LOWER 
    sta spr_v_current_ptr+GOLFER_LOWER_BODY_SPR_NUM
    lda #sprd_c_GOLFER_SWING_LOWER+sprd_c_GOLFER_BACKSWING_FRAMES-1
    sta spr_v_anim_end_ptr+GOLFER_LOWER_BODY_SPR_NUM
    lda #sprd_c_GOLFER_SWING_CLUB
    sta spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
    lda #sprd_c_GOLFER_SWING_CLUB+sprd_c_GOLFER_BACKSWING_FRAMES-1
    sta spr_v_anim_end_ptr+GOLFER_CLUB_SHAFT_SPR_NUM

    lda #1
    sta spr_v_anim_seq_inc+GOLFER_UPPER_BODY_SPR_NUM
    sta spr_v_anim_seq_inc+GOLFER_LOWER_BODY_SPR_NUM
    sta spr_v_anim_seq_inc+GOLFER_CLUB_SHAFT_SPR_NUM

    lda #GOLFER_BACKSWING_FRAME_RATE
    sta spr_v_anim_timer+GOLFER_UPPER_BODY_SPR_NUM
    sta spr_v_anim_timer+GOLFER_LOWER_BODY_SPR_NUM
    sta spr_v_anim_timer+GOLFER_CLUB_SHAFT_SPR_NUM
    sta spr_v_framerate+GOLFER_UPPER_BODY_SPR_NUM
    sta spr_v_framerate+GOLFER_LOWER_BODY_SPR_NUM
    sta spr_v_framerate+GOLFER_CLUB_SHAFT_SPR_NUM

    ; Get rid of the crosshair sprite.
    ; TODO: maybe re-use it as ball's shadow?
;    +spr_m_disable2 GOLFER_POS_MARKER_SPR_NUM
;    +clr golfer_crosshair_active

    rts
; end sub golfer_s_init_swing_animation
} ; !zone

; **************************************************

; NOTE: these three routines called by raster interrupt callback.
; This update routine is for the animations.
!zone {
golfer_s_update_anim
    lda golfer_v_current_state

    cmp #golfer_c_STATE_READY
    bne +
    jmp .looking_down
+
    cmp #golfer_c_STATE_LOOKING_UP
    bne +
    jmp .looking_up
+
    cmp #golfer_c_STATE_POWERING_UP
    bne +
    jmp .powering_up
+
    cmp #golfer_c_STATE_HOLD
    bne +
    jmp .hold
+
    cmp #golfer_c_STATE_PRECISION
    bne +
    jmp .precision
+
    cmp #golfer_c_STATE_HOLD2
    bne +
    jmp .hold2
+
    cmp #golfer_c_STATE_ANIMATE_BACKSWING
    bne +
    jmp .backswing
+
    cmp #golfer_c_STATE_ANIMATE_HOLD
    bne +
    jmp .hold_animate
+
    cmp #golfer_c_STATE_ANIMATE_FORESWING
    bne +
    jmp .foreswing
+
    cmp #golfer_c_STATE_ANIMATE_HOLD2
    bne +
    jmp .animate_hold2
+
    cmp #golfer_c_STATE_ANIMATE_BACKSWING_PUTT
    bne +
    jmp .backswing_putt
+
    cmp #golfer_c_STATE_ANIMATE_HOLD_PUTT
    bne +
    jmp .hold_putt
+
    cmp #golfer_c_STATE_ANIMATE_FORESWING_PUTT
    bne +
    jmp .foreswing_putt
    rts ; EXIT POINT.

.looking_down
    jsr golfer_s_update_looking_down
;    jsr golfer_move_crosshair
    jsr golfer_s_move_marker
    rts ; EXIT POINT.
.looking_up
    jsr golfer_s_update_looking_up
;    jsr golfer_move_crosshair
    jsr golfer_s_move_marker
    rts ; EXIT POINT.
.powering_up
.precision
    jsr powarc_s_update
    rts ; EXIT POINT.
.hold
    jsr golfer_s_update0_hold
    rts ; EXIT POINT.
.hold2
    jsr golfer_s_update0_hold2
    rts ; EXIT POINT.
.backswing
    jsr golfer_s_animate_swing
    bcc +
    jsr golfer_s_init_animate_hold
+
    rts ; EXIT POINT.
.hold_animate
    jsr golfer_s_update0_hold_animate   
    rts ; EXIT POINT.
.foreswing
    jsr golfer_s_animate_swing
    bcc +
    +golfer_m_init_hold golfer_c_HOLD_TIME_SWING_COMPLETE,golfer_c_STATE_ANIMATE_HOLD2
+
    rts ; EXIT POINT.
.animate_hold2
    jsr golfer_s_update0_hold2_animate
    rts ; EXIT POINT.
.backswing_putt
    ldx #1
    jsr golfer_advance_putting_animation
    bcc +
    +golfer_m_init_hold golfer_c_HOLD_DURATION,golfer_c_STATE_ANIMATE_HOLD_PUTT
+
    rts ; EXIT POINT.
.hold_putt
    jsr golfer_s_update0_hold_putt
    rts ; EXIT POINT.
.foreswing_putt
    ldx #1
    jsr golfer_advance_putting_animation
    bcc +
    lda #golfer_c_STATE_FINISHED
    sta golfer_v_current_state
+
    rts ; EXIT POINT.


;    cmp #GOLFER_TOP_OF_SWING
;    beq .top_of_swing
;    cmp #GOLFER_SWINGING_FORWARD
;    beq .swinging_forward
;;    bne ++
;;+
;;    jsr powbar_update
;;++
;;    rts
;
;;+
;    cmp #GOLFER_SWINGING_BACK
;    beq .swinging_back
;    cmp #GOLFER_TOP_OF_SWING
;    beq .top_of_swing
;    cmp #GOLFER_SWINGING_FORWARD
;    beq .swinging_forward
;    cmp #GOLFER_SWING_COMPLETE
;    beq .complete
;    cmp #GOLFER_WATCHING_SHOT
;    beq .watching
;    cmp #GOLFER_PUTT_READY
;    bcc +
;    cmp #GOLFER_PUTT_HOLDING+1
;;    bcs +
;    beq +
;    jsr golfer_update0_putt
;+
;    ;cmp #GOLFER_PUTT_LOOKING_UP
;    ;cmp #GOLFER_PUTT_POWERING_UP
;    ;cmp #GOLFER_PUTT_ANIMATING
;    ;cmp #GOLFER_WATCHING_SHOT2
;    ;cmp #GOLFER_FINISHED
;    rts
;
;.swinging_back
;    jsr powarc_update
;    jsr golfer_animate_swing
;    bcs +
;    rts ; EXIT POINT - still swinging back.
;+
;    jsr golfer_finalize_power
;;    jsr ball_calc_initial_velocity
;;    jsr partsys_init
;    ; NOTE: this will send a message to the main thread that the particle
;    ; system needs initializing for the current shot.
;    inc partsys_current_state
;    jsr golfer_init_top_of_swing
;    jmp .end
;
;.top_of_swing
;;    jsr powbar_update
;    dec golfer_frame_count
;    beq +
;    rts
;+
;    lda #GOLFER_SWINGING_FORWARD
;    sta golfer_v_current_state
;    ;+clr golfer_frame_count
;    ldx #snd_c_SWISH
;    jsr snd_s_play_effect
;    ;jsr powarc_s_advance_precision
;    jmp .end
;
;.swinging_forward
;    jsr powarc_update
;    jsr golfer_animate_swing
;;    bcc .end
;    bcs +
;;    jsr powbar_update
;    jmp .end
;+
;    jsr golfer_init_swing_complete2
;    rts
;
;.complete
;    dec golfer_frame_count
;    bne +
;    jsr golfer_init_watching_shot1
;+
;    jsr powarc_update
;    rts
;
;.watching
;    rts
;
;.looking_up
;    ldx #0
;    jsr golfer_update_looking_up
;    jsr golfer_move_crosshair
;    rts ; EXIT POINT.
;
;.looking_down
;    ldx #0
;    jsr golfer_update_looking_down
;    jsr golfer_move_crosshair
;
;.end
;    rts
; end sub golfer_s_update_anim
} ; !zone

; **************************************************

; Check the controls here.
; TODO: joystick #1 or #2?!
; FIXME: TIDY THIS MESS UP!!!
!zone {
golfer_s_update1
    lda golfer_v_current_state
    cmp #golfer_c_STATE_READY
    beq .preswing
    cmp #golfer_c_STATE_LOOKING_UP
    beq .preswing
;    cmp #golfer_c_STATE_CONCEDE_QUERY
;    beq .concede_query
    cmp #golfer_c_STATE_POWERING_UP
    beq .powering_up
    cmp #golfer_c_STATE_PRECISION
    beq .precision
    cmp #golfer_c_STATE_FINISHED
    beq .finished
    rts

.preswing
    jsr golfer_s_update1_preswing
    rts ; EXIT POINT.

;.concede_query
;    jsr golfer_s_update1_concede_query
;    rts ; EXIT POINT.

.powering_up
    jsr golfer_s_update1_powering_up
    rts ; EXIT POINT.

.precision
    jsr golfer_s_update1_precision
    rts ; EXIT POINT.

.finished
    jsr golfer_s_update1_finished
    rts

;.backswing
;.foreswing
;    jsr golfer_handle_swing
;    rts
;.pre_putt
;    jsr golfer_update1_pre_putt
;    rts
;.putt_power
;    jsr golfer_update1_putt_power
;    rts
;.poised
;.putt_fore_stroke
;.putt_finished
;.finished
;    jsr golfer_update1_finished
;    rts
; end sub golfer_update1
} ; !zone

; **************************************************

;!zone {
;golfer_s_update2
;    jsr golfer_draw
;    rts
;; end sub golfer_s_update2
;} ; !zone

; **************************************************

!zone {
golfer_s_setup_draw
    ; Load in the correct sprite sequence (for animation).
    ldx round_v_current_player
    lda shared_v_player_genders,x
    asl
    tax
    lda round_v_must_putt
    +branch_if_false +
    inx
+
    jsr sstore_s_load_sequence


    ; TODO: custom colours?
    ; Set skin & hair color.
    ldx round_v_current_player
    lda shared_v_player_shirt_color_indices,x
    tay
    lda shared_l_PLAYER_SHIRT_COLORS,y
    pha
;    lda players_l_SHIRT_COLORS,x
;    pha
    lda shared_v_player_skin_tones,x
    tay
    lda shared_l_PLAYER_SKIN_TONES,y
    sta SPMC0
    sta golfer_v_current_skin_tone
    lda shared_l_PLAYER_HAIR_COLORS,y
    sta SPMC1

;    lda #BLUE
    pla
    sta spr_v_color+GOLFER_UPPER_BODY_SPR_NUM   
    lda #GREY1
    sta spr_v_color+GOLFER_LOWER_BODY_SPR_NUM
    lda #BLACK
    sta spr_v_color+GOLFER_CLUB_SHAFT_SPR_NUM
    ; Enable sprites #1-3.
;    lda SPENA
;    ora #%00001110
;    sta SPENA
;    lda #0
;    sta XXPAND

    lda #0
    sta golfer_has_hit_ball
    sta golfer_v_shot_spin_set
    sta golfer_anim_frame
    sta golfer_v_is_hidden
    sta golfer_v_concede_query_active
    sta golfer_v_p_key_locked
    sta golfer_v_punch_selected 
    sta golfer_v_kb_locked

    ; Crosshair.
    jsr golfer_reset_crosshair
    ; Particle system.
;    lda #PARTSYS_STATE_INACTIVE
;    sta partsys_current_state

    lda round_v_must_putt
    +branch_if_true .putt

    ; TODO: male or female?
    lda #sprd_c_GOLFER_SWING_UPPER 
;    clc
;    adc round_current_gender_offset 
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_SWING_LOWER 
    sta spr_v_current_ptr+GOLFER_LOWER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_SWING_CLUB
    sta spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM   
    ; Set positions.
    lda GOLFER_UPPER_BODY_POS_X
    sta spr_v_x_lo+GOLFER_UPPER_BODY_SPR_NUM
    lda GOLFER_UPPER_BODY_POS_Y
    sta spr_v_y+GOLFER_UPPER_BODY_SPR_NUM
    lda GOLFER_LOWER_BODY_POS_X
    sta spr_v_x_lo+GOLFER_LOWER_BODY_SPR_NUM
    lda #GOLFER_BASE_Y+21
    sta spr_v_y+GOLFER_LOWER_BODY_SPR_NUM
    lda GOLFER_CLUB_X
    sta spr_v_x_lo+GOLFER_CLUB_SHAFT_SPR_NUM 
    lda GOLFER_CLUB_Y
    sta spr_v_y+GOLFER_CLUB_SHAFT_SPR_NUM 

    lda #golfer_c_STATE_READY   ;GOLFER_READY
    sta golfer_v_current_state

    ; We borrow this to control modification in-game options.
    lda #1
;    sta titles_keyboard_locked
    sta golfer_keyboard_locked

    rts ; EXIT POINT.

.putt
    ; Initial set-up for putting.  First the sprite data.
    lda #sprd_c_GOLFER_PUTT_UPPER 
;    clc
;    adc round_current_gender_offset 
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_SWING_LOWER 
    sta spr_v_current_ptr+GOLFER_LOWER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_CLUB
    sta spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM   
    ; Now the positions.
    lda #GOLFER_PUTT_POS_X 
    sta spr_v_x_lo+GOLFER_UPPER_BODY_SPR_NUM
    sta spr_v_x_lo+GOLFER_LOWER_BODY_SPR_NUM
    lda #GOLFER_PUTT_POS_Y+3 
    sta spr_v_y+GOLFER_UPPER_BODY_SPR_NUM
    lda #GOLFER_PUTT_POS_Y+21 
    sta spr_v_y+GOLFER_LOWER_BODY_SPR_NUM
    lda #GOLFER_PUTT_CLUB_SHAFT_OFFSETS_X
    sta spr_v_x_lo+GOLFER_CLUB_SHAFT_SPR_NUM 
    lda #GOLFER_PUTT_CLUB_SHAFT_OFFSETS_Y
    sta spr_v_y+GOLFER_CLUB_SHAFT_SPR_NUM 

    lda #golfer_c_STATE_READY ;GOLFER_PUTT_READY
    sta golfer_v_current_state

    rts
; end sub golfer_s_setup_draw
} ; !zone

; **************************************************

!zone {
golfer_init2
    ; There are some global settings.
    ; Position of sprites #0 and #1 never changes and they're both
    ; multicolour.  We can also set the two global colours here -
    ; light red and red.
    ; NOTE: we're clearing out the sprite multicolor register here.
;    lda #7
;    lda #%1110
;    sta SPMC
    lda #LIGHT_RED
    sta SPMC0
    lda #BROWN  ;RED
    sta SPMC1

    ; Set positions.
;    lda GOLFER_UPPER_BODY_POS_X
;    sta spr_x_lo+GOLFER_UPPER_BODY_SPR_NUM
;    lda GOLFER_UPPER_BODY_POS_Y
;    sta spr_y+GOLFER_UPPER_BODY_SPR_NUM
;    lda GOLFER_LOWER_BODY_POS_X
;    sta spr_x_lo+GOLFER_LOWER_BODY_SPR_NUM
;    lda #GOLFER_BASE_Y+21
;    sta spr_y+GOLFER_LOWER_BODY_SPR_NUM
;    lda GOLFER_CLUB_X
;    sta spr_x_lo+GOLFER_CLUB_SHAFT_SPR_NUM 
;    lda GOLFER_CLUB_Y
;    sta spr_y+GOLFER_CLUB_SHAFT_SPR_NUM 
    ; NOTE: all X-positions have high byte=0.
    lda #0
    sta spr_v_x_hi+GOLFER_UPPER_BODY_SPR_NUM
    sta spr_v_x_hi+GOLFER_LOWER_BODY_SPR_NUM
    sta spr_v_x_hi+GOLFER_CLUB_SHAFT_SPR_NUM 
    sta spr_v_hires+GOLFER_UPPER_BODY_SPR_NUM
    sta spr_v_hires+GOLFER_LOWER_BODY_SPR_NUM
    sta spr_v_hires+GOLFER_CLUB_SHAFT_SPR_NUM 

;    lda #GOLFER_POS_X_LO
;    sta spr_x_lo
;    sta spr_x_lo+1
;    lda #GOLFER_SHADOW_POS_X_LO
;    sta spr_x_lo+2
;    lda #GOLFER_POS_X_HI
;    sta spr_x_hi
;    sta spr_x_hi+1
;    sta spr_x_hi+2
;    lda #GOLFER_POS_Y_UPPER
;    sta spr_y
;    lda #GOLFER_POS_Y_LOWER
;    sta spr_y+1
;    lda #GOLFER_SHADOW_POS_Y
;    sta spr_y+2

;    lda SPENA
;    ora #1<<GOLFER_POS_MARKER_SPR_NUM
;    sta SPENA
;    lda #GREY1
;    sta spr_v_color+GOLFER_POS_MARKER_SPR_NUM
    lda #sprd_c_CROSSHAIR
    sta spr_v_current_ptr+GOLFER_POS_MARKER_SPR_NUM
    lda #GOLFER_POS_MARKER_X 
    sta spr_v_x_lo+GOLFER_POS_MARKER_SPR_NUM
    lda #0
    sta spr_v_x_hi+GOLFER_POS_MARKER_SPR_NUM
    lda #GOLFER_POS_MARKER_Y 
    sta spr_v_y+GOLFER_POS_MARKER_SPR_NUM
    lda #GOLFER_POS_MARKER_VX_LO
    sta spr_v_vx_lo+GOLFER_POS_MARKER_SPR_NUM 
    lda #GOLFER_POS_MARKER_VX_HI
    sta spr_v_vx_hi+GOLFER_POS_MARKER_SPR_NUM 

    ; And finally, the ball...
;    jsr ball_init
;    sec
;    jsr ball_draw

    ; Make sure we don't try to handle any joystick input while the first
    ; hole is being rendered!
;    lda #GOLFER_IN_LIMBO
;    sta golfer_v_current_state

    rts
; end sub golfer_init2
} ; !zone

; **************************************************

;!zone {
;golfer_handle_swing
;    ldx joy_v_current_port 
;
;    lda golfer_v_current_state
;    cmp #GOLFER_SWINGING_BACK
;    beq .swinging_back
;    cmp #GOLFER_SWINGING_FORWARD
;    beq .swinging_forward
;    rts ; For now...
;
;.swinging_back
;    lda golfer_shot_power_finalized
;    +branch_if_true .end
;
;    ; Let's see if the player has released the joystick button.
;    +joy_m_is_fire
;    bne +
;    ; Still pressing fire.
;    lda golfer_v_shot_power
;    clc
;    adc #golfer_c_POWER_INCREMENT
;    sta golfer_v_shot_power
;    rts ; EXIT POINT.
;
;+   ; Player has released the joystick button, so power should now be
;    ; finalized.
;    inc golfer_shot_power_finalized
;    +golfer_set_club_color GOLFER_CLUB_NORMAL_COLOR
;    
;    ; Modify swing animation if our current frame < penultimate...
;    ; NOTE: load pre-penultimate frame into .X and compare against current
;    ;       frame.  If pre-penultimate is >= current, adjust animation.
;;    ldx #SPRD_GOLFER_MALE_UPPER+SPRD_GOLFER_BACKSWING_FRAMES-4
;;    cpx spr_current_ptr
;;    bcc .end
;;    ; Shorten the animation sequence so golfer executes a less powerful 
;;    ; swing (visually).
;;    inx ; I.e. go to penultimate frame.
;;    stx spr_anim_end_ptr
;    rts ; EXIT POINT.
;
;.swinging_forward
;    lda golfer_v_shot_spin_set
;    +branch_if_true .check_hit
;
;    +joy_m_is_fire
;    ; If player isn't pressing fire button, make sure it's unlocked.
;    bne .unlock
;    ; We acknowledge the fire button only if it's unlocked.
;    +joy_m_is_locked_fire
;    +branch_if_true .check_hit
;;    inc powbar_spin_set
;    inc golfer_v_shot_spin_set
;    
;    lda powarc_v_step_count
;    sta powarc_v_precision_offset 
;
;    +golfer_set_club_color GOLFER_CLUB_NORMAL_COLOR 
;    jmp .check_hit
;.unlock
;    +joy_m_release_fire
;.check_hit
;    ; BUG: whether ball is launched or not depends on the current frame
;    ; and the state of the ball (not in flight).  But if the power is very
;    ; weak and the ball lands (and becomes stationary) while we're still
;    ; on this animation frame, it will be launched a second time (?!)...
;    ; SOLUTION: make use of 'golfer_has_hit_ball' variable!!!
;    lda spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM   
;    sec
;    sbc round_current_gender_offset
;    cmp #SPRD_GOLFER2_SWING_UPPER+SPRD_GOLFER2_CONTACT_OFFSET 
;    bcc .end
;    lda golfer_has_hit_ball
;    +branch_if_true .end
;    inc golfer_has_hit_ball
;    ; NOTE: there isn't enough time here to launch the ball without
;    ; occasional screen flicker.  So just set a flag and it'll be launched
;    ; (i.e. 'ball_s_launch' will be called) when the raster is at the bottom
;    ; of the screen.
;;    jsr ball_launch
;    inc ball_v_must_launch
;    jsr partsys_start
;
;.end
;    rts
;; end sub golfer_handle_swing
;} ; !zone

; **************************************************

;!zone {
;golfer_finalize_power
;    lda golfer_shot_power_finalized
;    +branch_if_true +
;    ; Player is still holding down fire button at very top of backswing, so
;    ; set power to maximum (= 0).
;    +clr golfer_v_shot_power
;    +golfer_set_club_color GOLFER_CLUB_NORMAL_COLOR 
;+
;;    lda powarc_step_count
;;    sta powarc_power_offset
;    ; TODO: this works but messes up putting!
;;    ldx powarc_step_count
;;    lda POWARC_POWER_TABLE,x
;;    sta golfer_shot_power
;
;    rts
;; end sub golfer_finalize_power
;} ; !zone

; **************************************************

;; FIXME: use a lookup table to avoid code duplication!
;!zone {
;golfer_init_top_of_swing
;    lda #GOLFER_HOLD_TIME
;    sta golfer_v_frame_count
;    lda #GOLFER_TOP_OF_SWING
;    sta golfer_v_current_state
;    lda #0
;    sta golfer_v_shot_spin_set
;
;    ; Might as well prepare the animation here as well (for the down-swing).
;    ; Full- or half-swing?
;;    lda spr_anim_end_ptr
;;    cmp #SPRD_GOLFER_MALE_UPPER+7
;;    beq .full
;;
;;    ; So half-swing...
;;    lda #SPRD_GOLFER_MALE_UPPER+9
;;    sta spr_anim_start_ptr
;;    sta spr_current_ptr
;;    lda #SPRD_GOLFER_MALE_UPPER+13
;;    sta spr_anim_end_ptr
;;    lda #SPRD_GOLFER_MALE_LOWER+9 
;;    sta spr_anim_start_ptr+1
;;    sta spr_current_ptr+1
;;    lda #SPRD_GOLFER_MALE_LOWER+13
;;    sta spr_anim_end_ptr+1
;;    jmp +
;;
;;.full
;;    lda #SPRD_GOLFER_MALE_UPPER+7
;;    sta spr_anim_start_ptr
;;    sta spr_current_ptr
;;    lda #SPRD_GOLFER_MALE_UPPER+14
;;    sta spr_anim_end_ptr
;;    lda #SPRD_GOLFER_MALE_LOWER+7 
;;    sta spr_anim_start_ptr+1
;;    sta spr_current_ptr+1
;;    lda #SPRD_GOLFER_MALE_LOWER+14 
;;    sta spr_anim_end_ptr+1
;;
;;+
;;    lda #1
;;    sta spr_anim_seq_inc
;;    sta spr_anim_seq_inc+1
;;    lda #GOLFER_FORESWING_FRAME_RATE
;;    sta spr_anim_timer
;;    sta spr_anim_timer+1
;;    sta spr_framerate
;;    sta spr_framerate+1
;
;    ; Torso.
;    lda #sprd_c_GOLFER_SWING_UPPER+sprd_c_GOLFER_MIDSWING_OFFSET 
;;    clc
;;    adc round_current_gender_offset 
;    sta spr_v_anim_start_ptr+GOLFER_UPPER_BODY_SPR_NUM    
;    lda #sprd_c_GOLFER_SWING_UPPER+sprd_c_GOLFER_MIDSWING_OFFSET+sprd_c_GOLFER_FORESWING_FRAMES-1 
;;    clc
;;    adc round_current_gender_offset 
;    sta spr_v_anim_end_ptr+GOLFER_UPPER_BODY_SPR_NUM        
;    ; Legs.
;    lda #sprd_c_GOLFER_SWING_LOWER+sprd_c_GOLFER_MIDSWING_OFFSET 
;    sta spr_v_anim_start_ptr+GOLFER_LOWER_BODY_SPR_NUM    
;    lda #sprd_c_GOLFER_SWING_LOWER+sprd_c_GOLFER_MIDSWING_OFFSET+sprd_c_GOLFER_FORESWING_FRAMES-1
;    sta spr_v_anim_end_ptr+GOLFER_LOWER_BODY_SPR_NUM        
;;    lda #180
;;    sta spr_anim_start_ptr+1 
;;    lda #183
;;    sta spr_anim_end_ptr+1        
;    ; Club.
;    lda #sprd_c_GOLFER_SWING_CLUB+sprd_c_GOLFER_MIDSWING_OFFSET 
;    sta spr_v_anim_start_ptr+GOLFER_CLUB_SHAFT_SPR_NUM    
;    lda #sprd_c_GOLFER_SWING_CLUB+sprd_c_GOLFER_MIDSWING_OFFSET+sprd_c_GOLFER_FORESWING_FRAMES-1
;    sta spr_v_anim_end_ptr+GOLFER_CLUB_SHAFT_SPR_NUM        
;
;    lda #1
;    sta spr_v_anim_seq_inc+GOLFER_CLUB_SHAFT_SPR_NUM           
;    sta spr_v_anim_seq_inc+GOLFER_UPPER_BODY_SPR_NUM        
;    sta spr_v_anim_seq_inc+GOLFER_LOWER_BODY_SPR_NUM        
;
;    lda #GOLFER_FORESWING_FRAME_RATE
;    sta spr_v_anim_timer+GOLFER_CLUB_SHAFT_SPR_NUM                     
;    sta spr_v_anim_timer+GOLFER_UPPER_BODY_SPR_NUM                 
;    sta spr_v_anim_timer+GOLFER_LOWER_BODY_SPR_NUM                  
;    sta spr_v_framerate+GOLFER_CLUB_SHAFT_SPR_NUM                                
;    sta spr_v_framerate+GOLFER_UPPER_BODY_SPR_NUM                            
;    sta spr_v_framerate+GOLFER_LOWER_BODY_SPR_NUM       
;    ; Player has to release fire button at top of swing for 'spin'
;    ; to be acknowledged - when fire button is pressed again later.
;    ldx joy_v_current_port
;    +joy_m_lock_fire
;
;    jsr powarc_s_init_precision
;
;    rts
;; end sub golfer_init_top_of_swing
;} ; !zone

; **************************************************

;!zone {
;golfer_init_swing_complete
;    jsr golfer_init_swing_complete2
;    rts
;
;    ; First check whether spin is set at all...
;    lda golfer_shot_spin_set
;    +branch_if_false .max_slice
;
;    ; Calculation: (when fire button pressed) - (number of frames to contact).
;    lda golfer_shot_spin
;    sec
;    sbc #28 ;golfer_frame_count
;    bmi .hook
;    beq .no_spin
;
;    ; Slice.  Value in .A is positive.  Beyond threshold = max slice.
;    cmp #GOLFER_SPIN_THRESHOLD+1
;    bcc +
;.max_slice
;    lda #GOLFER_SPIN_THRESHOLD
;+   sta ball_spin_i
;    lda #BALL_SPIN_SLICE
;    sta ball_spin_type
;    jmp .update_state
;
;.hook
;    ; Value negative so first let's take 2's comp.
;    +nega
;    cmp #GOLFER_SPIN_THRESHOLD+1
;    bcc +
;    lda #GOLFER_SPIN_THRESHOLD
;+   sta ball_spin_i
;    lda #BALL_SPIN_HOOK
;    sta ball_spin_type
;    jmp .update_state
;
;.no_spin
;    lda #BALL_SPIN_NONE
;    sta ball_spin_type
;    +clr ball_spin_i
;
;.update_state
;    inc golfer_v_current_state
;    lda #GOLFER_HOLD_TIME_SWING_COMPLETE
;    sta golfer_frame_count
;    jsr ball_init_spin
;
;    rts
;; end sub golfer_init_swing_complete
;} ; !zone

; **************************************************

;!zone {
;golfer_init_swing_complete2
;    lda powarc_v_step_count
;    sec
;    sbc #POWARC_PRECISION_BEGIN_STEP
;    bcc .mishit
;    cmp #POWARC_PRECISION_MID_STEP-POWARC_PRECISION_BEGIN_STEP
;    beq .straight
;    bcc .hook
;
;    ; Slice!
;    ; Need to do: (12-step) so index is valid.
;    sta MATHS0
;    lda #POWARC_PRECISION_END_STEP-POWARC_PRECISION_BEGIN_STEP
;    ; C flag already set.
;    sbc MATHS0
;    sta ball_spin_i
;    lda #BALL_SPIN_SLICE
;    sta ball_spin_type
;    jmp .update_state
;
;.mishit
;    ; TODO: handle mishit!
;
;.straight
;    lda #BALL_SPIN_NONE  
;    sta ball_spin_type
;    ; FIXME: is this necessary?!
;    +clr ball_spin_i
;    jmp .update_state
;
;.hook
;    sta ball_spin_i
;    lda #BALL_SPIN_HOOK
;    sta ball_spin_type
;
;.update_state
;    inc golfer_v_current_state
;    lda #GOLFER_HOLD_TIME_SWING_COMPLETE
;    sta golfer_v_frame_count
;    jsr ball_init_spin
;
;    rts
;; end sub golfer_init_swing_complete2
;} ; !zone

; **************************************************

!zone {
golfer_init_watching_shot1
    lda #golfer_c_STATE_FINISHED
    sta golfer_v_current_state

    lda #sprd_c_GOLFER_WATCHING_SWING_UPPER
;    clc
;    adc round_current_gender_offset 
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM
    lda #sprd_c_GOLFER_WATCHING_SWING_CLUB
    sta spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
    lda #GOLFER_WATCHING_UPPER_OFFSET_X
    sta spr_v_x_lo+GOLFER_UPPER_BODY_SPR_NUM
    lda #GOLFER_WATCHING_UPPER_OFFSET_Y 
    sta spr_v_y+GOLFER_UPPER_BODY_SPR_NUM
    lda #GOLFER_WATCHING_CLUB_OFFSET_X 
    sta spr_v_x_lo+GOLFER_CLUB_SHAFT_SPR_NUM
    lda #GOLFER_WATCHING_CLUB_OFFSET_Y 
    sta spr_v_y+GOLFER_CLUB_SHAFT_SPR_NUM 

;    inc golfer_v_current_state
;    lda #SPRD_GOLFER_WATCHING_SHOT_UPPER
;    sta spr_current_ptr
;    lda #SPRD_GOLFER_WATCHING_SHOT_LOWER
;    sta spr_current_ptr+1
    rts
; end sub golfer_init_watching_shot1
} ; !zone

; **************************************************

; OUTPUTS:  C flag set if should notify player of distance; otherwise clear.
!zone {
golfer_s_record_distance
    +clr round_v_current_player_is_on_green

    ; Whatever happens later, we'll always need to add a shot.
    ; Update the display afterwards as well.
    ldx round_v_current_player
    inc players_v_current_shots,x

    ; No need to calculate distance if the ball's already in the hole!
    lda ball_v_is_in_hole
    +branch_if_false +

    jsr golfer_s_evaluate_hole

    ; Write our score to the score card UNLESS MATCH PLAY!
    lda shared_v_is_match_play
    +branch_if_true .dont_write
    ldx round_v_current_player
    jsr sc_s_write
.dont_write
    ; We record the fact that the current player has finished the hole by 
    ; setting their 'current_shots' value to $ff (i.e. negative).
    ldx round_v_current_player
    lda #$ff
    sta players_v_current_shots,x
    ; Display this golfer's updated score straight away (unless match play).
    ; NOTE: X already holds player #.
    lda shared_v_is_match_play
    +branch_if_true .skip_display_score
    lda #1
    sta P0
    sta P1
    jsr sc_s_draw_name_and_score
.skip_display_score
    jsr stats_s_inc_balls_holed
    clc
    rts ; EXIT POINT.

+
    ; If ball's out of bounds or in water it hasn't moved unless,
    ; exceptionally, we're allowed to land on water.
    lda ball_v_out_of_bounds    
    +branch_if_true .end    ;.no_distance
    lda ball_v_current_terrain
    cmp #ball_c_TERRAIN_WATER
    bne +
    lda round_v_allow_water
    +branch_if_true +
;.no_distance
;    clc
    sec
    rts ; EXIT POINT.

+
    ; NOTE: X still holds value of round_v_current_player.
    lda target_z_lo
    sta players_v_distance_lo,x
    lda target_z_hi
    sta players_v_distance_hi,x
    lda ball_v_current_terrain
    sta players_v_terrain,x

    ; NOTE: terrain in accumulator.
    cmp #ball_c_TERRAIN_GREEN_FWAY
    bne .end
    lda target_z_lo
    cmp #round_c_MAX_PUTT_DISTANCE_LO 
    lda target_z_hi
    sbc #round_c_MAX_PUTT_DISTANCE_HI 
    bcs .end
    inc round_v_current_player_is_on_green

.end
    sec
    rts
; end sub golfer_s_record_distance
} ; !zone

; **************************************************

; Enable the sprite and reset its x-position to the center of
; the screen.
!zone {
golfer_reset_crosshair
;    lda SPENA
;    ora #1<<GOLFER_POS_MARKER_SPR_NUM 
;    sta SPENA

    ldx #golfer_c_MARKER_INITIAL_INDEX
    stx golfer_v_marker_index
    lda golfer_l_MARKER_X_POS2,x
    sta spr_v_x_lo+GOLFER_POS_MARKER_SPR_NUM 
    lda #0
    sta spr_v_x_hi+GOLFER_POS_MARKER_SPR_NUM 
    lda golfer_l_MARKER_Y_POS2,x
    sta spr_v_y+GOLFER_POS_MARKER_SPR_NUM 

    lda #sprd_c_CROSSHAIR
    sta spr_v_current_ptr+GOLFER_POS_MARKER_SPR_NUM 

    lda #1
    sta golfer_v_crosshair_active
    lda #GOLFER_POS_MARKER_VX_LO
    sta spr_v_vx_lo+GOLFER_POS_MARKER_SPR_NUM 

    lda #golfer_c_MARKER_MOVE_DELAY_NORMAL
    sta golfer_v_marker_move_delay
    lda #GREY1
    sta spr_v_color+GOLFER_POS_MARKER_SPR_NUM

    rts
; end sub golfer_reset_crosshair
} ; !zone

; **************************************************

;!zone {
;golfer_move_crosshair
;    lda golfer_pos_marker_direction
;    ; Skip over this if no movement (i.e. value negative).
;    bmi .pulse
;    pha
;
;    ; If putting, may need to change velocity depending on whether player is
;    ; pulling down on the joystick.
;    lda round_v_must_putt
;    +branch_if_false .move
;    ldx joy_v_current_port
;    +joy_m_is_down
;    bne +
;    ; Should be slow.
;    lda golfer_v_slow_crosshair
;    +branch_if_true .move
;-
;    jsr golfer_s_change_crosshair_speed
;    jmp .move
;+
;    ; Should be normal.
;    lda golfer_v_slow_crosshair
;    +branch_if_true -
;
;.move
;    ldx #GOLFER_POS_MARKER_SPR_NUM 
;    jsr spr_s_calc_dx
;    pla
;    cmp #spr_c_LEFT
;    beq .left
;    jsr spr_s_move_right
;    lda #GOLFER_POS_MARKER_MAX_X 
;    sta spr_v_edge
;    jsr spr_s_clamp_right
;    jmp .pulse
;.left
;    jsr spr_s_move_left
;    lda #GOLFER_POS_MARKER_MIN_X 
;    sta spr_v_edge
;    jsr spr_s_clamp_left
;
;.pulse
;    dec golfer_crosshair_pulse_count
;    bpl .end
;    lda #GOLFER_CROSSHAIR_PULSE_DELAY
;    sta golfer_crosshair_pulse_count
;    inc golfer_crosshair_pulse_index
;    ldx golfer_crosshair_pulse_index
;    cpx #GOLFER_CROSSHAIR_COLORS_N
;    bne +
;    ldx #0
;    stx golfer_crosshair_pulse_index
;+
;    lda GOLFER_CROSSHAIR_COLORS,x
;    sta spr_v_color+GOLFER_POS_MARKER_SPR_NUM 
;
;.end
;    rts
;; end sub golfer_move_crosshair
;} ; !zone

; **************************************************

!zone {
golfer_s_update1_preswing
    lda shared_v_is_match_play
    +branch_if_false +
    jsr golfer_s_update1_concede_query
    bcc +
    ; Concede query in progress so don't check for anything else just yet.
    rts ; EXIT POINT.

+
    ldx joy_v_current_port
    lda round_v_must_putt
    +branch_if_false .not_putting
    ; We are putting.  Checks pertinent to the greens, then straight onto 
    ; checking horizontal axis for crosshair positioning.
    jsr golfer_s_check_power_speed
    jsr golfer_s_check_hidden
    jmp .check_horizontal

.not_putting
!if _DEBUG_PHYSICS_VARS_ {
    jsr golfer_s_update_physics_config
} ; !if

    ; Not yet on the green.  As long as we're not teeing off, or in the sand,
    ; player may select/deselect a punch shot.
    lda round_v_teeing_off
    +branch_if_true +
    lda ball_v_current_terrain
    cmp #ball_c_TERRAIN_BUNKER
    beq +
    jsr golfer_s_check_punch_toggle

+
    ; NOTE: have to reload current port into X - trashed during call to 
    ; 'golfer_s_check_punch_toggle'.
    ldx joy_v_current_port
    ; Swing not yet initiated - player may position crosshair and select club.
    +joy_m_is_up
    bne .check_down
    +joy_m_is_locked_up
    beq +
    rts ; EXIT POINT - tried 'up' but locked.
+
    +joy_m_lock_up
    jsr clubs_s_prev
    jsr clubs_s_draw2
    rts ; EXIT POINT - previous club selected.

.check_down
    +joy_m_is_down
    bne .unlock
    +joy_m_is_locked_down
    +branch_if_true .end
    +joy_m_lock_down
    jsr clubs_s_next
    jsr clubs_s_draw2
    rts ; EXIT POINT - next club selected.

.unlock
    +joy_m_release_vertical

.check_horizontal
    ; Check horizontal.
    +joy_m_is_left
    bne .check_right
    ldx #spr_c_LEFT
    jsr golfer_s_init_marker_move
    rts ; EXIT POINT.
    
.check_right
    +joy_m_is_right
    bne .crosshair_still
    ldx #spr_c_RIGHT
    jsr golfer_s_init_marker_move
    rts ; EXIT POINT.

.crosshair_still
    lda #$ff
    sta golfer_pos_marker_direction

    ; No directional input.  Does player wish to intiate swing?
    ; NOTE: don't allow this if golfer is hidden!
    lda golfer_v_is_hidden
    +branch_if_true .end
    +joy_m_is_fire
    bne .unlock_fire
    +joy_m_is_locked_fire
    +branch_if_true .end
    lda #golfer_c_STATE_POWERING_UP
    sta golfer_v_current_state
    +clr golfer_v_crosshair_active
    ; FIXME: hack to hide crosshair!
    lda #0
    sta SP0Y+(2*GOLFER_CROSSHAIR_SPR_NUM) 
    rts ; EXIT POINT.

.unlock_fire
    +joy_m_release_fire

.end
    rts
; end sub golfer_update1_preswing
} ; !zone

; **************************************************

!zone {
golfer_s_update1_finished
    lda round_v_current_state
    cmp #round_c_STATE_SHOT_COMPLETE
    beq +
    rts ; EXIT POINT.

+
    ldx joy_v_current_port
    ; We're just listening out for the fire button.
    +joy_m_is_fire
    bne .unlock
    +joy_m_is_locked_fire
    +branch_if_true .end
    +joy_m_lock_fire

    ldx #round_c_MSG_NEXT_SHOT
    jsr round_s_record_msg
    ; Do nothing (yet) if msg isn't accepted.
    bcc .end
    ; Don't want to handle any more input from golfer from now on, so change
    ; state to 'IN_LIMBO'.
    lda #golfer_c_STATE_IN_LIMBO
    sta golfer_v_current_state
    rts ; EXIT POINT.

.unlock
    +joy_m_release_fire
.end
    rts
; end sub golfer_s_update1_finished
} ; !zone

; **************************************************

!zone {
golfer_update1_putt_power
    lda golfer_shot_power_finalized
    +branch_if_true .end

    jsr powarc_s_update

    ldx joy_v_current_port
    +joy_m_is_fire
    bne .finalize
    ; Player is still pressing button, so increment shot power.
    lda golfer_v_shot_power
    clc
    adc #GOLFER_PUTT_POWER_INCREMENT
    sta golfer_v_shot_power
    rts ; EXIT POINT.

.finalize
    inc golfer_shot_power_finalized
    +golfer_set_club_color GOLFER_CLUB_NORMAL_COLOR

.end
    rts
; end sub golfer_update1_putt_power
} ; !zone

; **************************************************

!zone {
golfer_s_update_looking_up
    dec golfer_look_up_count
    bne .end

    lda round_v_must_putt
    +branch_if_true .putt

    ; So swing...
    lda GOLFER_UPPER_BODY_POS_Y
    sta spr_v_y+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_SWING_UPPER 
    bne +

.putt
    ; FIXME: a bit messy!  Could alternatively set a variable somewhere?!
    ; If the golfer is hidden, update looking up/down as usual but write the
    ; new sprite pointer to the buffer, rather than to the sprite engine.  In
    ; this way, we'll see the change when the sprite pointers are restored.
    lda #sprd_c_GOLFER_PUTT_UPPER
    ldx golfer_v_is_hidden
    beq +
    sta golfer_v_spr_pointer_buffer+1
    bne ++
+
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM   
++
    lda #golfer_c_STATE_READY
    sta golfer_v_current_state
    +clr golfer_look_up_count

.end
    rts
; end sub golfer_s_update_looking_up
} ; !zone

; **************************************************

!zone {
golfer_s_update_looking_down
    jsr rand_s_get
    cmp #$40
    bcs .end
    inc golfer_look_up_count
    lda golfer_look_up_count
    cmp #50
    bcc .end

    lda round_v_must_putt
    +branch_if_true .putt

    ; So swing...
    lda #GOLFER_UPPER_BODY_LOOKING_UP_POS_Y 
    sta spr_v_y+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_LOOKING_UP  
    bne +

.putt
    ; So, putt...
    lda #sprd_c_GOLFER_PUTT_LOOKING_UP
    ldx golfer_v_is_hidden
    beq +
    sta golfer_v_spr_pointer_buffer+1
    bne ++
+
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM   
++
    lda #golfer_c_STATE_LOOKING_UP
    sta golfer_v_current_state

.end
    rts
; end sub golfer_s_update_looking_down
} ; !zone

; **************************************************

!zone {
golfer_init_poised
    lda #GOLFER_HOLD_TIME*5
    sta golfer_v_frame_count
    inc golfer_v_current_state

    ; Set up second half of putting animation.
    lda #sprd_c_GOLFER_PUTT_UPPER+sprd_c_GOLFER_PUTT_BACKSWING_FRAMES-1
    sta spr_v_anim_start_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_CLUB+sprd_c_GOLFER_PUTT_BACKSWING_FRAMES-1 
    sta spr_v_anim_start_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
    lda #sprd_c_GOLFER_PUTT_UPPER
    sta spr_v_anim_end_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_CLUB
    sta spr_v_anim_end_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
    lda #0
    sta spr_v_anim_seq_inc+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_seq_inc+GOLFER_CLUB_SHAFT_SPR_NUM
    lda #GOLFER_PUTTING_FRAME_RATE
    sta spr_v_anim_timer+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_timer+GOLFER_CLUB_SHAFT_SPR_NUM
    sta spr_v_framerate+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_framerate+GOLFER_CLUB_SHAFT_SPR_NUM
    ; Index into club shaft position offsets table.
    lda #3
    sta golfer_anim_frame   

    rts
; end sub golfer_init_poised
} ; !zone

; **************************************************

!zone {
golfer_s_store_new_ball_position
    ldx round_v_current_player
    lda ball_world_x_lo
    sta players_v_ball_pos_x_lo,x
    lda ball_world_x_hi
    sta players_v_ball_pos_x_hi,x
    lda ball_world_z_lo
    sta players_v_ball_pos_z_lo,x
    lda ball_world_z_hi
    sta players_v_ball_pos_z_hi,x
    rts
; end sub golfer_s_store_new_ball_position
} ; !zone

; **************************************************

; OUTPUTS:  C flag set if animation complete; otherwise clear.
!zone {
golfer_s_animate_swing
    ldx #GOLFER_CLUB_SHAFT_SPR_NUM   
    jsr spr_s_animate_onetime
    ldx #GOLFER_UPPER_BODY_SPR_NUM   
    jsr spr_s_animate_onetime
    ldx #GOLFER_LOWER_BODY_SPR_NUM   
    jsr spr_s_animate_onetime
    bcs .animation_complete

    ; NOTE: following flag is set if we advanced to the next frame in
    ; latest call to spr_s_animate_onetime.
    lda spr_v_is_next_frame
    +branch_if_false .still_animating

    inc golfer_anim_frame
    lda golfer_anim_frame
    pha
    cmp #7
    bne +
    ldy #sfx_c_SWISH
    jsr snd_s_init_sfx
+
    pla
    tax
    lda GOLFER_UPPER_BODY_POS_X,x
    sta spr_v_x_lo+GOLFER_UPPER_BODY_SPR_NUM   
    lda GOLFER_UPPER_BODY_POS_Y,x
    sta spr_v_y+GOLFER_UPPER_BODY_SPR_NUM   
    lda GOLFER_LOWER_BODY_POS_X,x
    sta spr_v_x_lo+GOLFER_LOWER_BODY_SPR_NUM   
    lda GOLFER_CLUB_X,x
    sta spr_v_x_lo+GOLFER_CLUB_SHAFT_SPR_NUM   
    lda GOLFER_CLUB_Y,x
    sta spr_v_y+GOLFER_CLUB_SHAFT_SPR_NUM   

    ; Must the ball be launched (and particle system started)?
    lda spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM
    cmp #sprd_c_GOLFER_SWING_UPPER+sprd_c_GOLFER_CONTACT_OFFSET 
    bne .still_animating
    sta ball_v_must_launch

.still_animating
    clc
    rts

.animation_complete
    rts
; end sub golfer_s_animate_swing
} ; !zone
; **************************************************

;!zone {
;golfer_animate_putt
;    lda golfer_v_current_state
;    cmp #GOLFER_PUTT_POWERING_UP
;    bne +
;    jmp .powering_up
;+
;    cmp #GOLFER_PUTT_POISED
;    bne +
;    jmp .poised
;+
;    cmp #GOLFER_PUTT_FORE_STROKE_0
;    bne +
;    jmp .fore_stroke0
;+
;    cmp #GOLFER_PUTT_FORE_STROKE_1
;    bne +
;    jmp .fore_stroke1
;+
;    ; Unknown state?!
;    rts
;
;.powering_up
;    ldx #1
;    jsr golfer_advance_putting_animation
;    bcc +
;    ; Animation complete.
;    jsr golfer_finalize_power
;    jsr ball_s_calc_initial_velocity
;    jsr golfer_init_poised
;+
;    rts ; EXIT POINT.
;
;.poised
;    dec golfer_v_frame_count
;    bne +
;    lda #GOLFER_PUTT_FORE_STROKE_0   
;    sta golfer_v_current_state
;+
;    rts ; EXIT POINT.
;
;.fore_stroke0
;    ldx #(-1)
;    jsr golfer_advance_putting_animation
;    bcc +
;    ; Prepare second part of forestroke animation.
;    lda #SPRD_GOLFER2_PUTT_UPPER+SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT0  
;    clc
;    adc round_current_gender_offset
;    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM
;    sta spr_v_anim_start_ptr+GOLFER_UPPER_BODY_SPR_NUM
;    lda #SPRD_GOLFER2_PUTT_SHAFT+SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT0  
;    sta spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
;    sta spr_v_anim_start_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
;    lda #SPRD_GOLFER2_PUTT_UPPER+SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT0+SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT1-1  
;    clc
;    adc round_current_gender_offset
;    sta spr_v_anim_end_ptr+GOLFER_UPPER_BODY_SPR_NUM
;    lda #SPRD_GOLFER2_PUTT_SHAFT+SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT0+SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT1-1  
;    sta spr_v_anim_end_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
;    lda #1
;    sta spr_v_anim_seq_inc+GOLFER_UPPER_BODY_SPR_NUM
;    sta spr_v_anim_seq_inc+GOLFER_CLUB_SHAFT_SPR_NUM
;    lda #GOLFER_PUTTING_FRAME_RATE
;    sta spr_v_anim_timer+GOLFER_UPPER_BODY_SPR_NUM   
;    sta spr_v_anim_timer+GOLFER_CLUB_SHAFT_SPR_NUM
;    sta spr_v_framerate+GOLFER_UPPER_BODY_SPR_NUM   
;    sta spr_v_framerate+GOLFER_CLUB_SHAFT_SPR_NUM
;    lda #4
;    sta golfer_anim_frame
;    tax
;    lda GOLFER_PUTT_CLUB_SHAFT_OFFSETS_X,x
;    sta spr_v_x_lo+GOLFER_CLUB_SHAFT_SPR_NUM
;    lda GOLFER_PUTT_CLUB_SHAFT_OFFSETS_Y,x
;    sta spr_v_y+GOLFER_CLUB_SHAFT_SPR_NUM
;    inc golfer_v_current_state
;    jsr ball_launch_putt
;+  
;    rts ; EXIT POINT.
;
;.fore_stroke1
;    ldx #1
;    jsr golfer_advance_putting_animation
;    bcc +
;;    lda #GOLFER_IN_LIMBO             
;    lda #GOLFER_FINISHED
;    sta golfer_v_current_state
;+
;    rts
;; end sub golfer_animate_putt
;} ; !zone

; **************************************************

; OUTPUTS:  C flag set if animation complete; else clear.
!zone {
golfer_advance_putting_animation
    ldx #GOLFER_UPPER_BODY_SPR_NUM
    jsr spr_s_animate_onetime
    ldx #GOLFER_CLUB_SHAFT_SPR_NUM
    jsr spr_s_animate_onetime
    bcc +
    rts ; EXIT POINT - animation sequence has ended.

+
    lda spr_v_is_next_frame
    +branch_if_false .end
    
    ; Check if ball needs to be launched (- i.e. club head has come into
    ; contact with it)...
    lda golfer_v_current_state
    cmp #golfer_c_STATE_ANIMATE_FORESWING_PUTT
    bne .end
    lda ball_v_current_state
    cmp #ball_c_STATE_STATIONARY
    bne .end
    lda spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM
    cmp #sprd_c_GOLFER_PUTT_UPPER+sprd_c_GOLFER_PUTT_CONTACT_OFFSET
    bne .end
    jsr ball_s_launch_putt

.end
    ; Make sure C flag clear so animation continues.
    clc
    rts
; end sub golfer_advance_putting_animation
} ; !zone

; **************************************************

;!zone {
;golfer_check_view_scorecards
;    jsr SCNKEY
;    ldx $cb
;    lda KB_MATRIX_DECODE_TBL,x 
;    bmi .unlock_keyboard
;    cmp #SCR_CODE_S+64
;    bne .end
;
;    ; Acknowledge if keyboard unlocked.
;    lda titles_keyboard_locked
;    +branch_if_true .end
;    inc EXTCOL
;    inc titles_keyboard_locked
;    rts ; EXIT POINT.
;
;.unlock_keyboard
;    +clr titles_keyboard_locked
;
;.end
;    rts
;; end sub golfer_check_view_scorecards
;} ; !zone

; **************************************************

; While player is measuring up the next shot, some in-game options may be set.
; F1 = toggle camera shake, F3 = change border color, F5 = change sky color
; OUTPUT:   C flag set if an option was changed, otherwise clear.
;!zone {
;golfer_check_options
;    jsr SCNKEY
;    ldx $cb
;    cpx #64
;    beq .unlock_keyboard
;
;;    lda titles_keyboard_locked
;    lda golfer_keyboard_locked
;    +branch_if_true .no_keypress
;
;    ; Here we have a keypress, so lock keyboard.
;;    inc titles_keyboard_locked
;    inc golfer_keyboard_locked
;
;    cpx #4
;    beq .f1
;    cpx #5
;    beq .f3
;    cpx #6
;    beq .f5
;    jmp .no_keypress
;
;.f1
;    lda #28
;    sta MATHS0
;    lda #3
;    sta MATHS1
;    lda #2
;    sta MATHS2
;
;    jsr round_toggle_camera_shake
;    sec
;    rts
;
;.f3
;    jsr round_inc_border_color
;    sec
;    rts
;.f5
;    jsr round_next_sky_color
;    sec
;    rts
;
;.unlock_keyboard
;    lda #0
;;    sta titles_keyboard_locked
;    sta golfer_keyboard_locked
;
;.no_keypress
;    clc
;    rts
;; end sub golfer_check_options
;} ; !zone

; **************************************************

;!zone {
;golfer_handle_power
;    lda powbar_power_on
;    beq +
;
;    ldx joy_current_port
;    +joy_m_is_fire
;    beq +
;
;    ; Must lock fire button so precision not immediately set.
;    +joy_m_lock_fire
;    ; Start precision.
;    +clr powbar_power_on
;;    inc golfer_v_current_state
;;    jsr powbar_init_precision
;    
;+
;    rts
;; end sub golfer_handle_power
;} ; !zone

; **************************************************

;!zone {
;golfer_start_precision
;    ldx joy_current_port
;    +joy_m_lock_fire
;    ;inc golfer_v_current_state
;    jsr powarc_init_precision
;    jsr golfer_init_top_of_swing
;    
;    rts
;; end sub golfer_start_precision
;} ; !zone

; **************************************************

;!zone {
;golfer_handle_precision
;    ldx joy_v_current_port
;
;    +joy_m_is_fire 
;    bne .unlock_fire
;    +joy_m_is_locked_fire
;    bne .end
;
;;    inc powbar_spin_set
;;    lda #GOLFER_FINISHED
;;    sta golfer_v_current_state
;    rts
;
;.unlock_fire
;    +joy_m_release_fire
;
;.end
;    rts
;; end sub golfer_handle_precision
;} ; !zone

; **************************************************

!zone {
golfer_s_update1_powering_up
    ldx joy_v_current_port

    lda powarc_v_power_maxed_out
    +branch_if_true .hold

    ; Fire-button release?
    +joy_m_is_fire
    beq .end

.hold
    +golfer_m_init_hold golfer_c_HOLD_DURATION,golfer_c_STATE_HOLD
    lda powarc_v_step_count
    sta powarc_v_power_offset
    jsr powarc_s_init_max_animation
    ; X may be trashed so load again.
    ldx joy_v_current_port
    +joy_m_lock_fire
    ; If this is a putt, need to send msg to ball module that initial velocity
    ; now needs to be calculated.  In case of swing, still need to set
    ; precision.
    lda round_v_must_putt
    +branch_if_false .end
    lda #ball_c_STATE_MUST_INIT
    sta ball_v_current_state

.end
    rts
; end sub golfer_s_update1_powering_up
} ; !zone

; **************************************************

!zone {
golfer_s_update0_hold
    dec golfer_v_frame_count
    bne .end

    ; Next stage depends on whether this is a swing or putt.
;    jsr powarc_s_deactivate_top_pulse
    lda round_v_must_putt
    +branch_if_true .putt

    ; So swing:
    lda #golfer_c_STATE_PRECISION
    sta golfer_v_current_state
    jsr powarc_s_init_precision
    rts ; EXIT POINT.

.putt
    lda #golfer_c_STATE_ANIMATE_BACKSWING_PUTT
    sta golfer_v_current_state
    jsr golfer_s_init_putt_animation

.end
    rts
; end sub golfer_s_update0_hold
} ; !zone

; **************************************************

!zone {
golfer_s_update1_precision
    ldx joy_v_current_port  

    lda powarc_v_precision_ended  
    +branch_if_true .hold

    +joy_m_is_fire 
    bne .unlock
    +joy_m_is_locked_fire
    +branch_if_true .end

.hold
    inc golfer_v_shot_spin_set 
    lda powarc_v_step_count   
    sta powarc_v_precision_offset 
    +golfer_m_init_hold golfer_c_HOLD_DURATION,golfer_c_STATE_HOLD2
    ; Message to ball module that trajectory must be initialized.
    lda #ball_c_STATE_MUST_INIT
    sta ball_v_current_state
    rts ; EXIT POINT.

.unlock
    +joy_m_release_fire

.end
    rts
; end sub golfer_s_update1_precision
} ; !zone

; **************************************************

!zone {
golfer_s_update0_hold2
    dec golfer_v_frame_count
    bne .end
    ; Initialize the golfer animation.
    ; TODO: full or half swing?  Should depend on club & power.
    lda #golfer_c_STATE_ANIMATE_BACKSWING
    sta golfer_v_current_state
    jsr golfer_s_init_swing_animation
.end
    rts
; end sub golfer_s_update0_hold2
} ; !zone

; **************************************************

!zone {
golfer_s_init_animate_hold
;    lda #golfer_c_HOLD_DURATION_SWING 
;    sta golfer_v_frame_count
;    lda #golfer_c_STATE_ANIMATE_HOLD
;    sta golfer_v_current_state
    +golfer_m_init_hold golfer_c_HOLD_DURATION_SWING,golfer_c_STATE_ANIMATE_HOLD
    ; Prepare sprite animation.
    ; Torso.
    lda #sprd_c_GOLFER_SWING_UPPER+sprd_c_GOLFER_MIDSWING_OFFSET 
;    clc
;    adc round_current_gender_offset 
    sta spr_v_anim_start_ptr+GOLFER_UPPER_BODY_SPR_NUM    
    lda #sprd_c_GOLFER_SWING_UPPER+sprd_c_GOLFER_MIDSWING_OFFSET+sprd_c_GOLFER_FORESWING_FRAMES-1 
;    clc
;    adc round_current_gender_offset 
    sta spr_v_anim_end_ptr+GOLFER_UPPER_BODY_SPR_NUM        
    ; Legs.
    lda #sprd_c_GOLFER_SWING_LOWER+sprd_c_GOLFER_MIDSWING_OFFSET 
    sta spr_v_anim_start_ptr+GOLFER_LOWER_BODY_SPR_NUM    
    lda #sprd_c_GOLFER_SWING_LOWER+sprd_c_GOLFER_MIDSWING_OFFSET+sprd_c_GOLFER_FORESWING_FRAMES-1
    sta spr_v_anim_end_ptr+GOLFER_LOWER_BODY_SPR_NUM        
    ; Club.
    lda #sprd_c_GOLFER_SWING_CLUB+sprd_c_GOLFER_MIDSWING_OFFSET 
    sta spr_v_anim_start_ptr+GOLFER_CLUB_SHAFT_SPR_NUM    
    lda #sprd_c_GOLFER_SWING_CLUB+sprd_c_GOLFER_MIDSWING_OFFSET+sprd_c_GOLFER_FORESWING_FRAMES-1
    sta spr_v_anim_end_ptr+GOLFER_CLUB_SHAFT_SPR_NUM        

    lda #1
    sta spr_v_anim_seq_inc+GOLFER_CLUB_SHAFT_SPR_NUM           
    sta spr_v_anim_seq_inc+GOLFER_UPPER_BODY_SPR_NUM        
    sta spr_v_anim_seq_inc+GOLFER_LOWER_BODY_SPR_NUM        

    lda #GOLFER_FORESWING_FRAME_RATE
    sta spr_v_anim_timer+GOLFER_CLUB_SHAFT_SPR_NUM                     
    sta spr_v_anim_timer+GOLFER_UPPER_BODY_SPR_NUM                 
    sta spr_v_anim_timer+GOLFER_LOWER_BODY_SPR_NUM                  
    sta spr_v_framerate+GOLFER_CLUB_SHAFT_SPR_NUM                                
    sta spr_v_framerate+GOLFER_UPPER_BODY_SPR_NUM                            
    sta spr_v_framerate+GOLFER_LOWER_BODY_SPR_NUM       

    rts
; end sub golfer_s_init_animate_hold
} ; !zone

; **************************************************

;!zone {
;golfer_s_init_animate_hold2
;    lda #golfer_c_HOLD_TIME_SWING_COMPLETE
;    sta golfer_v_frame_count
;    lda #golfer_c_STATE_ANIMATE_HOLD2
;    sta golfer_v_current_state
;    rts
;; end sub golfer_s_init_animate_hold2
;} ; !zone

; **************************************************

!zone {
golfer_s_update0_hold_animate
    dec golfer_v_frame_count
    bne .end
    lda #golfer_c_STATE_ANIMATE_FORESWING
    sta golfer_v_current_state

.end
    rts
; end sub golfer_s_update0_hold_animate
} ; !zone

; **************************************************

!zone {
golfer_s_update0_hold2_animate
    dec golfer_v_frame_count
    bne .end
    jsr golfer_init_watching_shot1
.end
    rts
; end sub golfer_s_update0_hold2_animate
} ; !zone

; **************************************************

!zone {
golfer_s_init_putt_animation
    lda #sprd_c_GOLFER_PUTT_UPPER 
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_start_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_CLUB
    sta spr_v_anim_start_ptr+GOLFER_CLUB_SHAFT_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_UPPER+sprd_c_GOLFER_PUTT_FORESWING_FRAMES_PT0-1  
    sta spr_v_anim_end_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_CLUB+sprd_c_GOLFER_PUTT_FORESWING_FRAMES_PT0-1 
    sta spr_v_anim_end_ptr+GOLFER_CLUB_SHAFT_SPR_NUM   
    lda #1
    sta spr_v_anim_seq_inc+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_seq_inc+GOLFER_CLUB_SHAFT_SPR_NUM   
    lda #GOLFER_PUTTING_FRAME_RATE 
    sta spr_v_anim_timer+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_timer+GOLFER_CLUB_SHAFT_SPR_NUM   
    sta spr_v_framerate+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_framerate+GOLFER_CLUB_SHAFT_SPR_NUM   

    rts
; end sub golfer_s_init_putt_animation
} ; !zone

; **************************************************

!zone {
golfer_s_update0_hold_putt
    dec golfer_v_frame_count
    bne .end
    lda #golfer_c_STATE_ANIMATE_FORESWING_PUTT
    sta golfer_v_current_state
    jsr golfer_s_init_putt_animation_foreswing
.end
    rts
; end sub golfer_s_update0_hold_putt
} ; !zone

; **************************************************

!zone {
golfer_s_init_putt_animation_foreswing
    lda #sprd_c_GOLFER_PUTT_UPPER+sprd_c_GOLFER_PUTT_MIDSWING_OFFSET
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_start_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    ;lda #SPRD_GOLFER2_PUTT_SHAFT 
    lda #sprd_c_GOLFER_PUTT_CLUB+18
    sta spr_v_anim_start_ptr+GOLFER_CLUB_SHAFT_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_UPPER+9 ;SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT0-1  
    sta spr_v_anim_end_ptr+GOLFER_UPPER_BODY_SPR_NUM   
    lda #sprd_c_GOLFER_PUTT_CLUB+9;             sprd_c_CLUBS_BASE+23 ;SPRD_GOLFER2_PUTT_SHAFT+SPRD_GOLFER2_PUTT_FORESWING_FRAMES_PT0-1 
    sta spr_v_anim_end_ptr+GOLFER_CLUB_SHAFT_SPR_NUM   
    lda #1
    sta spr_v_anim_seq_inc+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_seq_inc+GOLFER_CLUB_SHAFT_SPR_NUM   
    lda #GOLFER_PUTTING_FRAME_RATE 
    sta spr_v_anim_timer+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_anim_timer+GOLFER_CLUB_SHAFT_SPR_NUM   
    sta spr_v_framerate+GOLFER_UPPER_BODY_SPR_NUM   
    sta spr_v_framerate+GOLFER_CLUB_SHAFT_SPR_NUM   

    rts
; end sub golfer_s_init_putt_animation_foreswing
} ; !zone

; **************************************************

!zone {
golfer_s_rotate_direction
    lda #golfer_c_BASE_DIRECTION_X_LO 
    sta golfer_v_direction_x_lo 
    lda #golfer_c_BASE_DIRECTION_X_HI 
    sta golfer_v_direction_x_hi 
    lda #golfer_c_BASE_DIRECTION_Z_LO 
    sta golfer_v_direction_z_lo 
    lda #golfer_c_BASE_DIRECTION_Z_HI 
    sta golfer_v_direction_z_hi 

    lda #<golfer_v_direction_x_lo
    sta MATHS0
    lda #>golfer_v_direction_x_lo
    sta MATHS1
    lda #<golfer_v_direction_z_lo
    sta MATHS2
    lda #>golfer_v_direction_z_lo
    sta MATHS3
    lda hole_current_rotation_angle     
    sta MATHS4
    ; FIXME: why was it sec below?!
    sec ;clc
    jsr maths_s_rotate

;    ; Find quadrant and 'angle' for this!
;    lda golfer_v_direction_x_hi
;    sta P0
;    lda golfer_v_direction_z_hi
;    sta P1
;    jsr maths_find_quadrant
;    lda golfer_v_direction_x_lo
;    sta P0
;    lda golfer_v_direction_x_hi
;    sta P1
;    lda golfer_v_direction_z_lo
;    sta P2
;    lda golfer_v_direction_z_hi
;    sta P3
;    jsr maths_arctan

;    ; Rough rotation first.  Prepare args.
;    ; Address of x into P0-P1:
;    lda #<golfer_v_direction_x_lo
;    sta P0
;    lda #>golfer_v_direction_x_lo
;    sta P1
;    ; Address of z into P2-P3:
;    lda #<golfer_v_direction_z_lo
;    sta P2
;    lda #>golfer_v_direction_z_lo
;    sta P3
;    ; P4 = current_quadrant.
;    lda #1
;    sta P4
;    ; P5 = quadrants to rotate.
;    lda hole_current_rotation_quadrants 
;    sta P5
;    jsr maths_rough_rotate_vec
;
;    ; Now the refined rotation.
;    lda #<golfer_v_direction_x_lo
;    sta P0
;    lda #>golfer_v_direction_x_lo
;    sta P1
;    lda #<golfer_v_direction_z_lo
;    sta P2
;    lda #>golfer_v_direction_z_lo
;    sta P3
;    lda hole_current_rotation_angle     
;    sta P4
;    ; Clear 'C' flag for ccw rotation.
;    sec
;    jsr maths_rotate_vec
    
    rts
; end sub golfer_s_rotate_direction
} ; !zone

; **************************************************

; INPUTS:   A = current value of 'golfer_v_slow_crosshair'.
;!zone {
;golfer_s_change_crosshair_speed
;    beq .to_slow
;    ; Set to normal.
;    dec golfer_v_slow_crosshair
;    lda #GOLFER_POS_MARKER_VX_LO
;    bne +
;.to_slow
;    inc golfer_v_slow_crosshair
;    lda #GOLFER_POS_MARKER_SLOW_VX_LO
;+
;    sta spr_v_vx_lo+GOLFER_POS_MARKER_SPR_NUM 
;    rts
;; end sub golfer_s_change_crosshair_speed
;} ; !zone

; **************************************************

; INPUTS:   X = direction (spr_c_LEFT or spr_c_RIGHT)
!zone {
golfer_s_init_marker_move
    cpx golfer_pos_marker_direction
    beq .end    ; Already moving in this direction!
    lda golfer_l_MARKER_BOUNDS,x
    cmp golfer_v_marker_index
    beq .end    ; Can't move any further.
    stx golfer_pos_marker_direction
    ; Set the delay to 1 because we want the marker sprite to move 
    ; immediately on the next refresh.
    lda #1
    sta golfer_v_marker_move_count

.end
    rts
; end sub golfer_s_init_marker_move
} ; !zone

; **************************************************

!zone {
golfer_s_move_marker
    ldx golfer_pos_marker_direction
    bmi .pulse
    
    dec golfer_v_marker_move_count
    lda golfer_v_marker_move_count
    bne .pulse  ; Not time yet...

    ; Time to move.  X holds direction.
    lda golfer_v_marker_index
    clc
    adc golfer_l_MARKER_DELTA,x
    sta golfer_v_marker_index
    cmp golfer_l_MARKER_BOUNDS,x
    bne +
    ; Can't go any further after this so stop moving.
    ; FIXME: this may not make any difference!
    lda #$ff
    sta golfer_pos_marker_direction

+
    lda golfer_v_marker_move_delay
    sta golfer_v_marker_move_count
    ldx golfer_v_marker_index
    lda golfer_l_MARKER_X_POS2,x
    sta spr_v_x_lo+GOLFER_POS_MARKER_SPR_NUM                 
    lda golfer_l_MARKER_Y_POS2,x
    sta spr_v_y+GOLFER_POS_MARKER_SPR_NUM                 

.pulse
    ; NOTE: use same colors as overhead ball.
;    lda spr_v_color+hole_c_OVERHEAD_BALL_SW_SPR_NUM
;    sta spr_v_color+GOLFER_POS_MARKER_SPR_NUM 

    rts
; end sub golfer_s_move_marker
} ; !zone

; **************************************************

; NOTE: golfer should be hidden during pre-putt while joystick is DOWN...
; INPUTS:   X = current joystick port
!zone {
golfer_s_check_hidden
    +joy_m_is_down 
    beq .hide

    ; Joystick not down so show golfer if hidden.
    lda golfer_v_is_hidden
    +branch_if_false .end
    ; Restore pointers.
    lda golfer_v_spr_pointer_buffer
    sta spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
    lda golfer_v_spr_pointer_buffer+1
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM
    lda golfer_v_spr_pointer_buffer+2
    sta spr_v_current_ptr+GOLFER_LOWER_BODY_SPR_NUM
    +clr golfer_v_is_hidden
    ; Lock fire so can't start powering up as soon as joydown is released.
    +joy_m_lock_fire 
    jsr golfer_s_restore_shadow
    ; Restore normal crosshair speed.
    lda #golfer_c_MARKER_MOVE_DELAY_NORMAL
    sta golfer_v_marker_move_delay
    rts ; EXIT POINT.

.hide
    lda golfer_v_is_hidden
    +branch_if_true .end
    ; Save existing pointers in buffer (so can be restored later).
    lda spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
    sta golfer_v_spr_pointer_buffer 
    lda spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM
    sta golfer_v_spr_pointer_buffer+1 
    lda spr_v_current_ptr+GOLFER_LOWER_BODY_SPR_NUM
    sta golfer_v_spr_pointer_buffer+2 
    ; NOTE: particle sprite will be cleared, so draw that instead.
    lda #sprd_c_PARTICLES_SHOT
    sta spr_v_current_ptr+GOLFER_CLUB_SHAFT_SPR_NUM
    sta spr_v_current_ptr+GOLFER_UPPER_BODY_SPR_NUM
    sta spr_v_current_ptr+GOLFER_LOWER_BODY_SPR_NUM
    sta golfer_v_is_hidden
    jsr golfer_s_hide_shadow
    ; Crosshair moves slowly while joydown.
    lda #golfer_c_MARKER_MOVE_DELAY_SLOW
    sta golfer_v_marker_move_delay

.end
    rts
; end sub golfer_s_check_hidden
} ; !zone

; **************************************************

!zone {
golfer_s_init_shadow
    +utils_m_kernal_out

    ldx round_v_must_putt
    lda golfer_l_SHADOW_POS_LO,x
    sta P0
    lda golfer_l_SHADOW_POS_HI,x
    sta P1

    ldy #(16-1)
-
    lda (P0),y
    sta golfer_l_ANTI_SHADOW_BUFFER,y
    and golfer_l_SHADOW_PATTERN,y
    sta golfer_l_SHADOW_BUFFER,y
    sta (P0),y
    dey
    bpl -

    +utils_m_kernal_in

    rts
; end sub golfer_s_init_shadow
} ; !zone

; **************************************************

!zone {
golfer_s_hide_shadow
    ldx round_v_must_putt
    lda golfer_l_SHADOW_POS_LO,x
    sta P0
    lda golfer_l_SHADOW_POS_HI,x
    sta P1

    ldy #(16-1)
-
    lda golfer_l_ANTI_SHADOW_BUFFER,y
    sta (P0),y
    dey
    bpl -

    rts
; end sub golfer_s_hide_shadow
} ; !zone

; **************************************************

!zone {
golfer_s_restore_shadow
    ldx round_v_must_putt
    lda golfer_l_SHADOW_POS_LO,x
    sta P0
    lda golfer_l_SHADOW_POS_HI,x
    sta P1

    ldy #(16-1)
-
    lda golfer_l_SHADOW_BUFFER,y
    sta (P0),y
    dey
    bpl -

    rts
; end sub golfer_s_restore_shadow
} ; !zone

; **************************************************

; Based on number of shots and distance putted, see if golfer is 
; deserving of praise.
; INPUTS: X = current player.
!zone {
golfer_s_evaluate_hole
    lda hole_v_par
    sec
    sbc players_v_current_shots,x
    clc
    adc #2
    bpl .at_least_double_bogey

    ; So over par, but maybe was a good putt?
    lda players_v_distance_lo,x
    cmp #<golfer_c_MIN_PRAISEWORTHY_PUTT_FT
    lda players_v_distance_hi,x
    sbc #>golfer_c_MIN_PRAISEWORTHY_PUTT_FT
    bcs .praiseworthy_putt

    ; No praise this time!
    rts ; EXIT POINT.

.praiseworthy_putt
    ldx #playmsg_c_TYPE_PUTT
    bne +
.at_least_double_bogey
    tax
+
    jsr playmsg_s_praise_shot
    bcc .end
    ldx #50
    jsr wash_s_init

.end
    rts
; end sub golfer_s_evaluate_hole
} ; !zone

; **************************************************

; OUTPUT:   C flag set = player has requested to concede hole;
;           else C flag clear.
!zone {
golfer_s_check_concede
    jsr SCNKEY
    lda $cb
    cmp #kbmat_c_RUNSTOP
    bne .no_request

    ; Player has requested to concede hole.
    +set golfer_v_concede_query_active
;    lda #golfer_c_STATE_CONCEDE_QUERY
;    sta golfer_v_current_state        
    ldx #msg_c_CONCEDE_HOLE          
    jsr msg_s_display_stock_msg
    lda #golfer_c_RESPONSE_YES
    sta golfer_v_current_response
    jsr golfer_s_toggle_concede_response
    sec
    rts ; EXIT POINT.

.no_request
    clc
    rts
; end sub golfer_s_check_concede
} ; !zone

; **************************************************

!zone {
golfer_s_toggle_concede_response
    lda golfer_v_current_response
    eor #$01
    sta golfer_v_current_response
    pha

    ; First color both options in normal color.
    ldx #14
    lda #golfer_c_RESPONSE_GHOST_COLOR
-
    sta gfxs_c_DISPLAY_BASE+(24*40),x
    inx
    cpx #20
    bne -

    ; And now highlight current response.
    pla
    tay
    ldx golfer_l_RESPONSE_OFFSET_BEGIN,y
    lda golfer_l_RESPONSE_OFFSET_END,y
    sta MATHS0
    lda #golfer_c_RESPONSE_HIGHLIGHT_COLOR
-
    sta gfxs_c_DISPLAY_BASE+(24*40),x
    inx
    cpx MATHS0
    bne -

    rts
; end sub golfer_s_toggle_concede_response
} ; !zone

; **************************************************

!zone {
golfer_s_handle_concede_response
    ; These happen irrespective of response!
    jsr golfer_s_repair_msg_area
    +clr golfer_v_concede_query_active

    lda golfer_v_current_response
    cmp #golfer_c_RESPONSE_NO
    beq .end

    ; So, yes...
    ; NOTE: message should always be accepted so no need to examine return
    ; code!
    ldx #round_c_MSG_CONCEDE_HOLE
    jsr round_s_record_msg
    ; We do not want to process any further input for current golf shot!
    lda #golfer_c_STATE_IN_LIMBO                 
    sta golfer_v_current_state
    rts ; EXIT POINT.

.end
    rts
; end sub golfer_s_handle_concede_response
} ; !zone

; **************************************************

; OUTPUTS:  C flag clear - no query in progress; C flag set - query in progress
;           so don't check controls further after this.
!zone {
golfer_s_update1_concede_query
    ; If no query active, check if there should be.
    lda golfer_v_concede_query_active
    +branch_if_true .check_controls
    jsr golfer_s_check_concede
    ; NOTE: C flag set as appropriate by above routine.
    rts ; EXIT POINT.

.check_controls
    ldx joy_v_current_port

    +joy_m_is_fire
    bne .check_left
    +joy_m_is_locked_fire
    bne .end
    +joy_m_lock_fire
    jsr golfer_s_handle_concede_response
    sec
    rts ; EXIT POINT.

.check_left
    +joy_m_is_left
    bne .check_right
    ; React only if left is unlocked & current response is YES (= 1).
    +joy_m_is_locked_left
    bne .end
    +joy_m_lock_left
    lda golfer_v_current_response
    beq .end
    jsr golfer_s_toggle_concede_response
    sec
    rts ; EXIT POINT.
    
.check_right
    +joy_m_is_right
    bne .unlock
    ; React only if right is unlocked & current response is NO (= 0).
    +joy_m_is_locked_right
    bne .end
    +joy_m_lock_right
    lda golfer_v_current_response
    bne .end
    jsr golfer_s_toggle_concede_response
    sec
    rts ; EXIT POINT.

.unlock
    +joy_m_release_horizontal

.end
    sec
    rts
; end sub golfer_s_update1_concede_query
} ; !zone

; **************************************************

; Small helper routine.
!zone {
golfer_s_repair_msg_area
    jsr msg_s_clear

    lda #GREY3
    ldx #14
-
    sta gfxs_c_DISPLAY_BASE+(24*40),x
    inx
    cpx #20
    bne -

    rts
; end sub golfer_s_repair_msg_area
} ; !zone

; **************************************************

; NOTE: a convenience routine to hide golfer's 'lower' sprite by setting its
; y-coord to 0.  This is necessary because if same h/w sprite # is being
; used by backdrop module it'll be re-enabled each frame if interrupts are
; running.
!zone {
golfer_s_push_off_screen
    lda #0
    sta spr_v_y+GOLFER_LOWER_BODY_SPR_NUM
    rts
; end sub golfer_s_push_off_screen
} ; !zone

; **************************************************

; NOTE: assuming X holds current joy port.
!zone {
golfer_s_check_power_speed
    +joy_m_is_up
    bne .unlock
    +joy_m_is_locked_up
    bne .end
    +joy_m_lock_up
    jsr powarc_s_toggle_power_up_speed
    rts ; EXIT POINT.

.unlock
    +joy_m_release_up

.end
    rts
; end sub golfer_s_check_power_speed
} ; !zone

; **************************************************

!zone {
golfer_s_check_punch_toggle
    jsr SCNKEY
    lda $cb
    cmp #kbmat_c_P 
    bne .unlock_p
    lda golfer_v_p_key_locked
    +branch_if_true .end

    ; Called on power-arc module because that's where visual indicator will
    ; be drawn.
    lda golfer_v_punch_selected 
    eor #$01
    sta golfer_v_punch_selected
    pha
    jsr powarc_s_toggle_punch
    pla
    tax
    jsr clubs_s_ghost_carry
    inc golfer_v_p_key_locked
    rts ; EXIT POINT.

.unlock_p
    +clr golfer_v_p_key_locked
.end
    rts
; end sub golfer_s_check_punch_toggle
} ; !zone

; **************************************************

; OUTPUTS:  C flag set if ball holed, else clear.
!zone {
golfer_s_update_score_card
    ; Add a shot, whatever happens later.
    ldx round_v_current_player
    inc players_v_current_shots,x

    lda ball_v_is_in_hole
    +branch_if_false .not_holed

    ; NOTE: this may initialize the color_wash module.
    jsr golfer_s_evaluate_hole

    ; Write our score to the score card UNLESS MATCH PLAY!
    ldx round_v_current_player
    lda shared_v_is_match_play
    +branch_if_true +
    jsr sc_s_write
+
    ; We record the fact that the current player has finished the hole by
    ; setting their 'current_shots' value to $ff (i.e. negative).
    lda #$ff
    sta players_v_current_shots,x

    ; Display this golfer's updated score straight away (unless match play).
    lda shared_v_is_match_play
    +branch_if_true +
    lda #1
    sta P0
    sta P1
    jsr sc_s_draw_name_and_score
+
    jsr stats_s_inc_balls_holed

    sec
    rts ; EXIT POINT.

.not_holed
    clc
    rts
; end sub golfer_s_update_score_card
} ; !zone

; **************************************************

!zone {
golfer_s_record_distance2
    ldx round_v_current_player
    lda target_z_lo
    sta players_v_distance_lo,x
    lda target_z_hi
    sta players_v_distance_hi,x
    rts
; end sub golfer_s_record_distance2
} ; !zone

; **************************************************

!zone {
golfer_s_record_terrain
    +clr round_v_current_player_is_on_green

    ldx round_v_current_player
    lda ball_v_current_terrain
    sta players_v_terrain,x

    cmp #ball_c_TERRAIN_GREEN_FWAY
    bne .end
    lda target_z_lo
    cmp #round_c_MAX_PUTT_DISTANCE_LO
    lda target_z_hi
    sbc #round_c_MAX_PUTT_DISTANCE_HI
    bcs .end
    inc round_v_current_player_is_on_green

.end
    rts
; end sub golfer_s_record_terrain
} ; !zone

; **************************************************

!if _DEBUG_PHYSICS_VARS_ {

!zone {
golfer_s_update_physics_config
    jsr SCNKEY
    lda $cb
    cmp #kbmat_c_Q
    beq .inc_min_bounce
    cmp #kbmat_c_A
    beq .dec_min_bounce
    cmp #kbmat_c_W
    beq .inc_bounce_diff
    cmp #kbmat_c_S
    beq .dec_bounce_diff
    cmp #kbmat_c_E
    beq .inc_friction
    cmp #kbmat_c_D
    beq .dec_friction
    cmp #kbmat_c_R
    beq .inc_min_vy
    cmp #kbmat_c_F
    beq .dec_min_vy
    cmp #kbmat_c_T
    beq .inc_min_vz
    cmp #kbmat_c_G
    beq .dec_min_vz

    lda #0
    sta golfer_v_kb_locked
    rts ; EXIT POINT.
.inc_min_bounce
    lda golfer_v_kb_locked
    bne .end
    inc ball_l_BOUNCE_FACTOR_MIN+2
    jmp .redraw
.dec_min_bounce
    lda golfer_v_kb_locked
    bne .end
    dec ball_l_BOUNCE_FACTOR_MIN+2
    jmp .redraw
.inc_bounce_diff
    lda golfer_v_kb_locked
    bne .end
    inc ball_l_BOUNCE_FACTOR_DIFF+2    
    jmp .redraw
.dec_bounce_diff
    lda golfer_v_kb_locked
    bne .end
    dec ball_l_BOUNCE_FACTOR_DIFF+2    
    jmp .redraw
.inc_friction
    lda golfer_v_kb_locked
    bne .end
    inc ball_l_FRICTION_COEFFICIENTS+2
    jmp .redraw
.dec_friction
    lda golfer_v_kb_locked
    bne .end
    dec ball_l_FRICTION_COEFFICIENTS+2
    jmp .redraw
.inc_min_vy
    lda golfer_v_kb_locked
    bne .end
    inc ball_c_MIN_VY_ON_BOUNCE
    jmp .redraw
.dec_min_vy
    lda golfer_v_kb_locked
    bne .end
    dec ball_c_MIN_VY_ON_BOUNCE
    jmp .redraw
.inc_min_vz
    lda golfer_v_kb_locked
    bne .end
    inc ball_c_ROLLING_MIN_VZ
    jmp .redraw
.dec_min_vz
    lda golfer_v_kb_locked
    bne .end
    dec ball_c_ROLLING_MIN_VZ

.redraw
    jsr golfer_s_redraw_physics_vars
    inc golfer_v_kb_locked

.end
    rts
; end sub golfer_s_update_physics_config
} ; !zone

; **************************************************

!zone {
.buffer !fill 2
.hex    !byte   $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$41,$42,$43,$44,$45,$46

golfer_s_redraw_physics_vars
    lda ball_l_BOUNCE_FACTOR_MIN+2
    and #$0f
    tax
    lda .hex,x
    sta .buffer+1
    lda ball_l_BOUNCE_FACTOR_MIN+2
    lsr
    lsr
    lsr
    lsr
    tax
    lda .hex,x
    sta .buffer
    ; Params for routine call.
    lda #<.buffer
    sta P0
    lda #>.buffer
    sta P1
    lda #24*8
    sta P2
    lda #20*4
    sta P3
    lda #2
    sta P4
    jsr font_s_draw_text

    lda ball_l_BOUNCE_FACTOR_DIFF+2
    and #$0f
    tax
    lda .hex,x
    sta .buffer+1
    lda ball_l_BOUNCE_FACTOR_DIFF+2
    lsr
    lsr
    lsr
    lsr
    tax
    lda .hex,x
    sta .buffer
    ; Params for routine call.
    lda #<.buffer
    sta P0
    lda #>.buffer
    sta P1
    lda #24*8
    sta P2
    lda #23*4
    sta P3
    lda #2
    sta P4
    jsr font_s_draw_text

    lda ball_l_FRICTION_COEFFICIENTS+2
    and #$0f
    tax
    lda .hex,x
    sta .buffer+1
    lda ball_l_FRICTION_COEFFICIENTS+2
    lsr
    lsr
    lsr
    lsr
    tax
    lda .hex,x
    sta .buffer
    ; Params for routine call.
    lda #<.buffer
    sta P0
    lda #>.buffer
    sta P1
    lda #24*8
    sta P2
    lda #26*4
    sta P3
    lda #2
    sta P4
    jsr font_s_draw_text

    lda ball_c_MIN_VY_ON_BOUNCE
    and #$0f
    tax
    lda .hex,x
    sta .buffer+1
    lda ball_c_MIN_VY_ON_BOUNCE
    lsr
    lsr
    lsr
    lsr
    tax
    lda .hex,x
    sta .buffer
    ; Params for routine call.
    lda #<.buffer
    sta P0
    lda #>.buffer
    sta P1
    lda #24*8
    sta P2
    lda #29*4
    sta P3
    lda #2
    sta P4
    jsr font_s_draw_text

    lda ball_c_ROLLING_MIN_VZ
    and #$0f
    tax
    lda .hex,x
    sta .buffer+1
    lda ball_c_ROLLING_MIN_VZ
    lsr
    lsr
    lsr
    lsr
    tax
    lda .hex,x
    sta .buffer
    ; Params for routine call.
    lda #<.buffer
    sta P0
    lda #>.buffer
    sta P1
    lda #24*8
    sta P2
    lda #32*4
    sta P3
    lda #2
    sta P4
    jsr font_s_draw_text


    rts
; end sub golfer_s_redraw_physics_vars
} ; !zone
} ; !if _DEBUG_PHYSICS_VARS_

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

golfer_c_SIZE = *-golfer_c_BEGIN
