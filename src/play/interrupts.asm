; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


interrupts_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; Terminate list with 0.
interrupts_l_SPLITS    !byte   $a6,$b1,$ba,$fa,0

interrupts_c_UPPER_MIDDLE   = 0
interrupts_c_MIDDLE         = 1
interrupts_c_LOWER_MIDDLE   = 2
interrupts_c_BOTTOM         = 3

interrupts_c_REQUEST_NONE = $ff


; *****************
; *** VARIABLES ***
; *****************
interrupts_v_current_raster !byte   0
interrupts_v_state_change_request     !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; NOTE: this is 'jmp'd to by the callback (cb) routines.
!zone {
interrupts_s_reset
    ldy interrupts_v_current_raster
    iny
-
    lda (INTERRUPTS_LO),y
    bne +
    ldy #0
    beq -
+
    sty interrupts_v_current_raster
    sta RASTER
    +utils_m_clear_raster_bit9 
    ; Release latch.
    asl VICIRQ

; NOTE: will often jump straight to here.
interrupts_s_clean_up
    ; Pull registers off of stack and restore.
    pla
    tay
    pla
    tax
    pla
    rti

; end sub interrupts_reset
} ; !zone

; **************************************************

!zone {
interrupts_s_uninstall
    sei
    lda #0
    sta IRQMSK
    cli
    rts
; end sub interrupts_s_uninstall
} ; !zone

; **************************************************

!zone {
interrupts_s_install
    sei

    lda #interrupts_c_REQUEST_NONE
    sta interrupts_v_state_change_request

    lda #<interrupts_s_cb
    sta CINV
    lda #>interrupts_s_cb
    sta CINV+1
    lda #<interrupts_l_SPLITS    
    sta INTERRUPTS_LO
    lda #>interrupts_l_SPLITS    
    sta INTERRUPTS_HI

    ; Enable raster interrupts.
    lda #$01
    sta IRQMSK
    ; Turn off CIA interrupts.
    lda #$7f
    sta $dc0d
    
    ldy #0
    sty interrupts_v_current_raster
    lda (INTERRUPTS_LO),y
    sta RASTER
    +utils_m_clear_raster_bit9 

.end
    cli
    rts

; end sub interrupts_s_install
} ; !zone

; **************************************************

!zone {
.N = (SIGN_CHANGED+1) - P0

interrupts_s_copy_zp
    ldx #0
-
    lda P0,x
    sta ZP_END,x
    inx
    cpx #.N
    bne -

;    lda P0
;    sta PP0
;    lda P1
;    sta PP1
;    lda P2
;    sta PP2
;    lda P3
;    sta PP3
;    lda P4
;    sta PP4
;    lda P5
;    sta PP5
;    lda P6
;    sta PP6
;    lda P7
;    sta PP7
;    lda MATHS0
;    sta MMATHS0
;    lda MATHS1
;    sta MMATHS1
;    lda MATHS2
;    sta MMATHS2
;    lda MATHS3
;    sta MMATHS3
;    lda MATHS4
;    sta MMATHS4
;    lda MATHS5
;    sta MMATHS5
;    lda MATHS6
;    sta MMATHS6
;    lda MATHS7
;    sta MMATHS7
;    lda xcos_lo
;    sta ROT0
;    lda xcos_hi
;    sta ROT1
;    lda xsin_lo
;    sta ROT2
;    lda xsin_hi
;    sta ROT3
;    lda zcos_lo
;    sta ROT4
;    lda zcos_hi
;    sta ROT5
;    lda zsin_lo
;    sta ROT6
;    lda zsin_hi
;    sta ROT7
;    lda SIGN_CHANGED
;    sta ROT8

    rts
; end sub interrupts_s_copy_zp
} ; !zone

; **************************************************

!zone {
.N = (SIGN_CHANGED+1) - P0

interrupts_s_restore_zp
    ldx #0
-
    lda ZP_END,x
    sta P0,x
    inx
    cpx #.N
    bne -
;    lda PP0
;    sta P0
;    lda PP1
;    sta P1
;    lda PP2
;    sta P2
;    lda PP3
;    sta P3
;    lda PP4
;    sta P4
;    lda PP5
;    sta P5
;    lda PP6
;    sta P6
;    lda PP7
;    sta P7
;    lda MMATHS0
;    sta MATHS0
;    lda MMATHS1
;    sta MATHS1
;    lda MMATHS2
;    sta MATHS2
;    lda MMATHS3
;    sta MATHS3
;    lda MMATHS4
;    sta MATHS4
;    lda MMATHS5
;    sta MATHS5
;    lda MMATHS6
;    sta MATHS6
;    lda MMATHS7
;    sta MATHS7
;    lda ROT0
;    sta xcos_lo
;    lda ROT1
;    sta xcos_hi
;    lda ROT2
;    sta xsin_lo
;    lda ROT3
;    sta xsin_hi
;    lda ROT4
;    sta zcos_lo
;    lda ROT5
;    sta zcos_hi
;    lda ROT6
;    sta zsin_lo
;    lda ROT7
;    sta zsin_hi
;    lda ROT8
;    sta SIGN_CHANGED

    rts
; end sub interrupts_s_restore_zp
} ; !zone

; **************************************************

!zone {
interrupts_s_cb
    lda VICIRQ
    bmi +
    jmp interrupts_s_clean_up

+
    ; Changes to the round manager's state must happen here, inside the
    ; interrupt service routine.
    lda interrupts_v_state_change_request
    bmi +
    sta round_v_current_state
    ; Acknowledge request (by clearing it).
    lda #interrupts_c_REQUEST_NONE
    sta interrupts_v_state_change_request

+
    lda round_v_current_state
    cmp #round_c_STATE_SHOT_IN_PROGRESS
    beq .in_progress
    cmp #round_c_STATE_SHOT_COMPLETE
    beq .complete
    cmp #round_c_STATE_SCORE_CARDS
    beq .cards
    ; Must be wiping screen!
    jsr interrupts_s_do_wiping_screen
    jmp interrupts_s_reset
.in_progress
    jsr interrupts_s_do_shot_in_progress
    jmp interrupts_s_reset
.complete
    jsr interrupts_s_do_shot_complete
    jmp interrupts_s_reset
.cards
    jsr interrupts_s_do_score_cards
    jmp interrupts_s_reset

;    rts
; end sub interrupts_s_cb
} ; !zone

; **************************************************

!zone {
interrupts_s_do_shot_in_progress
    inc shared_v_random_seed

    lda interrupts_v_current_raster
    beq .upper_middle
    cmp #interrupts_c_MIDDLE
    beq .middle
    cmp #interrupts_c_BOTTOM
    beq .bottom

    ; So must be 'lower middle'...
    lda golfer_v_current_skin_tone
    sta SPMC0
    rts ; EXIT POINT.

.upper_middle
    jsr hole_s_draw_for_real
    jsr golfer_s_draw_lower
    rts ; EXIT POINT.

.middle
    lda round_v_current_ground_color
    sta BGCOL0
    jsr partsys_s_draw
    rts ; EXIT POINT.

.bottom
    lda target_v_fabric_color
    sta SPMC0
    lda round_v_current_sky_color
    sta BGCOL0
    jsr interrupts_s_copy_zp
    jsr golfer_s_update1
    jsr ball_s_launch
;    jsr golfer_s_update2
    jsr golfer_s_draw
    jsr bdrop_s_update
    clc
    jsr ball_s_draw
    jsr hole_s_draw_overhead_ball
    jsr powarc_s_update_max_animation
    jsr powarc_s_draw_max_animation
    jsr partsys_s_copy_buffer_to_vram
    jsr partsys_s_update
    jsr golfer_s_update_anim
    jsr powarc_s_update_putt_assist
    jsr ball_s_update_time_critical
    jsr quads_s_update_shimmers
    jsr tweeter_s_update
    jsr snd_s_update
;    jsr wash_s_update
    jsr interrupts_s_restore_zp

    rts
; end sub interrupts_s_do_shot_in_progress
} ; !zone

; **************************************************

!zone {
interrupts_s_do_shot_complete
    lda interrupts_v_current_raster
    beq .upper_middle
    cmp #interrupts_c_MIDDLE
    beq .middle
    cmp #interrupts_c_BOTTOM
    beq .bottom

    ; So must be 'lower middle' ...
    lda golfer_v_current_skin_tone
    sta SPMC0
    rts ; EXIT POINT.

.upper_middle
    jsr hole_s_draw_for_real
    jsr golfer_s_draw_lower
    rts ; EXIT POINT.

.middle
    lda round_v_current_ground_color
    sta BGCOL0
    jsr partsys_s_draw
    rts ; EXIT POINT.

.bottom
    lda target_v_fabric_color
    sta SPMC0
    lda round_v_current_sky_color
    sta BGCOL0
    jsr interrupts_s_copy_zp
    jsr golfer_s_update1    ; controls
;    jsr ball_s_launch
;    jsr golfer_s_update2    ; draw?!
    jsr golfer_s_draw
    jsr bdrop_s_update
    ; NOTE: no need to project the ball or build sprite - will never change
    ; at this stage.  Save results from last frame of ball flight?!
    clc
    jsr ball_s_draw
    jsr hole_s_draw_overhead_ball
;    jsr powarc_s_update_max_animation
;    jsr powarc_s_draw_max_animation
;    jsr partsys_copy_buffer_to_vram
;    jsr partsys_update
;    jsr golfer_s_update_anim
    jsr powarc_s_update_putt_assist
;    jsr ball_s_update_time_critical
    jsr quads_s_update_shimmers
    jsr tweeter_s_update
    jsr snd_s_update
    jsr wash_s_update
    jsr interrupts_s_restore_zp

    rts
; end sub interrupts_s_do_shot_complete
} ; !zone

; **************************************************

!zone {
interrupts_s_do_score_cards
    lda interrupts_v_current_raster
    beq .upper_middle
    cmp #interrupts_c_MIDDLE
    beq .middle
    cmp #interrupts_c_BOTTOM
    beq .bottom

    ; So must be lower middle...
    rts ; EXIT POINT.

.upper_middle
    jsr hole_s_draw_for_real
    rts ; EXIT POINT.

.middle
    lda round_v_current_ground_color
    sta BGCOL0
    rts ; EXIT POINT.

.bottom
    lda round_v_current_sky_color
    sta BGCOL0
;    jsr interrupts_s_copy_zp
    jsr bdrop_s_update
    jsr powarc_s_update_putt_assist
    jsr quads_s_update_shimmers
    jsr tweeter_s_update
    jsr snd_s_update
;    jsr interrupts_s_restore_zp

    rts
; end sub interrupts_s_do_score_cards
} ; !zone

; **************************************************

!zone {
interrupts_s_do_wiping_screen
    lda interrupts_v_current_raster
    beq .upper_middle
    cmp #interrupts_c_MIDDLE
    beq .middle
    cmp #interrupts_c_BOTTOM
    beq .bottom

    ; So must be lower middle...
    rts ; EXIT POINT.

.upper_middle
    jsr hole_s_draw_for_real
    rts ; EXIT POINT.

.middle
    lda round_v_current_ground_color
    sta BGCOL0
    rts ; EXIT POINT.

.bottom
    lda round_v_current_sky_color
    sta BGCOL0
;    jsr interrupts_s_copy_zp
    jsr bdrop_s_update
;    jsr powarc_s_update_putt_assist
;    jsr quads_s_update_shimmers
;    jsr tweeter_s_update
;    jsr snd_s_update
    jsr transtn_s_update
;    jsr interrupts_s_restore_zp

    rts
; end sub interrupts_s_do_wiping_screen
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

interrupts_c_SIZE = *-interrupts_c_BEGIN

