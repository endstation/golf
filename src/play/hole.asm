; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


hole_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
hole_c_PIXELS_PER_YARD = 21
hole_c_PIXELS_PER_FOOT = 7

hole_c_BOUNDARY_BOTTOM_LO = <(-50*hole_c_PIXELS_PER_YARD)
hole_c_BOUNDARY_BOTTOM_HI = >(-50*hole_c_PIXELS_PER_YARD)

hole_c_ICON_NUM_BYTES = 245

;hole_c_HAZARD_TYPE_WATER  = 1
;hole_c_HAZARD_TYPE_BUNKER = 3

hole_c_OVERHEAD_BALL_SW_SPR_NUM = 0
hole_c_OVERHEAD_BALL_HW_SPR_NUM = 0

hole_c_PULSE_FRAME_RATE = 10
hole_c_PULSE_COLORS !byte   WHITE,LIGHT_RED,VIOLET,BLUE,VIOLET,LIGHT_RED
hole_c_PULSE_COLORS_N = 6

hole_c_OVERHEAD_BALL_PTR = (play_l_OVERHEAD_BALL_SPRITE-$c000)/64


; *****************
; *** VARIABLES ***
; *****************
hole_v_boundary_top_lo      !byte   0
hole_v_boundary_top_hi      !byte   0
hole_v_boundary_left_lo     !byte   0
hole_v_boundary_left_hi     !byte   0
hole_v_boundary_right_lo    !byte   0
hole_v_boundary_right_hi    !byte   0
hole_v_par                  !byte   0
;hole_v_hazard_type          !byte   0
hole_v_overhead_origin_x_lo !byte   0
hole_v_overhead_origin_x_hi !byte   0
hole_v_overhead_origin_y    !byte   0
; Scale is 1 pixel of overhead map to world pixels.
hole_v_overhead_scale_lo    !byte   0
hole_v_overhead_scale_hi    !byte   0

hole_v_boundary_aa_top_lo       !byte   0
hole_v_boundary_aa_top_hi       !byte   0
hole_v_boundary_aa_left_lo      !byte   0
hole_v_boundary_aa_left_hi      !byte   0
hole_v_boundary_aa_right_lo     !byte   0
hole_v_boundary_aa_right_hi     !byte   0
hole_v_boundary_aa_bottom_lo    !byte   0
hole_v_boundary_aa_bottom_hi    !byte   0

hole_current_rotation_quadrants !byte   0
hole_current_rotation_angle     !byte   0

hole_v_overhead_map_icon    !fill   245,0

hole_v_overhead_ball_x_lo   !byte   0
hole_v_overhead_ball_x_hi   !byte   0
hole_v_overhead_ball_y      !byte   0
hole_v_overhead_ball_active !byte   0

hole_v_pulse_iter   !byte   0
hole_v_pulse_count  !byte   0


; *******************
; ****** MACROS *****
; *******************
!macro hole_m_deactivate_overhead_ball {
    lda #0
    sta hole_v_overhead_ball_active
} ; hole_m_deactivate_overhead_ball


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   P0/P1 = address of course data.
;           Offsets: 6 = top boundary (2 bytes), 8 = left boundary (2 bytes),
;               10 = right boundary (2 bytes), 12 = par (1 byte),
;               13-16 = overhead origin (3 bytes),
;               17-18 = overhead scale (2 bytes).            
; NOTE: since these variables follow each other in memory, we can write to
;       them in one go.
!zone {
.OFFSET     = 6
.NUM_BYTES  = 12

hole_s_init
    ldy #.OFFSET
-   lda (P0),y
    sta hole_v_boundary_top_lo-.OFFSET,y
    iny
    cpy #.OFFSET+.NUM_BYTES
    bne -

    lda #0
    sta hole_current_rotation_angle
    sta hole_current_rotation_quadrants

    ldx round_v_current_hole
    lda hole_v_par
    sta sc_v_pars,x

    rts
; end sub hole_s_init
} ; !zone

; **************************************************

!zone {
hole_s_set_aa_boundaries
    ldx #5
-
    lda hole_v_boundary_top_lo,x
    sta hole_v_boundary_aa_top_lo,x
    dex
    bpl -
    ; NOTE: the 'bottom' boundary is the same for each hole.
    lda #hole_c_BOUNDARY_BOTTOM_LO 
    sta hole_v_boundary_aa_bottom_lo
    lda #hole_c_BOUNDARY_BOTTOM_HI 
    sta hole_v_boundary_aa_bottom_hi

    +sbc16 hole_v_boundary_aa_top_lo,ball_world_z_lo
    +sbc16 hole_v_boundary_aa_bottom_lo,ball_world_z_lo
    +sbc16 hole_v_boundary_aa_left_lo,ball_world_x_lo
    +sbc16 hole_v_boundary_aa_right_lo,ball_world_x_lo

    rts
; end sub hole_s_set_aa_boundaries
} ; !zone

; **************************************************

; INPUTS:   P0-P1 - address of data.
; We must load 245 bytes.
!zone {
hole_s_load_overhead_map
    ldy #0

-
    lda (P0),y
    sta hole_v_overhead_map_icon,y
    iny
    cpy #hole_c_ICON_NUM_BYTES
    bne -

    rts
; end sub hole_s_load_overhead_map
} ; !zone

; **************************************************

!zone {
hole_s_draw_overhead_map
    lda #<hole_v_overhead_map_icon
    sta P0
    lda #>hole_v_overhead_map_icon
    sta P1
    lda #$ff
    sta P2
    jsr icon_s_draw
    rts
; end sub hole_s_draw_overhead_map
} ; !zone

; **************************************************

!zone {
hole_s_init_overhead_ball_sprite
    ; This will map the ball's world coordinates to a position on the map
    ; (and stored in hole_v_overhead_ball_x_lo, etc.).
    jsr ball_s_update_overhead_ball

    lda hole_v_overhead_ball_x_lo
    sta spr_v_x_lo+hole_c_OVERHEAD_BALL_SW_SPR_NUM 
    lda hole_v_overhead_ball_x_hi
    sta spr_v_x_hi+hole_c_OVERHEAD_BALL_SW_SPR_NUM 
    lda hole_v_overhead_ball_y
    sta spr_v_y+hole_c_OVERHEAD_BALL_SW_SPR_NUM 
    lda #WHITE
    sta spr_v_color+hole_c_OVERHEAD_BALL_SW_SPR_NUM
    lda #1
    sta spr_v_hires+hole_c_OVERHEAD_BALL_SW_SPR_NUM
    lda #hole_c_OVERHEAD_BALL_PTR
    sta spr_v_current_ptr+hole_c_OVERHEAD_BALL_SW_SPR_NUM

;    lda SPENA
;    ora #$01
;    sta SPENA
    lda #1
    sta hole_v_overhead_ball_active

    lda #hole_c_PULSE_FRAME_RATE
    sta hole_v_pulse_count
    lda #0
    sta hole_v_pulse_iter

    rts
; end sub hole_s_init_overhead_ball_sprite
} ; !zone

; **************************************************

!zone {
hole_s_draw_overhead_ball
    ; Always do this no matter what because golfer's crosshair can use the
    ; same color/pulse mechanic.
    jsr hole_s_update_pulse

    lda round_v_must_putt
    +branch_if_false +
    rts ; EXIT POINT.

+
    lda hole_v_overhead_ball_x_lo
    sta spr_v_x_lo+hole_c_OVERHEAD_BALL_SW_SPR_NUM
    lda hole_v_overhead_ball_x_hi
    sta spr_v_x_hi+hole_c_OVERHEAD_BALL_SW_SPR_NUM
    lda hole_v_overhead_ball_y
    sta spr_v_y+hole_c_OVERHEAD_BALL_SW_SPR_NUM

;    ldy #hole_c_OVERHEAD_BALL_SW_SPR_NUM
;    ldx #hole_c_OVERHEAD_BALL_HW_SPR_NUM
;    jsr spr_s_write_to_vic_ii

    rts
; end sub hole_s_draw_overhead_ball
} ; !zone

; **************************************************

; OUTPUTS:  X = index into color table.
!zone {
hole_s_update_pulse
    dec hole_v_pulse_count
    bne .end
    lda #hole_c_PULSE_FRAME_RATE
    sta hole_v_pulse_count
    ldx hole_v_pulse_iter
    inx
    cpx #hole_c_PULSE_COLORS_N
    bne +
    ldx #0
+
    stx hole_v_pulse_iter
    lda hole_c_PULSE_COLORS,x
    sta spr_v_color+hole_c_OVERHEAD_BALL_SW_SPR_NUM

.end
    rts
; end sub hole_s_update_pulse
} ; !zone

; **************************************************

!zone {
hole_s_draw_for_real
    lda hole_v_overhead_ball_active
    +branch_if_false .end
    ldy #hole_c_OVERHEAD_BALL_SW_SPR_NUM
    ldx #hole_c_OVERHEAD_BALL_HW_SPR_NUM
    jsr spr_s_write_to_vic_ii
.end
    rts
; end sub hole_s_draw_for_real
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

hole_c_SIZE = *-hole_c_BEGIN

