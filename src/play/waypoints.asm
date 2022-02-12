; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


waypts_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
WAYPTS_MAX = 3


; *****************
; *** VARIABLES ***
; *****************
waypts_box_x0_lo   !fill   WAYPTS_MAX-1
waypts_box_x0_hi   !fill   WAYPTS_MAX-1
waypts_box_z0_lo   !fill   WAYPTS_MAX-1
waypts_box_z0_hi   !fill   WAYPTS_MAX-1
waypts_box_x1_lo   !fill   WAYPTS_MAX-1
waypts_box_x1_hi   !fill   WAYPTS_MAX-1
waypts_box_z1_lo   !fill   WAYPTS_MAX-1
waypts_box_z1_hi   !fill   WAYPTS_MAX-1

waypts_x_lo !fill   WAYPTS_MAX
waypts_x_hi !fill   WAYPTS_MAX
waypts_z_lo !fill   WAYPTS_MAX
waypts_z_hi !fill   WAYPTS_MAX

; Storage for axis-aligned coordinates of current waypoint (relative to
; ball's position).  This is what gets rotated.
waypts_aa_x_lo  !byte   0
waypts_aa_x_hi  !byte   0
waypts_aa_z_lo  !byte   0
waypts_aa_z_hi  !byte   0

waypts_n    !byte   0
waypts_current_i    !byte   0
;waypts_iter !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   ball_world_x_lo, etc.
; OUTPUTS:  X = index for current waypoint
!zone {
waypts_find_index
    ldx #0

.loop
    ; Is this the last waypoint?
    inx
    cpx waypts_n
    bne +

    ; Last waypoint is the one we're looking for.
    dex
    stx waypts_current_i
    rts ; EXIT POINT.

+
    dex
    ; Not the last waypoint so there should be a box associated with it.
    ; Is the ball located inside it?
    ; For all following subtractions, a<b should hold for a-b.
    lda waypts_box_x0_lo,x
    sec
    sbc ball_world_x_lo
    lda waypts_box_x0_hi,x
    sbc ball_world_x_hi
    +bge_s .next
    lda ball_world_x_lo
    sec
    sbc waypts_box_x1_lo,x
    lda ball_world_x_hi
    sbc waypts_box_x1_hi,x
    +bge_s .next
    lda ball_world_z_lo
    sec
    sbc waypts_box_z0_lo,x
    lda ball_world_z_hi
    sbc waypts_box_z0_hi,x
    +bge_s .next
    lda waypts_box_z1_lo,x
    sec
    sbc ball_world_z_lo
    lda waypts_box_z1_hi,x
    sbc ball_world_z_hi
    +bge_s .next

    ; So this is the index we're looking for (in X).
    stx waypts_current_i
    rts ; EXIT POINT.

.next
    inx
    jmp .loop

    ; NOTE: never reach this...
;    rts
; end sub waypts_find_index
} ; !zone

; **************************************************

; INPUTS:   P0-P1 - address of first byte of waypoints data.
; OUTPUT:   P0-P1 - address of byte following waypoints data.
; NOTE: data format is: number of waypoints;
; ... then, for each waypoint:
; (box) x0-lo,x0-hi,z0-lo,z0-hi, x1-lo,x1-hi,z1-lo,z1-hi
; (point) x-lo,x-hi, z-lo,z-hi
!zone {
waypts_load
    ldy #0
    lda (P0),y
    sta waypts_n
    beq .end

    ; Use X to keep track of waypoint index.
    ldx #0

.loop_top
    iny
    lda (P0),y
    sta waypts_box_x0_lo,x
    iny
    lda (P0),y
    sta waypts_box_x0_hi,x
    iny
    lda (P0),y
    sta waypts_box_z0_lo,x
    iny
    lda (P0),y
    sta waypts_box_z0_hi,x
    iny
    lda (P0),y
    sta waypts_box_x1_lo,x
    iny
    lda (P0),y
    sta waypts_box_x1_hi,x
    iny
    lda (P0),y
    sta waypts_box_z1_lo,x
    iny
    lda (P0),y
    sta waypts_box_z1_hi,x
    iny
    lda (P0),y
    sta waypts_x_lo,x
    iny
    lda (P0),y
    sta waypts_x_hi,x
    iny
    lda (P0),y
    sta waypts_z_lo,x
    iny
    lda (P0),y
    sta waypts_z_hi,x

    inx
    cpx waypts_n
    bne .loop_top

.end
    ; Make sure P0-P1 now point to the byte following the waypoints data.
    iny
    sty P2  ; This is what we need to add!
    lda P0
    clc
    adc P2
    sta P0
    lda P1
    adc #0
    sta P1

    rts
; end sub waypts_load
} ; !zone

; **************************************************

; OUTPUT:   C flag set if there shouldn't be any rotation; otherwise C flag
;           clear.
; NOTE: you should call waypts_find_index before calling this, so that
; the index is valid.
!zone {
waypts_calc_rotation
    ; Check for the easiest case first - no rotation (- coordinates will be
    ; 0,0).
    ldx waypts_current_i
    lda waypts_x_lo,x
    ora waypts_x_hi,x
    ora waypts_z_lo,x
    ora waypts_z_hi,x
    beq .no_rotation

    ; A rotation must be executed.  First we must find out the waypoint's 
    ; coordinates relative to the ball's current (world) position.
    lda waypts_x_lo,x
    sec
    sbc ball_world_x_lo
    sta waypts_aa_x_lo
    lda waypts_x_hi,x
    sbc ball_world_x_hi
    sta waypts_aa_x_hi
    lda waypts_z_lo,x
    sec
    sbc ball_world_z_lo
    sta waypts_aa_z_lo
    lda waypts_z_hi,x
    sbc ball_world_z_hi
    sta waypts_aa_z_hi
    jsr waypts_rotate_to_pos_z_axis
    clc
    rts

.no_rotation
    lda #0
    sta hole_current_rotation_quadrants 
    sta hole_current_rotation_angle 
    sec
    rts
; end sub waypts_calc_rotation
} ; !zone

; **************************************************

; FIXME: this routine is repeated almost verbatim in 'target' module!!!
; OUTPUT:   we record the rotation necessary to get the current waypoint
;           aligned with the positive z-axis.
; NOTE: we're not bothered about actually fully rotating the waypoint.  As soon
;       as we get the quadrant and angle (for trig-table lookup) we're done.
!zone {
waypts_rotate_to_pos_z_axis
    lda waypts_aa_x_lo  
    sta maths_mod08
    lda waypts_aa_x_hi  
    sta maths_mod09
    lda waypts_aa_z_lo  
    sta maths_mod10
    lda waypts_aa_z_hi  
    sta maths_mod11
    clc
    jsr maths_s_rotate_vec_to_pos_z_axis
    sta hole_current_rotation_angle
    rts
; end sub waypts_rotate_to_pos_z_axis
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
; **************************************************
; **************************************************
; **************************************************
; **************************************************

waypts_c_SIZE = *-waypts_c_BEGIN
