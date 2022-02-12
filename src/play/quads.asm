; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


quads_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
quads_c_MAX_N = 25

quads_c_MAX_VALID_X_PLUS_ONE_LO   = <160
quads_c_MAX_VALID_X_PLUS_ONE_HI   = >160
; Use these for when clipping quads at x=0 and x=159.
quads_c_VERTEX_STATE_UNSET        = (-1)
quads_c_VERTEX_STATE_IN_BOUNDS    = 0
quads_c_VERTEX_STATE_OOB_LEFT     = 1;
quads_c_VERTEX_STATE_OOB_RIGHT    = 2;
; Refers to dimensions of bitmap coordinate space.
quads_c_SCREEN_HEIGHT = 200

quads_c_MAX_BUFFER_SIZE = 7*4

; NOTE: skip over first four bytes (where index would be 0).
quads_l_TEXTURE_COLORS = *-4
    !byte   LIGHT_RED,LIGHT_RED,GREY3,WHITE             ; for sand
    !byte   LIGHT_GREEN,LIGHT_GREEN,LIGHT_GREEN,YELLOW  ; for grass
quads_l_TEXTURE_COLOR_OFFSETS   !byte   0,4,8
quads_l_TEXTURES    !byte   %11000000,%00110000,%00001100,%00000011
                    !byte   %11110000,%00111100,%00001111,%11001100

QUADS_NUM_BYTES_PER_QUAD = 18

quads_c_MAX_SHIMMERS = 64
quads_c_SHIMMER_FRAME_RATE = 10
quads_l_SHIMMER_COLORS
    !byte   (LIGHT_BLUE<<4)|GREEN
    !byte   (GREY3<<4)|GREEN
    !byte   (WHITE<<4)|GREEN
quads_c_NUM_SHIMMER_COLORS = 3
quads_l_SHIMMER_PATTERNS    !byte   %01111111,%11011111,%11110111,%11111101

; NOTE: indexed by terrain type.  This is what to write to the bitmap for a 
; whole byte's worth of pixels (four in a row).
quads_l_FILL_BYTES  !byte   $00,%01010101,%10101010,%11111111

quads_c_MIN_DRAWABLE_DEPTH = 2


; *****************
; *** VARIABLES ***
; *****************
; NOTE: these are filled in when a new hole is loaded.
quads_n             !byte   0
quads_num_vertices  !byte   0
; NOTE: 'type' must encode also the color.
quads_type      !fill   quads_c_MAX_N
quads_x_lo      !fill   quads_c_MAX_N*4
quads_x_hi      !fill   quads_c_MAX_N*4
quads_z_lo      !fill   quads_c_MAX_N*4
quads_z_hi      !fill   quads_c_MAX_N*4
quads_aa_x_lo   !fill   quads_c_MAX_N*4
quads_aa_x_hi   !fill   quads_c_MAX_N*4
quads_aa_z_lo   !fill   quads_c_MAX_N*4
quads_aa_z_hi   !fill   quads_c_MAX_N*4
quads_rot_x_lo  !fill   quads_c_MAX_N*4
quads_rot_x_hi  !fill   quads_c_MAX_N*4
quads_rot_z_lo  !fill   quads_c_MAX_N*4
quads_rot_z_hi  !fill   quads_c_MAX_N*4

quads_min_z_lo      !byte   0
quads_min_z_hi      !byte   0
quads_max_z_lo      !byte   0
quads_max_z_hi      !byte   0
quads_min_z_point   !byte   0
quads_max_z_point   !byte   0
quads_min_y_lo      !byte   0
quads_max_y_lo      !byte   0
quads_min_y_point   !byte   0
quads_max_y_point   !byte   0

; In buffer, quad points are stored in ccw order: x-lo,x-hi,z-lo,z-hi...
quads_buffer        !fill   quads_c_MAX_BUFFER_SIZE
quads_buffer_size   !byte   0
quads_buffer_iter   !byte   0
; Projected points go here - we'll feed this to the quad-fill routine.
quads_buffer_proj   !fill   quads_c_MAX_BUFFER_SIZE

; Stuff used for filling in the quads.
quads_edges_from    !fill   256
quads_edges_to      !fill   256

; 'Working' buffer stuff.
; FIXME: can another existing buffer be reused?!
quads_wbuf      !fill   quads_c_MAX_BUFFER_SIZE
quads_wbuf_iter !byte   0
quads_wbuf_size !byte   0
 
; Drawing order is based on ROTATED vertices.
quads_drawing_order     !fill   quads_c_MAX_N
quads_rot_min_z_lo      !fill   quads_c_MAX_N
quads_rot_min_z_hi      !fill   quads_c_MAX_N
quads_rot_max_z_lo      !fill   quads_c_MAX_N
quads_rot_max_z_hi      !fill   quads_c_MAX_N
quads_rot_min_z_points  !fill   quads_c_MAX_N
quads_rot_max_z_points  !fill   quads_c_MAX_N

quads_triangle_indices  !fill   quads_c_MAX_N
; Triangle arrays (- they're hidden inside quads!).
; FIXME: every quad can be a triangle?!
quads_triangles_vertex_to_omit  !fill   quads_c_MAX_N
quads_triangles_slope_lo        !fill   quads_c_MAX_N
quads_triangles_slope_hi        !fill   quads_c_MAX_N
quads_triangles_z_intercept_lo  !fill   quads_c_MAX_N
quads_triangles_z_intercept_hi  !fill   quads_c_MAX_N

quads_buf2_size !byte   0
quads_buf2_x_lo !fill   4
quads_buf2_x_hi !fill   4
quads_buf2_z_lo !fill   4
quads_buf2_z_hi !fill   4
quads_buf2_max_z_lo !byte   0
quads_buf2_max_z_hi !byte   0
quads_buf2_min_z_lo !byte   0
quads_buf2_min_z_hi !byte   0
quads_buf2_max_z_i  !byte   0
quads_buf2_min_z_i  !byte   0
 
quads_v_states          !fill   quads_c_MAX_SHIMMERS,(-1)
quads_v_frame_counts    !fill   quads_c_MAX_SHIMMERS
quads_v_vram_addrs_lo   !fill   quads_c_MAX_SHIMMERS
quads_v_vram_addrs_hi   !fill   quads_c_MAX_SHIMMERS
quads_v_shimmers_bitmap_addrs_lo    !fill   quads_c_MAX_SHIMMERS
quads_v_shimmers_bitmap_addrs_hi    !fill   quads_c_MAX_SHIMMERS
quads_v_shimmers_n  !byte   0
; NOTE: while filling in quads, keep track of whether we're inside a 
; 'water cell'.  In this case, there is a higher chance that the byte will
; be textured.  Variable can also record how many bytes have been affected
; and enforce an upper limit...
;quads_v_is_water_cell   !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUT:    P0-P1 = start address of memory from which to load data.
;           Data is in format: <num quads>,<quad 0> ... <quad n-1> 
;           - where each 'quad' is 17 bytes: type, x0-lo, x0-hi, z0-lo, z0-hi,
;           x1-lo, etc...
!zone {
.current_quad_i !byte   0
.end_iter       !byte   0

quads_s_load
    ; Reset the iterator.
    lda #0
    sta .current_quad_i

    ; Store number of quads (and number of vertices).
    ldy #0
    lda (P0),y
    sta quads_n
    asl
    asl
    sta quads_num_vertices

    ; Base address should always point to next quad.
    lda P0
    clc
    adc #1
    sta P0
    lda P1
    adc #0
    sta P1

.loop
    ldy #0

    ; For each quad...
    ; Store type.
    lda (P0),y
    ldx .current_quad_i
    sta quads_type,x

    ; Store triangle index.
    iny
    lda (P0),y
    sta quads_triangle_indices,x  

    ; Now there are 16 bytes for the vertices (- each vertex is made
    ; up of 4 bytes).  Use X as iterator for destination, so first multiply
    ; it by 4.
    txa
    asl
    asl
    tax
    ; Add 4 to X and save as end iterator (so we know when we've finished).
    clc
    adc #4
    sta .end_iter
    ; Prepare the source iterator also (=Y).
    iny

.inner_loop
    ; Ready to start.
    lda (P0),y
    sta quads_x_lo,x
    sta quads_aa_x_lo,x
    sta quads_rot_x_lo,x
    iny
    lda (P0),y
    sta quads_x_hi,x
    sta quads_aa_x_hi,x
    sta quads_rot_x_hi,x
    iny
    lda (P0),y
    sta quads_z_lo,x
    sta quads_aa_z_lo,x
    sta quads_rot_z_lo,x
    iny
    lda (P0),y
    sta quads_z_hi,x
    sta quads_aa_z_hi,x
    sta quads_rot_z_hi,x
    iny
    inx
    cpx .end_iter
    bne .inner_loop

    ; Advance base address to next quad (= +18 bytes).
    lda P0
    clc
    adc #QUADS_NUM_BYTES_PER_QUAD
    sta P0
    lda P1
    adc #0
    sta P1
     
    ; Now to the next quad.
    inc .current_quad_i
    lda quads_n
    cmp .current_quad_i
    beq .triangles
    jmp .loop

.triangles
    ; Now the triangle stuff.
    ; How many?  Store this value in MATHS0.
    ldy #0
    lda (P0),y
    sta MATHS0

    +utils_m_advance_zp_iter P0,1

    lda MATHS0
    beq +   ; No triangles.

    ; Use X as index into quads_triangles_* arrays. 
    ldx #0
-
    ldy #0
    lda (P0),y
    sta quads_triangles_vertex_to_omit,x
    iny
    lda (P0),y
    sta quads_triangles_slope_lo,x
    iny
    lda (P0),y
    sta quads_triangles_slope_hi,x
    iny
    lda (P0),y
    sta quads_triangles_z_intercept_lo,x
    iny
    lda (P0),y
    sta quads_triangles_z_intercept_hi,x

    +utils_m_advance_zp_iter P0,5

    inx
    cpx MATHS0
    bne -

+
    jsr quads_init_drawing_order

    rts
; end sub quads_load
} ; !zone

; **************************************************

; Find and record the minimum and maximum z-position for the given quad.
; INPUTS:   X = quad number (- so must multiply by 4 to get index into arrays).
!zone {
.one_past_end   !byte   0

quads_find_minmax_z
    ; First multiply by 4 so we have an index into the arrays.
    txa
    asl
    asl
    tax

    ; Store index+4 so we know when we've finished!
    ;txa
    clc
    adc #4
    sta .one_past_end

    ; Initially, we set point #0 to have minmax everything.
    lda quads_rot_z_lo,x
;    sta quads_min_z_lo
    sta quads_max_z_lo
    lda quads_rot_z_hi,x
;    sta quads_min_z_hi
    sta quads_max_z_hi
;    stx quads_min_z_point
;    stx quads_max_z_point

    ; X iterates over points.
    ; NOTE: for 16-bit signed comparisons, do a - b.  If N XOR V is true,
    ; then a < b; else a >= b.
    inx

.loop_top
;    lda quads_rot_z_lo,x
;    sec
;    sbc quads_min_z_lo
;    lda quads_rot_z_hi,x
;    sbc quads_min_z_hi
;    +bge_s .check_max_z
;
;    ; Fall through - set min-z.
;    lda quads_rot_z_lo,x
;    sta quads_min_z_lo
;    lda quads_rot_z_hi,x
;    sta quads_min_z_hi
;    stx quads_min_z_point

.check_max_z
    lda quads_rot_z_lo,x
    sec
    sbc quads_max_z_lo
    lda quads_rot_z_hi,x
    sbc quads_max_z_hi
    +blt_s .next

    ; Fall through - set max-z.
    lda quads_rot_z_lo,x
    sta quads_max_z_lo
    lda quads_rot_z_hi,x
    sta quads_max_z_hi
    stx quads_max_z_point

.next
    inx
    cpx .one_past_end
    beq .end
    jmp .loop_top

.end
    rts
; end sub quads_find_minmax_z
} ; !zone

; **************************************************

; INPUTS:   X = index into array for quad points.  So [X*4,X*4+3] are valid
;           indices.
; Any points of the quad that have z-values < 0 need to be transformed so
; they're aligned with the x-axis (at z=0) or omitted altogether.  Write the
; results to a buffer where they'll be immediately processed further.
; OUTPUT:   C flag clear if prepared successfully; C flag set if quad
;           shouldn't be drawn (because it's too close to/behind the camera).
!zone {
.begin_iter         !byte   0
.end_iter           !byte   0
.iter               !byte   0
.last_point_neg_z   !byte   0
.quads_i            !byte   0

quads_prepare_for_projection
    stx .quads_i

    txa
    asl
    asl
    sta .begin_iter
    clc
    adc #4
    sta .end_iter
    lda #0
    sta quads_buffer_size
    sta quads_buffer_iter
    sta .last_point_neg_z

    ; Initial checks.
    ; If min-z >= CAMERA_MIN_VALID_Z, everything's OK.
    lda quads_buf2_min_z_lo
    sec
    sbc #CAMERA_MIN_VALID_Z_LO
    lda quads_buf2_min_z_hi
    sbc #CAMERA_MIN_VALID_Z_HI
    +blt_s .skip
    jmp .all_points_ok

.skip
    ; So there's at least one point with z<CAMERA_MIN_VALID_Z.  Look at max-z.
    lda quads_buf2_max_z_lo
    sec
    sbc #CAMERA_MIN_VALID_Z_LO
    lda quads_buf2_max_z_hi
    sbc #CAMERA_MIN_VALID_Z_HI
    +bge_s .skip2

    ; All points are below the x-axis, so don't render anything.
    ; Set 'C' flag to signal this.
    sec
    rts

.skip2
    ; At least some of the quad is in front of the camera and can be 
    ; projected onto the screen.
    ; To begin with, add the max-z point.
    ldx quads_buf2_max_z_i
    stx .iter
    jsr quads_add_point_to_buffer
    jmp .next

.loop_top
    ldx .iter
    ; Check if the current point has z < CAMERA_MIN_VALID_Z_LO/HI.
    lda quads_buf2_z_lo,x
    sec
    sbc #CAMERA_MIN_VALID_Z_LO
    lda quads_buf2_z_hi,x
    sbc #CAMERA_MIN_VALID_Z_HI
    ; TODO: change branch target name!
    +blt_s .current_point_neg_z

    ; What about the previous point?
    lda .last_point_neg_z
    bne +
    jsr quads_add_point_to_buffer
    jmp .next
+
    ; Insert new point here: from current -> previous.
    ; TODO: solve linear equation!!!
    txa
    tay
    bne +
    ldy quads_buf2_size
+   dey
    jsr quads_solve_linear
    ; Clear flag.
    dec .last_point_neg_z
    ; Must also add current point before continuing.
    ldx .iter
    jsr quads_add_point_to_buffer
    jmp .next
    
.current_point_neg_z
    ; What about the previous point?
    lda .last_point_neg_z
    ; NOTE: keep flag set if another neg-z point.
    bne .next
    ; Insert new point here: from previous -> current.
    ; TODO: solve linear equation!!!
    ; Initialize parameters for routine call.
    ; X should hold index of point above x-axis and Y point below.  So first
    ; transfer X to Y.  Then decrement X - may involve wraparound if we're 
    ; already at .begin_iter.
    txa
    tay
    bne +
    ldx quads_buf2_size
+   dex
    jsr quads_solve_linear

    ; Set flag.
    inc .last_point_neg_z

.next
    ldx .iter
    inx
    cpx quads_buf2_size
    bne +
    ldx #0
+
    stx .iter
    ; We started at the max-z point, so let's see if we've looped back
    ; around to it yet.
    cpx quads_buf2_max_z_i
    bne .loop_top

    ; If last point was neg-z, we've got to polish things off.
    lda .last_point_neg_z
    beq .end
    ; Solve linear equation from current point to previous.  Then we've 
    ; processed all points of quad.
    ldx .iter
    txa
    tay
    bne +
    ldy quads_buf2_size
+   dey
    jsr quads_solve_linear
    jmp .end

.all_points_ok
    ; Check that min-z is < maximum allowed.
    lda quads_buf2_min_z_lo
    sec
    sbc #CAMERA_MAX_VALID_Z_LO
    lda quads_buf2_min_z_hi
    sbc #CAMERA_MAX_VALID_Z_HI
    +blt_s .in_bounds
    sec
    rts

.in_bounds
    ; FIXME: already have a routine to do this (for one point at a time)!!!
    ; There'll be three or four points.
    lda quads_buf2_size
    sta quads_buffer_size
    ldx #0
    ldy #0
.next_point
    lda quads_buf2_x_lo,x
    sta quads_buffer,y
    lda quads_buf2_x_hi,x
    sta quads_buffer+1,y
    lda quads_buf2_z_lo,x
    sta quads_buffer+2,y
    lda quads_buf2_z_hi,x
    sta quads_buffer+3,y
    inx
    cpx quads_buf2_size
    beq .end
    tya
    clc
    adc #4
    tay
    jmp .next_point

.end
    ; All OK so clear 'C' flag.
    clc
    rts
; end sub quads_prepare_for_projection
} ; !zone

; **************************************************

; INPUTS:   X = quads iter
!zone {
quads_add_point_to_buffer
    ldy quads_buffer_iter

    lda quads_buf2_x_lo,x
    sta quads_buffer,y
    lda quads_buf2_x_hi,x
    sta quads_buffer+1,y
    lda quads_buf2_z_lo,x
    sta quads_buffer+2,y
    lda quads_buf2_z_hi,x
    sta quads_buffer+3,y

    ; Advance buffer iterator.
    tya
    clc
    adc #4
    sta quads_buffer_iter

    ; Record what we've just done...
    inc quads_buffer_size

    rts
; end sub quads_add_point_to_buffer
} ; !zone

; **************************************************

; Given two points, we must find the 'x-intercept'.  I.e. solve the equation
; z=mx+c for when z=CAMERA_MIN_VALID_Z_LO/HI.  We'll do this by first
; calculating dx and dz; then find out how much of that dx is apportioned to
; the part of dz above our cut-off point.
; Regarding input parameters: give coordinates for the point ABOVE the x-axis
; first.  This guarantees that result of subtraction of z-components will be
; positive (- as long as we do: POINT0-POINT1).
; INPUTS:   X = 'from' iter, Y = 'to' iter 
!zone {
.dx_lo  !byte   0
.dx_hi  !byte   0
.dz_lo  !byte   0
.dz_hi  !byte   0
.point0_i   !byte   0
.point1_i   !byte   0
.must_add   !byte   0

quads_solve_linear
    stx .point0_i
    sty .point1_i
    ; By default, do an addition.
    lda #1
    sta .must_add

    ; First, calculate dx.
    lda quads_buf2_x_lo,x
    sec
    sbc quads_buf2_x_lo,y
    sta .dx_lo
    lda quads_buf2_x_hi,x
    sbc quads_buf2_x_hi,y
    sta .dx_hi

    ; Will we need to add or subtract our delta value?
    php
    +blt_s .skip
    ; POINT0x > POINT1x so we'll need to subtract.
    dec .must_add
.skip
    plp

    ; Negate if necessary...
    bpl +
    lda #<.dx_lo
    sta P0
    lda #>.dx_lo
    sta P1
    jsr maths_adjust_vec_signs
    ; Restore Y.
    ldy .point1_i
+

    ; Now dz.
    lda quads_buf2_z_lo,x
    sec
    sbc quads_buf2_z_lo,y
    sta .dz_lo
    lda quads_buf2_z_hi,x
    sbc quads_buf2_z_hi,y
    sta .dz_hi

    ; Do: dx * (POINT0z - CAMERA_MIN_VALID_Z_LO/HI).
    ; NOTE: second factor is guaranteed to be positive.
    lda .dx_lo
    sta P0
    lda .dx_hi
    sta P1

    lda quads_buf2_z_lo,x
    sec
    sbc #CAMERA_MIN_VALID_Z_LO
    sta P2
    lda quads_buf2_z_hi,x
    sbc #CAMERA_MIN_VALID_Z_HI
    sta P3

    jsr maths_mul16
    ; BUG TODO FIXME: we make the dangerous assumption that we're getting a
    ; 16-bit product!!!
    ; Now do: (dx*(POINT0z-CAMERA_MIN_VALID_Z)) / dz

    ; Make adjustments for 24-bit product if necessary (- keep dividing 
    ; dividend and divisor by 2 until P6 is 0...).
-   lda P6
    beq .ok_to_divide
    ; Dividend:
    lsr P6
    ror P5
    ror P4
    ; Divisor.
    lsr .dz_hi
    ror .dz_lo
    jmp -

.ok_to_divide
    lda P4
    sta P0
    lda P5
    sta P1
    lda .dz_lo
    sta P2
    lda .dz_hi
    sta P3
    jsr maths_div16

    ; Reset iterators.
    ; Use Y to hold iterator into buffer.
    ldx .point0_i
    ldy quads_buffer_iter

    lda .must_add
    beq .do_subtraction
    ; So here we'll add...
    lda quads_buf2_x_lo,x
    clc
    adc P0
    sta quads_buffer,y
    lda quads_buf2_x_hi,x
    adc P1
    sta quads_buffer+1,y
    jmp +
.do_subtraction
    lda quads_buf2_x_lo,x
    sec
    sbc P0
    sta quads_buffer,y
    lda quads_buf2_x_hi,x
    sbc P1
    sta quads_buffer+1,y
+
    lda #CAMERA_MIN_VALID_Z_LO
    sta quads_buffer+2,y
    lda #CAMERA_MIN_VALID_Z_HI
    sta quads_buffer+3,y

    tya
    clc
    adc #4
    sta quads_buffer_iter
    inc quads_buffer_size

    rts
; end sub quads_solve_linear
} ; !zone

; **************************************************

; Take whatever's in the buffer and project it through the camera.
!zone {
.buffer_iter    !byte   0
.vertex_iter    !byte   0

quads_project
    lda #0
    sta .vertex_iter
    sta .buffer_iter

.loop_top
    ldy .buffer_iter
    ; Load in x-position (world coordinates).
    lda quads_buffer,y
    sta CAMERA0
    lda quads_buffer+1,y
    sta CAMERA1
    ; Load in z-position (world coordinates).
    lda quads_buffer+2,y
    sta CAMERA2
    lda quads_buffer+3,y
    sta CAMERA3
    jsr camera_project_onto_plate_x
    ; Result in CAMERA0-CAMERA1.
    ; Divide by 2 because our horizontal resolution is halved.
    lda CAMERA1
    bpl +
    ; Negative so change sign first; then halve; then change sign again.
    lda #<CAMERA0
    sta P0
    lda #>CAMERA0
    sta P1
    jsr maths_adjust_vec_signs
    lsr CAMERA1
    ror CAMERA0
    jsr maths_adjust_vec_signs
    jmp ++
+
    lsr CAMERA1
    ror CAMERA0
++
    ldy .buffer_iter
    lda CAMERA0
    sta quads_buffer_proj,y
    lda CAMERA1
    sta quads_buffer_proj+1,y

    ; Y-position will always be 0 (flush with ground).
    lda #0
    sta P0
    sta P1
    ; Load in z-position.
    lda quads_buffer+2,y
    sta P2
    lda quads_buffer+3,y
    sta P3
    jsr camera_project_onto_plate_y
    ; Result in P0-P1.
    ldy .buffer_iter
    lda P0
    sta quads_buffer_proj+2,y
    lda P1
    sta quads_buffer_proj+3,y

    ; Increment .buffer_iter.
    lda .buffer_iter
    clc
    adc #4
    sta .buffer_iter

    ldx .vertex_iter
    inx
    cpx quads_buffer_size
    beq +
    stx .vertex_iter
    jmp .loop_top

+
    rts
; end sub quads_project
} ; !zone

; **************************************************

; Input vertices to this routine come from quads_buffer_proj.
; TODO: INPUT:  A = fill colour
; INPUT:    X = quad index
; NOTE: by this stage, all the vertices in quads_buffer_proj should fit
; into 8 bits so we can ignore the high byte.
!zone {
; Runs from 0 to 3...
.num_sides      !byte   0
.edges_iter     !byte   0
.current_y      !byte   0
.current_x      !byte   0
.end_x          !byte   0
.end_x_minus_3  !byte   0
.vertex_iter    !byte   0
.fill_color     !byte   0

quads_draw_filled
    lda quads_type,x
    sta .fill_color

    jsr quads_find_minmax_y
    lda quads_max_y_lo
    sec
    sbc quads_min_y_lo
    cmp #quads_c_MIN_DRAWABLE_DEPTH
    bcs .ok
;    inc EXTCOL
    rts ; EXIT POINT.

.ok
    lda #0
    sta .num_sides
    sta .edges_iter
    ; Tell line-drawing routine where to store the edges.
    lda #<quads_edges_from
    sta EDGES_LO
    lda #>quads_edges_from
    sta EDGES_HI

    ldx quads_min_y_point
    stx .vertex_iter

.lhs_loop
    lda .vertex_iter
    asl
    asl
    tax

    lda quads_buffer_proj,x
    sta LINE_X0_LO
    lda quads_buffer_proj+2,x
    sta LINE_Y0_LO

    ; Go to next point (follows ccw order).  Wrap around if necessary.
    ldx .vertex_iter
    inx
    cpx quads_buffer_size
    bne +
    ldx #0
+
    stx .vertex_iter
    txa
    asl
    asl
    tax

    lda quads_buffer_proj,x
    sta LINE_X1_LO
    lda quads_buffer_proj+2,x
    sta LINE_Y1_LO

    ldy .edges_iter
    lda .fill_color
    jsr dp_s_draw_line
    ; We will overwrite the last edge saved so it isn't recorded twice.
    dey
    sty .edges_iter
    inc .num_sides
    
    ; Are we at max-y yet?  X holds the index of the current point.
    ldx .vertex_iter
    cpx quads_max_y_point
    beq .process_rhs
    ; Still stuff to do on lhs.
    jmp .lhs_loop

.process_rhs
    ; Draw remaining sides, working down from min-y point.
    lda #<quads_edges_to
    sta EDGES_LO
    lda #>quads_edges_to
    sta EDGES_HI
    ldy #0
    sty .edges_iter
    ldx quads_min_y_point
    stx .vertex_iter

.rhs_loop
    ; Prepare the points for line-drawing routine.
    lda .vertex_iter
    asl
    asl
    tax

    lda quads_buffer_proj,x
    sta LINE_X0_LO
    lda quads_buffer_proj+2,x
    sta LINE_Y0_LO

    ; Wraparound if necessary.  
    ; NOTE: we're now moving clockwise, so decrement X.
    ldx .vertex_iter
    dex
    bpl +
    ldx quads_buffer_size
    dex
+
    stx .vertex_iter
    txa
    asl
    asl
    tax

    lda quads_buffer_proj,x
    sta LINE_X1_LO
    lda quads_buffer_proj+2,x
    sta LINE_Y1_LO

    ldy .edges_iter
    lda .fill_color
    jsr dp_s_draw_line
    dey
    sty .edges_iter
    inc .num_sides
    
    lda quads_buffer_size
    cmp .num_sides
    bne .rhs_loop

    ; This is the fill routine.
    ldx #0
    stx .edges_iter
    ldy quads_min_y_lo
    sty .current_y

.init_fill_loop
    ldx .edges_iter
    lda quads_edges_from,x
    sta .current_x
    lda quads_edges_to,x
    sta .end_x
    sec
    sbc #3
    ; If result is negative can't use burst mode.
    bcc .single_pixel
    sta .end_x_minus_3

.fill_loop
    
    ldy .current_y

    ; Attempt short cut - to draw a whole byte of pixels (=4) in one go.
    ; .current_x must be a multiple of 4...
    ; FIXME: in theory, could make this even faster by drawing a number of
    ; bytes in one go...
    lda .current_x
    and #$03
    bne .single_pixel
    ; ... and there should be at least 4 pixels to go.
    lda .end_x_minus_3
    cmp .current_x
    bcc .single_pixel

    ; Should be OK... (?!)
;    lda .edges_iter
;    sta P0
;    lda .current_x
;    sta P1
;    lda .fill_color
;    sta P2
;    jsr quads_s_modulate_color_cell
;    jsr quads_s_prepare_byte_pattern

;    lda .fill_color
;    sta P0
;    tax
;    lda QUADS_SECONDARY_COLORS,x
;    sta P1
    ldx .fill_color
    lda quads_l_FILL_BYTES,x
    ldx .current_x
    ldy .current_y
    jsr dp_s_draw_byte

    lda .current_x
    clc
    adc #4
    sta .current_x
    jmp .check_end

.single_pixel
    ldx .current_x
    ; BUG FIXED: in some paths through code, Y won't be set here!
    ldy .current_y
    lda .fill_color
    jsr dp_s_draw_pixel
    inc .current_x
    lda .current_x

.check_end
    cmp .end_x
    bcc .fill_loop
    beq .fill_loop
    ; Once current_x > right-edge, row is complete.

.end_of_row
    lda .current_y
    cmp quads_max_y_lo
    beq .end
    ; Prepare the next row.
    inc .current_y
    inc .edges_iter
    jmp .init_fill_loop

.end
    rts
; end sub quads_draw_filled
} ; !zone

; **************************************************

; This routine takes as its input the projected coordinates, stored in 
; quads_buffer_proj.  The number of vertices there is held in 
; quads_buffer_size.
!zone {
quads_find_minmax_y
    lda #0
    sta quads_min_y_point
    sta quads_max_y_point
    lda quads_buffer_proj+2
    sta quads_min_y_lo
    sta quads_max_y_lo

    ldx #1  ; Count vertices.
    ldy #4  ; Index into quads_buffer_proj.

.loop_top
    lda quads_buffer_proj+2,y
    cmp quads_min_y_lo
    bcs +
    sta quads_min_y_lo
    stx quads_min_y_point
    ; If this is min-y, it can't also be max-y, so skip over the next section.
    ; NOTE: sta and stx don't affect any processor flags, so C will still be
    ; clear here!
    bcc ++
+
    cmp quads_max_y_lo
    beq ++
    bcc ++
    sta quads_max_y_lo
    stx quads_max_y_point
++
    inx
    cpx quads_buffer_size
    beq .end
    tya
    clc
    adc #4
    tay
    jmp .loop_top
    
.end
    rts
; end sub quads_find_minmax_y
} ; !zone

; **************************************************

; New ball position - set axis-aligned coords rel. to ball.
; FIXME: temporary code - number of quads will vary!
!zone {
quads_set_aa_coords
    ldx #0

-
    ; x-component.
    lda quads_x_lo,x
    sec
    sbc ball_world_x_lo
    sta quads_aa_x_lo,x
    sta quads_rot_x_lo,x
    lda quads_x_hi,x
    sbc ball_world_x_hi
    sta quads_aa_x_hi,x
    sta quads_rot_x_hi,x
    ; z-component.
    lda quads_z_lo,x
    sec
    sbc ball_world_z_lo
    sta quads_aa_z_lo,x
    sta quads_rot_z_lo,x
    lda quads_z_hi,x
    sbc ball_world_z_hi
    sta quads_aa_z_hi,x
    sta quads_rot_z_hi,x

    inx
    cpx quads_num_vertices
    bne -
    
    rts
; end sub quads_set_aa_coords
} ; !zone

; **************************************************

; Rotate axis-aligned coords about ball with latest angle stored in 'hole'
; module.  Store in separate array.
!zone {
.iter   !byte   0
.buffer !fill   4
.X_LO = 0
.X_HI = 1
.Z_LO = 2
.Z_HI = 3

quads_rotate
    ldx #0
    stx .iter

-
    ; Fill buffer and use this as workspace until rotations complete.
    ; Then write values back into the array.
    lda quads_rot_x_lo,x
    sta .buffer
    lda quads_rot_x_hi,x
    sta .buffer+1
    lda quads_rot_z_lo,x
    sta .buffer+2
    lda quads_rot_z_hi,x
    sta .buffer+3

    ; First need to know what quadrant we're in.
;    lda .buffer+.X_HI
;    sta P0
;    lda .buffer+.Z_HI
;    sta P1
;    jsr maths_find_quadrant
;    ; Result in P2 - onto stack for now.
;    lda P2
;    pha
;    ; Rough rotation first.  Prepare args.
;    ; Address of x into P0-P1:
;    ;lda #<quad2_x_lo_rot
;    ;clc
;    ;adc .iter
;    ;sta P0
;    ;lda #>quad2_x_lo_rot
;    ;adc #0
;    ;sta P1
;    lda #<(.buffer+.X_LO)
;    sta P0
;    lda #>(.buffer+.X_LO)
;    sta P1
;    ; Address of z into P2-P3:
;    ;lda #<quad2_z_lo_rot
;    ;clc
;    ;adc .iter
;    ;sta P2
;    ;lda #>quad2_z_lo_rot
;    ;adc #0
;    ;sta P3
;    lda #<(.buffer+.Z_LO)
;    sta P2
;    lda #>(.buffer+.Z_LO)
;    sta P3
;    ; P4 = current_quadrant.
;    pla
;    sta P4
;    ; P5 = quadrants to rotate.
;    lda hole_current_rotation_quadrants 
;    sta P5
;    jsr maths_rough_rotate_vec
;
;    ; Now the refined rotation.
;    ;lda #<quad2_x_lo_rot
;    ;clc
;    ;adc .iter
;    ;sta P0
;    ;lda #>quad2_x_lo_rot
;    ;adc #0
;    ;sta P1
;    ;lda #<quad2_z_lo_rot
;    ;clc
;    ;adc .iter
;    ;sta P2
;    ;lda #>quad2_z_lo_rot
;    ;adc #0
;    ;sta P3
;    lda #<(.buffer+.X_LO)
;    sta P0
;    lda #>(.buffer+.X_LO)
;    sta P1
;    lda #<(.buffer+.Z_LO)
;    sta P2
;    lda #>(.buffer+.Z_LO)
;    sta P3
;    lda hole_current_rotation_angle     
;    sta P4
;    ; Clear 'C' flag for ccw rotation.
;    clc
;    jsr maths_rotate_vec

    lda #<(.buffer+.X_LO)
    sta MATHS0
    lda #>(.buffer+.X_LO)
    sta MATHS1
    lda #<(.buffer+.Z_LO)
    sta MATHS2
    lda #>(.buffer+.Z_LO)
    sta MATHS3
    lda hole_current_rotation_angle
    sta MATHS4
    clc
    jsr maths_s_rotate

    ; Write values back to the array.
    ldx .iter
    lda .buffer+.X_LO
    sta quads_rot_x_lo,x
    lda .buffer+.X_HI
    sta quads_rot_x_hi,x
    lda .buffer+.Z_LO
    sta quads_rot_z_lo,x
    lda .buffer+.Z_HI
    sta quads_rot_z_hi,x

    inx
    stx .iter
    cpx quads_num_vertices
    bne -

    rts
; end sub quads_rotate
} ; !zone

; **************************************************

!zone {
.iter   !byte   0

quads_s_project_and_draw
    jsr quads_find_all_minmax_z
    jsr quads_refresh_drawing_order

    ldx #0
    stx .iter
    lda quads_drawing_order,x
    tax

.loop_top
;    +clr quads_v_is_water_cell

    ; X = current quad.
    ; Copy 3 (triangle) or 4 (quad) points into buffer here.
    jsr quads_copy_current_to_buffer

    jsr quads_prepare_for_projection

    ; NOTE: don't draw if behind camera!
    bcs +
    jsr quads_project
    ; NOTE: check that quad isn't offscreen stage left or right!
    jsr quads_check_minmax_x
    bcs +

    jsr quads_clip_x
    jsr quads_final_adjustment
    ldx .iter
    lda quads_drawing_order,x
    tax
    jsr quads_draw_filled
+
    ldx .iter
    inx
    stx .iter
    cpx quads_n
    beq .end
    lda quads_drawing_order,x
    tax
    jmp .loop_top

.end
    rts
; end sub quads_s_project_and_draw
} ; !zone

; **************************************************

!zone {
quads_clear_everything
    lda #0
    sta quads_buffer_size
    sta quads_buffer_iter
    sta quads_wbuf_iter
    sta quads_wbuf_size
    ldx #0
-   sta quads_wbuf,x
    sta quads_buffer,x
    sta quads_buffer_proj,x
    inx
    cpx #24
    bne -
    rts
; end sub quads_clear_everything
} ; !zone

; **************************************************

; The quad's vertices have undergone perspective projection onto the C64
; bitmap screen.  Here we check to see if they need clipping at the left
; and/or right edges (and clip them if they do)...
!zone {
; Set this to true (=1) if we've wrapped around back to the first vertex but
; need to solve one last linear equation.
.last_time              !byte   0
.prev_vertex_state      !byte   0
.current_vertex_state   !byte   0

quads_clip_x
    ; Initialize the variables we'll be using.
    lda #quads_c_VERTEX_STATE_UNSET
    sta .prev_vertex_state
    lda #0
    sta .last_time
    sta quads_wbuf_iter
    sta quads_wbuf_size
    ; Use this as index into quads_buffer_proj.
    sta quads_buffer_iter

.loop_top
    ; LOOP STARTS HERE.
    ; Classify the current vertex.
    lda quads_buffer_iter
    asl
    asl
    tax

    lda quads_buffer_proj+1,x
    bmi .oob_left
    lda quads_buffer_proj,x
;    sec
;    sbc #quads_c_MAX_VALID_X_PLUS_ONE_LO
    cmp #quads_c_MAX_VALID_X_PLUS_ONE_LO
    lda quads_buffer_proj+1,x
    sbc #quads_c_MAX_VALID_X_PLUS_ONE_HI
    +bge_s .oob_right
    lda #quads_c_VERTEX_STATE_IN_BOUNDS
    sta .current_vertex_state
    ; Action depends on the state of the previous vertex.
    lda .prev_vertex_state
    bmi .add_current_vertex
    beq .add_current_vertex
    ; Here we have either scenario (iii) (x=0) or scenario (iv) (x=159).
    ; SOLVE LINEAR & ADD VERTEX.
    ; Prepare the arguments for our call to the linear equation solver.
    ldx #0
    cmp #quads_c_VERTEX_STATE_OOB_LEFT
    beq +
    ldx #159
+   jsr quads_solve_linear_x
    jmp .add_current_vertex

.oob_left
    lda #quads_c_VERTEX_STATE_OOB_LEFT
    jmp +
.oob_right
    lda #quads_c_VERTEX_STATE_OOB_RIGHT
+   sta .current_vertex_state
    ; Let's look at the previous vertex.
    lda .prev_vertex_state
    ; If unset, nothing to do.
    bmi .set_prev_vertex
    ; If in bounds, we must here solve a linear equation and add a new vertex.
    ; This will be either scenario (ii) x=0 or (v) x=159.
    beq .linear
    ; The previous vertex was also out of bounds.  If states are the same, both
    ; vertices are on the same side of the screen, so nothing to do.  If
    ; they're different, this 'line' stretches right across the screen and
    ; there are two linear equations to solve and two new vertices to add.
    cmp .current_vertex_state
    beq .set_prev_vertex
    ; So there are two linear equations to solve...
    ; NOTE: the order in which they're solved is important!
    ; Accumulator still holds '.prev_vertex_state' so we'll use that to see 
    ; which order we should solve the equations.
    cmp #quads_c_VERTEX_STATE_OOB_LEFT
    bne .right_to_left
    ; Previous vertex was on the left, so we're moving left-to-right.  So x=0
    ; comes first.
    ldx #0
    jsr quads_solve_linear_x
    ldx #159
    jsr quads_solve_linear_x
    jmp .set_prev_vertex
.right_to_left
    ldx #159
    jsr quads_solve_linear_x
    ldx #0
    jsr quads_solve_linear_x
    jmp .set_prev_vertex

.linear
    ; Previous vertex was in bounds; current one is out of bounds.
    ; This is either scenario (ii) (x=0) or (v) (x=159).  We need to look at
    ; .current_vertex_state to determine which scenario applies here.
    ldx #0
    lda .current_vertex_state
    cmp #quads_c_VERTEX_STATE_OOB_LEFT
    beq +
    ldx #159
+   jsr quads_solve_linear_x
    jmp .set_prev_vertex

.add_current_vertex
    ; Skip over this step if this is .last_time!
    lda .last_time
    bne .next
    ; Source = quad_buffer_proj, destination = working_buffer
    lda quads_buffer_iter
    asl
    asl
    tax
    lda quads_wbuf_size
    asl
    asl
    tay
    lda quads_buffer_proj,x
    sta quads_wbuf,y
    lda quads_buffer_proj+1,x
    sta quads_wbuf+1,y
    lda quads_buffer_proj+2,x
    sta quads_wbuf+2,y
    lda quads_buffer_proj+3,x
    sta quads_wbuf+3,y
    inc quads_wbuf_size

.set_prev_vertex
    lda .current_vertex_state
    sta .prev_vertex_state

.next
    ; Don't bother if this is our last time through - i.e. we've already
    ; wrapped around.
    lda .last_time
    bne .end
    ; Go to the next vertex.
    ldx quads_buffer_iter
    inx
    stx quads_buffer_iter
    cpx quads_buffer_size
    beq .wraparound
    jmp .loop_top

.wraparound
    ; Iterator into quad_buffer_proj must once more be pointing to the first
    ; vertex, but this time round the 'previous vertex' will be set.
    lda #0
    sta quads_buffer_iter
    inc .last_time
    jmp .loop_top

.end
    ; FIXME: inefficient!!!
    ; Copy working_buffer vertices into quad_buffer_proj.
    lda quads_wbuf_size
    asl
    asl
    tax
    dex
    ; X = index into working_buffer.  Counts from last index to 0.  As soon as
    ; it becomes negative, we're finished.
-   lda quads_wbuf,x
    sta quads_buffer_proj,x
    dex
    bpl -
    lda quads_wbuf_size
    sta quads_buffer_size

    rts
; end sub quads_clip_x
} ; !zone

; **************************************************

; INPUTS:   X = x value to solve for (low byte)
!zone {
.vertexA_iter   !byte   0
.vertexB_iter   !byte   0
.x_lo           !byte   0
.dx_lo          !byte   0
.dx_hi          !byte   0
.dy_lo          !byte   0
.dy_hi          !byte   0
.must_add       !byte   0
.numerator_lo   !byte   0
.numerator_hi   !byte   0

quads_solve_linear_x
    ; NOTE: x_hi is always 0.
    stx .x_lo
    ; We subtract the offset by default.
    lda #0
    sta .must_add

    ; Prepare the iterators.  By multiplying the vertex index by 4, we will
    ; have an index into the correct 4-byte sequence of quad_buffer_proj.
    lda quads_buffer_iter
    asl
    asl
    sta .vertexB_iter
    ; If current vertex = 0, we need to 'wraparound' to get the previous
    ; vertex.
    ldx quads_buffer_iter
    bne +
    ldx quads_buffer_size
+   dex
    txa
    asl
    asl
    sta .vertexA_iter

    ; Calculate dx.  Adjust sign if negative.
    ldx .vertexB_iter
    ldy .vertexA_iter
    lda quads_buffer_proj,x
    sec
    sbc quads_buffer_proj,y
    sta .dx_lo
    lda quads_buffer_proj+1,x
    sbc quads_buffer_proj+1,y
    sta .dx_hi
    bpl +
    lda #<.dx_lo 
    sta P0
    lda #>.dx_lo 
    sta P1
    jsr maths_adjust_vec_signs
+

    ; Now dy.  If the result of (By-Ay) is negative, we must add the offset
    ; value to By.  Record this fact before changing dy to positive.
    ldx .vertexB_iter
    ldy .vertexA_iter
    lda quads_buffer_proj+2,x
    sec
    sbc quads_buffer_proj+2,y
    sta .dy_lo
    lda quads_buffer_proj+3,x
    sbc quads_buffer_proj+3,y
    sta .dy_hi
    bpl +
    ; Record that we must add the offset to By.
    inc .must_add
    lda #<.dy_lo 
    sta P0
    lda #>.dy_lo 
    sta P1
    jsr maths_adjust_vec_signs
+

    ; Now prepare the numerator.  Adjust the sign if negative.
    ldx .vertexB_iter
    lda quads_buffer_proj,x
    sec
    sbc .x_lo
    sta .numerator_lo
    lda quads_buffer_proj+1,x
    sbc #0
    sta .numerator_hi
    bpl +
    lda #<.numerator_lo
    sta P0
    lda #>.numerator_lo
    sta P1
    jsr maths_adjust_vec_signs
+

    ; We now multiply this numerator by dy.
    lda .dy_lo
    sta P0
    lda .dy_hi
    sta P1
    lda .numerator_lo
    sta P2
    lda .numerator_hi
    sta P3
    jsr maths_mul16
    ; The result is in P4-P7.
    ; FIXME: assuming a 16-bit result!!!

    ; Result may be 24-bit.  If so, keep halving dividend and divisor until
    ; 3rd byte of dividend (P6) is 0.
-   lda P6
    beq .ok_to_divide
    ; Dividend:
    lsr P6
    ror P5
    ror P4
    ; Divisor:
    lsr .dx_hi
    ror .dx_lo
    jmp -

.ok_to_divide
    ; Divide this by dx.
    lda P4
    sta P0
    lda P5
    sta P1
    lda .dx_lo
    sta P2
    lda .dx_hi
    sta P3
    jsr maths_div16
    ; The result is in P0-P1.

    ; We will place the result for the y-position of the new vertex in
    ; .dy_lo/.dy_hi, since it won't be needed for the rest of the routine.
    ; FIXME: could use self-modifying code here when everything's settled...
    ldx .vertexB_iter
    lda .must_add
    bne .addition
    ; So subtraction.
    lda quads_buffer_proj+2,x
    sec
    sbc P0
    sta .dy_lo
    lda quads_buffer_proj+3,x
    sbc P1
    sta .dy_hi
    jmp .add_vertex
.addition
    lda quads_buffer_proj+2,x
    clc
    adc P0
    sta .dy_lo
    lda quads_buffer_proj+3,x
    adc P1
    sta .dy_hi

.add_vertex
    ; Let's use X as index into working_buffer.
    lda quads_wbuf_size
    asl
    asl
    tax
    lda .x_lo
    sta quads_wbuf,x
    lda #0
    sta quads_wbuf+1,x
    lda .dy_lo
    sta quads_wbuf+2,x
    lda .dy_hi
    sta quads_wbuf+3,x
    inc quads_wbuf_size

    rts
; end sub quads_solve_linear_x
} ; !zone

; **************************************************

; If we find any points which have y>=200, bump them back to y=199.
!zone {
quads_final_adjustment
    ldx #0

.loop_top
    txa
    asl
    asl
    tay

    lda quads_buffer_proj+2,y
    sec
    sbc #<quads_c_SCREEN_HEIGHT
    lda quads_buffer_proj+3,y
    sbc #>quads_c_SCREEN_HEIGHT
    +blt_s .skip
    lda #<(quads_c_SCREEN_HEIGHT-1)
    sta quads_buffer_proj+2,y
    lda #>(quads_c_SCREEN_HEIGHT-1)
    sta quads_buffer_proj+3,y
.skip

    inx
    cpx quads_buffer_size
    bne .loop_top
    rts
; end sub quads_final_adjustment
} ; !zone

; **************************************************

!zone {
quads_init_drawing_order
    ldx #0
-   txa
    sta quads_drawing_order,x
    inx
    cpx #quads_c_MAX_N
    bne -
    rts
; end sub quads_init_drawing_order
} ; !zone

; **************************************************

; INPUTS:   P3 = index #1, P4 = index #2
; OUTPUTS:  C flag set if should be swapped, otherwise clear
!zone {
quads_compare
    ldx P3
    ldy P4

    lda quads_rot_max_z_lo,x
    sec
    sbc quads_rot_max_z_lo,y
    lda quads_rot_max_z_hi,x
    sbc quads_rot_max_z_hi,y
    +blt_s .must_swap

    ; Don't swap - P3 already >= P4.
    clc
    rts

.must_swap
    sec
    rts
; end sub quads_compare
} ; !zone

; **************************************************

!zone {
quads_refresh_drawing_order
    ; Load in the 'compare' routine.
    lda #<quads_compare
    sta utils_s_comp+1
    lda #>quads_compare
    sta utils_s_comp+2
    ; And the address of the indices array into P0-P1.
    lda #<quads_drawing_order
    sta P0
    lda #>quads_drawing_order
    sta P1
    ; And the number of elements (= quads) into P2.
    lda quads_n
    sta P2
    jsr utils_s_bubble_sort
    rts
; end sub quads_refresh_drawing_order
} ; !zone

; **************************************************

!zone {
.iter   !byte   0

quads_find_all_minmax_z
    ldx #0
    stx .iter

-   jsr quads_find_minmax_z

    ldx .iter
;    lda quads_min_z_lo
;    sta quads_rot_min_z_lo,x
;    lda quads_min_z_hi
;    sta quads_rot_min_z_hi,x
    lda quads_max_z_lo
    sta quads_rot_max_z_lo,x
    lda quads_max_z_hi
    sta quads_rot_max_z_hi,x
;    lda quads_min_z_point
;    sta quads_rot_min_z_points,x
;    lda quads_max_z_point
;    sta quads_rot_max_z_points,x

    inx
    stx .iter
    cpx quads_n
    bne -

    rts
; end sub quads_find_all_minmax_z
} ; !zone

; **************************************************

; INPUTS:   quads_buffer_proj
; OUTPUTS:  C flag clear if all OK; set if quad is offscreen (and
;           therefore shouldn't be drawn).
; NOTE: quads_buffer_proj = x0-lo,x0-hi,z0-lo,z0-hi, x1-lo,x1-hi,etc.
!zone {
.min_x_lo   !byte   0
.min_x_hi   !byte   0
.max_x_lo   !byte   0
.max_x_hi   !byte   0

quads_check_minmax_x
    ; To begin with, set min and max to the first (0th) vertex.
    ldx #0
    lda quads_buffer_proj,x
    sta .min_x_lo
    sta .max_x_lo
    lda quads_buffer_proj+1,x
    sta .min_x_hi
    sta .max_x_hi

    ; Min-x first.
    ldy #1
    ldx #4
.min_x_loop
    lda quads_buffer_proj,x
    sec
    sbc .min_x_lo
    lda quads_buffer_proj+1,x
    sbc .min_x_hi
    +bge_s +
    ; Set new min-x.
    lda quads_buffer_proj,x
    sta .min_x_lo
    lda quads_buffer_proj+1,x
    sta .min_x_hi
+
    iny
    cpy quads_buffer_size
    beq .min_x_complete
    txa
    clc
    adc #4
    tax
    jmp .min_x_loop

.min_x_complete
    ; If min-x is >= 160
    lda .min_x_lo
    sec
    sbc #<160
    lda .min_x_hi
    sbc #>160
    +blt_s .check_max_x
    sec
    rts ; EXIT POINT.

.check_max_x
    ldy #1
    ldx #4
.max_x_loop
    lda .max_x_lo
    sec
    sbc quads_buffer_proj,x
    lda .max_x_hi
    sbc quads_buffer_proj+1,x
    +bge_s +
    ; Set new max-x.
    lda quads_buffer_proj,x
    sta .max_x_lo
    lda quads_buffer_proj+1,x
    sta .max_x_hi
+
    iny
    cpy quads_buffer_size
    beq .max_x_complete
    txa
    clc
    adc #4
    tax
    jmp .max_x_loop
    
.max_x_complete
    ; If this is negative, quad is offscreen stage left.
    bit .max_x_hi
    bpl .ok
    sec
    rts

.ok
    clc
    rts
; end sub quads_check_minmax_x
} ; !zone

; **************************************************

; X = quad #.
; NOTE: preserve value of X!
!zone {
.vertex_to_omit !byte   0

quads_copy_current_to_buffer
    +utils_m_save_x_to_stack  

    ; We need an index into the quads_rot_ arrays: index*4.
    ; Store this in MATHS0.  And the 'end-iter' in MATHS1.
    txa
    asl
    asl
    sta MATHS0
    clc
    adc #4
    sta MATHS1

    ; Look up & store vertex-to-omit (in case this is a triangle).
    ; NOTE: add 'begin-iter' (stored in MATHS0) to this so two indices are
    ; synchronized.
    lda #4
    sta quads_buf2_size
    lda #(-1)
    sta .vertex_to_omit

    lda quads_triangle_indices,x
    bmi +   ; Not a triangle.
    tax
    lda quads_triangles_vertex_to_omit,x
    clc
    adc MATHS0
    sta .vertex_to_omit
    dec quads_buf2_size
+

    ; Use Y as index into quads_buf2; X as index into quads_rot.
    ; This is the 'copy-to-buffer' routine.
    ldy #0
    ldx MATHS0

.loop
    cpx .vertex_to_omit
    beq .next
    lda quads_rot_x_lo,x
    sta quads_buf2_x_lo,y
    lda quads_rot_x_hi,x
    sta quads_buf2_x_hi,y
    lda quads_rot_z_lo,x
    sta quads_buf2_z_lo,y
    lda quads_rot_z_hi,x
    sta quads_buf2_z_hi,y
    ; BUG FIXED: only increment Y if we added a vertex!
    iny

.next
    inx
    cpx MATHS1
    bne .loop
    
    ; Now find & record min/max z, and indices for those.
    ldx #0
    lda quads_buf2_z_lo,x
    sta quads_buf2_min_z_lo
    sta quads_buf2_max_z_lo
    lda quads_buf2_z_hi,x
    sta quads_buf2_min_z_hi
    sta quads_buf2_max_z_hi
    stx quads_buf2_min_z_i
    stx quads_buf2_max_z_i
    inx
.loop2
    lda quads_buf2_z_lo,x
    sec
    sbc quads_buf2_min_z_lo
    lda quads_buf2_z_hi,x
    sbc quads_buf2_min_z_hi
    +bge_s .check_max_z
    ; Fall through - set min-z.
    lda quads_buf2_z_lo,x
    sta quads_buf2_min_z_lo
    lda quads_buf2_z_hi,x
    sta quads_buf2_min_z_hi
    stx quads_buf2_min_z_i
.check_max_z
    lda quads_buf2_z_lo,x
    sec
    sbc quads_buf2_max_z_lo
    lda quads_buf2_z_hi,x
    sbc quads_buf2_max_z_hi
    +blt_s .next2
    ; Fall through - set max-z.
    lda quads_buf2_z_lo,x
    sta quads_buf2_max_z_lo
    lda quads_buf2_z_hi,x
    sta quads_buf2_max_z_hi
    stx quads_buf2_max_z_i
.next2
    inx
    cpx quads_buf2_size
    beq .end
    jmp .loop2

.end
    +utils_m_restore_x_from_stack  

    rts
; end sub quads_copy_current_to_buffer
} ; !zone

; **************************************************

; FIXME: random selection of a slot would look much better!
!zone {
quads_s_spawn_new_shimmer
    jsr rand_s_get
    tax
    cpx quads_v_shimmers_n
    bcs +
    lda quads_v_states,x
    bmi .found
+
    rts ; EXIT POINT.

.found
    ; State initially set to 4, which is invalid...  But during the first 
    ; update it will immediately be decremented to 3, allowing the correct
    ; first color in the sequence to be shown.  (So set frame count to 1.)
    lda #quads_c_NUM_SHIMMER_COLORS
    sta quads_v_states,x
    lda #1
    sta quads_v_frame_counts,x    

    lda quads_v_shimmers_bitmap_addrs_lo,x
    sta MATHS2
    lda quads_v_shimmers_bitmap_addrs_hi,x
    sta MATHS3
    ldy #7
    lda #$ff
-
    sta (MATHS2),y
    dey
    bpl -

    ; Within this cell, pick a random row (0-7) and a random pattern (0-3).
    jsr rand_s_get
    pha
    and #$07
    tay
    pla
    and #$03
    tax
    lda quads_l_SHIMMER_PATTERNS,x
    sta (MATHS2),y

.end
    rts
; end sub quads_s_spawn_new_shimmer
} ; !zone

; **************************************************

!zone {
quads_s_update_shimmers
    lda quads_v_shimmers_n
    beq .end

    jsr quads_s_spawn_new_shimmer
w
.updates
    ; Update existing ones.
    ldx #0
.loop
    lda quads_v_states,x
    ; NOTE: inactive if negative.
    bmi .next
    dec quads_v_frame_counts,x
    bne .next
    dec quads_v_states,x
    bmi .next
    lda #quads_c_SHIMMER_FRAME_RATE 
    sta quads_v_frame_counts,x

    ; Set color.
    lda quads_v_vram_addrs_lo,x
    sta P0
    lda quads_v_vram_addrs_hi,x
    sta P1
    ldy quads_v_states,x
    lda quads_l_SHIMMER_COLORS,y
    ldy #0
    sta (P0),y

.next
    inx
    cpx quads_v_shimmers_n
    bne .loop

.end
    rts
; end sub quads_s_update_shimmers
} ; !zone

; **************************************************

!zone {
quads_s_reset_shimmers
    ldx #quads_c_MAX_SHIMMERS-1

    lda #(-1)
-
    sta quads_v_states,x
    dex
    bpl -
    lda #0
    sta quads_v_shimmers_n

    rts
; end sub quads_s_reset_shimmers
} ; !zone

; **************************************************

; INPUTS:   BITMAP_LO/HI = bitmap address (of byte #0),
;           VM_LO/HI = address of video matrix,
;           COLORS_LO/HI = address of color RAM.
!zone {
quads_s_add_shimmer
    ldx quads_v_shimmers_n
    cpx #quads_c_MAX_SHIMMERS
    beq .end
    
    ; Check that this cell is colored LIGHT_BLUE (in color RAM).
    ldy #0
    lda (COLORS_LO),y
    and #$0f
    cmp #LIGHT_BLUE
    bne .end

    lda BITMAP_LO
    sta quads_v_shimmers_bitmap_addrs_lo,x
    lda BITMAP_HI
    sta quads_v_shimmers_bitmap_addrs_hi,x
    lda VM_LO
    sta quads_v_vram_addrs_lo,x
    lda VM_HI
    sta quads_v_vram_addrs_hi,x
    
    inx
    stx quads_v_shimmers_n

.end
    rts
; end sub quads_s_add_shimmer
} ; !zone

; **************************************************

; NOTE: kernal is switched out when this routine is called...
!zone {
.CELL_COUNT_LO  = CAMERA0
.CELL_COUNT_HI  = CAMERA1
.PATTERN        = CAMERA2
.NUM_CELLS = 8*40
.BITMAP_START = gfxs_c_BITMAP_BASE+(16*40*8) 
.COLRAM_START = COLOR_RAM+(16*40)
.DISPLAY_START = gfxs_c_DISPLAY_BASE+(16*40) 

quads_s_add_textures
    jsr quads_s_reset_shimmers

    ; We'll keep going till this decrements to (-1).
    lda #<(.NUM_CELLS-1)
    sta .CELL_COUNT_LO
    lda #>(.NUM_CELLS-1)
    sta .CELL_COUNT_HI

    ; Set up the bitmap and color RAM pointers.
    lda #<.BITMAP_START
    sta BITMAP_LO
    lda #>.BITMAP_START
    sta BITMAP_HI
    lda #<.COLRAM_START
    sta COLORS_LO
    lda #>.COLRAM_START
    sta COLORS_HI
    lda #<.DISPLAY_START
    sta VM_LO
    lda #>.DISPLAY_START
    sta VM_HI

.loop
    ldy #7
    lda (BITMAP_LO),y
    cmp #$55    ; sand
    beq .valid_byte
    cmp #$aa    ; fway/green
    beq .valid_byte
    cmp #$ff    ; water
    beq .valid_byte

.next
    ; First byte we looked at for current cell wasn't valid, so let's go to 
    ; the next cell if there is one.
    lda .CELL_COUNT_LO
    sec
    sbc #1
    sta .CELL_COUNT_LO
    bcs +
    dec .CELL_COUNT_HI
    bmi .end

+
    lda BITMAP_LO
    clc
    adc #8
    sta BITMAP_LO
    lda BITMAP_HI
    adc #0
    sta BITMAP_HI

    ; NOTE: color RAM and video (matrix) RAM will wraparound in sync!
    inc COLORS_LO
    inc VM_LO
    bne .loop
    inc COLORS_HI
    inc VM_HI
    ; NOTE: VM_HI is never going to wraparound to zero!
    bne .loop
    
.valid_byte
    sta .PATTERN
-
    dey
    bmi .do_texturing
    lda (BITMAP_LO),y
    cmp .PATTERN
    bne .next
    beq -

.do_texturing
    ; NOTE: A will still hold '.PATTERN' value.
    ; Isolating least significant two bits will give us a terrain index
    ; (omitting 0).
    and #$03
    cmp #$03
    bne +
    ; So add shimmer here!
    ; We need to check the color at this cell in color RAM.  But to do that,
    ; need to switch the kernal back in!
    +utils_m_kernal_in
    jsr quads_s_add_shimmer
    +utils_m_kernal_out
    jmp .next
+
    ; TODO: texture to fway/green or sand!!!
    jsr quads_s_texture_fway_sand
    jmp .next

.end
    rts
; end sub quads_s_add_textures
} ; !zone

; **************************************************

; INPUTS:   A = terrain type (either 1 or 2),
;           BITMAP_LO/HI = bitmap address, VM_LO/HI = vram color address,
;           COLORS_LO/HI = color RAM address.
!zone {
.BASE_THRESHOLD = 225
.GREEN_THRESHOLD = 245
.MYCOLORS !byte 0,LIGHT_RED,LIGHT_GREEN
.MY_THRESHOLD = LINE_X0_LO 

quads_s_texture_fway_sand
    ; Terrain type into X register.
    tax
    lda #.BASE_THRESHOLD
    cpx #ball_c_TERRAIN_BUNKER
    beq +
    ldy round_v_must_putt
    beq +   ; i.e. if not putting
    lda #.GREEN_THRESHOLD
+
    sta .MY_THRESHOLD

    ; Check that color of this cell is OK.
    ldy #0
    lda (VM_LO),y
    cmp #(YELLOW<<4)|GREEN
    bne .end

    jsr rand_s_get_fast
    cmp .MY_THRESHOLD
    bcc .end
    ; Mask out bits 0 and 1 to get value in range [0,4).  Use this to index
    ; texture colors for this terrain.  Need to bank the kernal back in to be
    ; able to write to color RAM!
    and #$03
    clc
    adc quads_l_TEXTURE_COLOR_OFFSETS,x
    tax
    +utils_m_kernal_in
    lda quads_l_TEXTURE_COLORS,x
    sta (COLORS_LO),y
    +utils_m_kernal_out

    ; Now must modify bitmap so there's a random pixel in this cell that uses
    ; texture color.
    jsr rand_s_get_fast
    and #$07
    tay
    jsr rand_s_get_fast
    and #$07
    tax
    lda quads_l_TEXTURES,x
    ora (BITMAP_LO),y
    sta (BITMAP_LO),y

.end
    rts
; end sub quads_s_texture_fway_sand
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

quads_c_SIZE = *-quads_c_BEGIN
