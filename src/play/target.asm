; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


target_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
target_c_SW_SPR_NUM = 7
TARGET_SPR_Y_OFFSET = 20

target_c_NUM_DEPTH_LEVELS = 9
; NOTE: final entry is placeholder for hole.  Must use sprite's own color 
; here because target sprite can't be multiplexed - is always below horizon.
target_l_FABRIC_COLORS !byte GREY3,GREY3,GREY3,WHITE,WHITE,WHITE,WHITE,WHITE,0

target_l_DIST_LO  
    !byte <(180*hole_c_PIXELS_PER_YARD)
    !byte <(120*hole_c_PIXELS_PER_YARD)
    !byte <(70*hole_c_PIXELS_PER_YARD)
    !byte <(55*hole_c_PIXELS_PER_YARD)
    !byte <(40*hole_c_PIXELS_PER_YARD)
    !byte <(30*hole_c_PIXELS_PER_YARD)
    !byte <(26*hole_c_PIXELS_PER_YARD)    
    !byte <(20*hole_c_PIXELS_PER_YARD)
target_l_DIST_HI
    !byte >(180*hole_c_PIXELS_PER_YARD)
    !byte >(120*hole_c_PIXELS_PER_YARD)
    !byte >(70*hole_c_PIXELS_PER_YARD)
    !byte >(55*hole_c_PIXELS_PER_YARD)
    !byte >(40*hole_c_PIXELS_PER_YARD)
    !byte >(30*hole_c_PIXELS_PER_YARD)
    !byte >(26*hole_c_PIXELS_PER_YARD)    
    !byte >(20*hole_c_PIXELS_PER_YARD)

target_l_TOP_OF_POLE_OFFSETS    !byte   15,13,11,9,6,4,2,0,20
target_l_POLE_HEIGHTS           !byte   6,8,10,12,15,17,19,21,0
; These widths are measured in DOUBLE PIXELS and include the pole.
target_l_WIDTHS !byte   2,2,3,3,3,4,4,4,0
; NOTE: use these to isolate a column of the flag sprite (i.e. one double
; pixel width) if it happens to be behind a tree trunk.
target_l_COLUMN_MASKS   !byte   $3f,$cf,$f3,$fc
target_l_FABRIC_PATTERNS
    !byte   $80,$90,$90
    !byte   $80,$94,$94,$94
    !byte   $80,$90,$94,$94,$84
    !byte   $80,$90,$95,$95,$95,$95,$85

target_l_FABRIC_PATTERN_OFFSETS     !byte   0,0,3,7,7,12,12,12
target_l_FABRIC_PATTERN_OFFSETS_END !byte   3,3,7,12,12,19,19,19
; Where these pattern bytes are to be drawn on the destination sprite.
target_l_FABRIC_PATTERN_DEST_OFFSETS    !byte   45,39,33,27,18,12,6,0
target_c_DATA_PTR = (play_l_BLANK_SPRITE_FLAG-$c000)/64    
target_l_HAS_SHADOW !byte   0,0,0,0,0,1,1,1,0
    
target_c_POLE_COLLISION_MIN_DEPTH = 3
target_c_POLE_COLLISION_MAX_DEPTH = 7


; *****************
; *** VARIABLES ***
; *****************
target_base_x_lo    !byte   0
target_base_x_hi    !byte   0
target_base_z_lo    !byte   0
target_base_z_hi    !byte   0
target_x_lo         !byte   0
target_x_hi         !byte   0
target_z_lo         !byte   0
target_z_hi         !byte   0

target_v_current_depth  !byte   0

target_coll_lower_z_lo  !byte   0
target_coll_lower_z_hi  !byte   0
target_coll_upper_z_lo  !byte   0
target_coll_upper_z_hi  !byte   0

target_hw_spr_num   !byte   target_c_SW_SPR_NUM

target_v_overlap_char_l !byte   0
target_v_overlap_char_r !byte   0
target_v_overlap_char_t !byte   0
target_v_overlap_char_b !byte   0
target_v_overlap_offset_x   !byte   0
target_v_overlap_offset_y   !byte   0
target_v_foliage_masks_offset   !byte   0

; A 64 byte buffer which will hold masks for any foliage bytes that may
; overlap the flag sprite.  Arranged in two columns of four char rows.
target_v_foliage_masks  !fill   64
target_v_top_of_pole    !byte   0
target_v_fabric_color   !byte   0


; *******************
; ****** MACROS *****
; *******************
;!macro target_reset_hw_sprite {
;    lda #target_c_SW_SPR_NUM 
;    sta target_hw_spr_num
;} ; target_reset_hw_sprite


; *******************
; *** SUBROUTINES ***
; *******************
; INPUT:    P0/P1 = data address: 4 bytes (x-lo/hi,z-lo/hi)
;           X-coordinate will (probably) always be 0.
!zone {
target_init
    ldy #0
-   lda (P0),y
    sta target_base_x_lo,y 
    sta target_x_lo,y
    iny
    cpy #4
    bne -

    ; Initialize sprite.
    lda #target_c_DATA_PTR
    sta spr_v_current_ptr+target_c_SW_SPR_NUM
;    ldx #target_c_SW_SPR_NUM
;    +spr_m_enable
    lda #CYAN
    sta spr_v_color+target_c_SW_SPR_NUM
    lda #0
    sta spr_v_hires+target_c_SW_SPR_NUM

    rts
; end sub target_init
} ; !zone

; **************************************************

; INPUT:    P0-P1 = origin x, P2-P3 = origin z
!zone {
target_reset_base_position
    lda P0
    clc
    adc target_base_x_lo
    sta target_x_lo
    lda P1
    adc target_base_x_hi
    sta target_x_hi

    lda P2
    clc
    adc target_base_z_lo
    sta target_z_lo
    lda P3
    adc target_base_z_hi
    sta target_z_hi

    rts
; end sub target_reset_base_position
} ; !zone

; **************************************************

!zone {
target_s_rotate_to_pos_z_axis
    lda target_x_lo
    sta maths_mod08
    lda target_x_hi
    sta maths_mod09
    lda target_z_lo
    sta maths_mod10
    lda target_z_hi
    sta maths_mod11
    lda #<target_x_lo
    sta maths_mod12
    lda #>target_x_lo
    sta maths_mod13
    lda #<target_z_lo
    sta maths_mod14
    lda #>target_z_lo
    sta maths_mod15
    sec
    jsr maths_s_rotate_vec_to_pos_z_axis
    rts
; end sub target_s_rotate_to_pos_z_axis
} ; !zone

; **************************************************

; NOTE: we don't save these in special 'aa' array because they're not
; needed for collision checks...
!zone {
target_s_recalculate_aa_coords
    lda target_base_x_lo
    sec
    sbc ball_world_x_lo
    sta target_x_lo
    lda target_base_x_hi
    sbc ball_world_x_hi
    sta target_x_hi
    lda target_base_z_lo
    sec
    sbc ball_world_z_lo
    sta target_z_lo
    lda target_base_z_hi
    sbc ball_world_z_hi
    sta target_z_hi
    rts
; end sub target_s_recalculate_aa_coords
} ; !zone

; **************************************************

; NOTE: this routine assumes that the target is aligned with the positive
;       z-axis.  Therefore the distance from origin (i.e. the ball)
;       is its z-position.
; OUTPUT:   P0-P1 (lo/hi byte).
!zone {
target_get_distance
    lda target_z_lo
    sta P0
    lda target_z_hi
    sta P1
    rts
; end sub target_get_distance
} ; !zone

; **************************************************

!zone {
target_s_draw
    lda #target_c_SW_SPR_NUM
    sta target_hw_spr_num   
    ldx target_v_current_depth

    ; If this happens to be the hole sprite, we'll override this.
    lda #GREY1  ;target_l_COLORS,x
    sta spr_v_color+target_c_SW_SPR_NUM

    lda #target_c_DATA_PTR ;8
    sta spr_v_current_ptr+target_c_SW_SPR_NUM
    cpx #target_c_NUM_DEPTH_LEVELS-1 ;3
    bne +
    ; We only need to draw the hole, rather than building a flag complete
    ; with masks.  
    jsr target_s_prepare_hole_sprite
    jmp ++
+

    jsr target_s_check_if_behind_trunk
    jsr target_s_build_sprite

++
    ldx #target_c_SW_SPR_NUM
;    +spr_m_enable
    ldy #target_c_SW_SPR_NUM
    jsr spr_s_write_to_vic_ii

    rts
    
; end sub target_s_draw
} ; !zone

; **************************************************

!zone {
target_s_reset
;    ldx target_v_current_depth
;    lda #GREY2  ;TARGET_COLORS,x
;    sta spr_v_color+target_c_SW_SPR_NUM

    lda #target_c_SW_SPR_NUM 
    sta target_hw_spr_num
    
    lda #target_c_DATA_PTR ;8
    sta spr_v_current_ptr+target_c_SW_SPR_NUM
;    ldx #target_c_SW_SPR_NUM
;    +spr_m_enable 
    jsr target_s_refresh_draw
    rts
; end sub target_s_reset
} ; !zone

; **************************************************

!zone {
target_s_project
    jsr target_determine_depth
    
    ; NOTE: because of waypoints, flag may not always be center of screen,
    ; so projection onto x-axis is also required!!!
    lda #0
    sta P0
    sta P1
    lda target_z_lo
    sta P2
    lda target_z_hi
    sta P3
    jsr camera_project_onto_plate_y

    ; Y-position in P0-P1.  We only need to consider the low byte.
    ; Subtract 20 from this value so that projected point is the BASE of 
    ; the flag sprite.
    lda P0
    clc
    adc #spr_c_VISIBLE_ALL_T-TARGET_SPR_Y_OFFSET
    sta spr_v_y+target_c_SW_SPR_NUM

    ; ... and record this for ball-pole collisions.
    ldx target_v_current_depth
    clc
    adc target_l_TOP_OF_POLE_OFFSETS,x
    sta target_v_top_of_pole

    lda target_x_lo
    sta CAMERA0
    lda target_x_hi
    sta CAMERA1
    lda target_z_lo
    sta CAMERA2
    lda target_z_hi
    sta CAMERA3
    jsr camera_project_onto_plate_x
    ; Result in P0-P1.
    lda CAMERA0
    clc
    adc #spr_c_VISIBLE_ALL_L
    and #$fe
    sta spr_v_x_lo+target_c_SW_SPR_NUM
    lda CAMERA1
    adc #0
    sta spr_v_x_hi+target_c_SW_SPR_NUM

    jsr target_s_calc_overlapping_chars

    ; Reset foliage masks - every byte should be $ff.
    ldx #0
    lda #$ff
-
    sta target_v_foliage_masks,x
    inx
    cpx #64
    bne -

    rts
; end sub target_s_project
} ; !zone

; **************************************************

!zone {
target_determine_depth
    ; X keeps track of 'depth index'.
    ldx #0

-
    lda target_z_lo
    sec
    sbc target_l_DIST_LO,x
    lda target_z_hi
    sbc target_l_DIST_HI,x
    +bge_s .done
    
    inx
    cpx #target_c_NUM_DEPTH_LEVELS-1
    bne -


.done
    stx target_v_current_depth

    ; If depth is max but player isn't on green, draw flag regardless
    ; of distance.
    cpx #target_c_NUM_DEPTH_LEVELS-1
    bne +
    ldx round_v_current_player    
    lda players_v_terrain,x
    cmp #ball_c_TERRAIN_GREEN_FWAY
    beq +
    dec target_v_current_depth
+
    ; Set color for SPMC0 - used for flag fabric.
    ldx target_v_current_depth
    lda target_l_FABRIC_COLORS,x
    sta target_v_fabric_color

    rts
; end sub target_determine_depth
} ; !zone

; **************************************************

; NOTE: not actually a collision box!  Just the upper and lower positions
; on the z-axis that we'll check the ball against.  No need to do this for
; the x-axis because the target will always be aligned on that (i.e. x=0).
!zone {
target_set_collbox
    lda target_z_lo
    clc
    adc #8
    sta target_coll_upper_z_lo
    lda target_z_hi
    adc #0
    sta target_coll_upper_z_hi

    lda target_z_lo
    clc
    adc #3
    sta target_coll_lower_z_lo
    lda target_z_hi
    adc #0
    sta target_coll_lower_z_hi

    rts
; end sub target_set_collbox
} ; !zone

; **************************************************

!zone {
target_rotate
    lda #<target_x_lo
    sta MATHS0
    lda #>target_x_lo
    sta MATHS1
    lda #<target_z_lo
    sta MATHS2
    lda #>target_z_lo
    sta MATHS3
    lda hole_current_rotation_angle     
    sta MATHS4
    clc 
    jsr maths_s_rotate
    jsr target_set_collbox
    rts
; end sub target_rotate
} ; !zone

; **************************************************

; NOTE: this is called when ball and target swap h/w sprite numbers.
; FIXME: ball is hi-res, target is multicolor sprite!
!zone {
target_s_refresh_draw
;    lda target_hw_spr_num
;    cmp #target_c_SW_SPR_NUM
;    beq .behind_ball

    ; Sprites 6&7 (ball & shadow) should be 0 (= hires); sprite 5 (target) 1.
;    lda SPMC
;    and #$1f
;    ora #$20
;    sta SPMC

;.behind_ball
    ldy #target_c_SW_SPR_NUM
    ldx target_hw_spr_num
    jsr spr_s_write_to_vic_ii
    rts
; end sub target_s_refresh_draw
} ; !zone

; **************************************************

; OUTPUTS:  MATHS2 = mask.
!zone {
.ONE_PAST_THE_END = MATHS1

target_s_check_if_behind_trunk
    ; Get target's x-position in m/c bitmap coordinates.  Use this as an index
    ; into the tree-trunks collision table to see if we're behind one.
    ; First subtract lhs-border and divide by 2.
    lda spr_v_x_lo+target_c_SW_SPR_NUM
    sec
    sbc #spr_c_VISIBLE_ALL_L 
    sta MATHS0
    lda spr_v_x_hi+target_c_SW_SPR_NUM
    sbc #0
    sta MATHS1
    lsr MATHS1
    ror MATHS0
    ; MATHS0 now holds index...
    ; We must calculate the 'one-past-the-end' index.
    ldx target_v_current_depth
    lda MATHS0
    clc
    adc target_l_WIDTHS,x
    sta .ONE_PAST_THE_END

    ; Use Y to count the bit positions for the 'result' byte (= MATHS2).  
    ; MATHS2 initially set to $ff - so everything is let through when
    ; bitwise AND.
    ldx MATHS0  ; flag sprite x
    ldy #$ff
    sty MATHS2
    ; NOTE: Y starts at 0.
    iny 
.loop
    lda ttrunks_v_depths_z_hi,x
    bmi .next
    ; There is a trunk here so check depth.
    lda ttrunks_v_depths_z_lo,x
    sec
    sbc target_z_lo
    lda ttrunks_v_depths_z_hi,x
    sbc target_z_hi
    +bge_s .next
    ; The flag will be behind the trunk in this column, so record this fact
    ; in our result byte.
    lda target_l_COLUMN_MASKS,y
    and MATHS2
    sta MATHS2
    
.next
    iny
    inx
    cpx .ONE_PAST_THE_END
    bne .loop

    rts
; end sub target_s_check_if_behind_trunk
} ; !zone

; **************************************************

; INPUTS:   MATHS2 = mask.
!zone {
.HEIGHT = MATHS0
.TRUNK_MASK = MATHS2

target_s_build_sprite
    +utils_m_clear_sprite_data play_l_BLANK_SPRITE_FLAG

    jsr target_s_align_mask_bytes

    ldx target_v_current_depth
    lda target_l_FABRIC_PATTERN_OFFSETS_END,x
    sta MATHS3
    lda target_l_FABRIC_PATTERN_OFFSETS,x
    pha
    lda target_l_FABRIC_PATTERN_DEST_OFFSETS,x
    tay
    pla
    tax

    ; Modify source address for foliage masks in-place so we can use same
    ; index (= X).  'PATTERNS' offset already in accumulator.
    sec
    sbc target_v_foliage_masks_offset
    sta MATHS6
    +sex MATHS7
    lda #<target_v_foliage_masks  
    sec
    sbc MATHS6
    sta .mod1+1
    sta .mod2+1
    lda #>target_v_foliage_masks  
    sbc MATHS7
    sta .mod1+2
    sta .mod2+2

    ; X = source index, Y = destination index.
-
    lda target_l_FABRIC_PATTERNS,x
    and .TRUNK_MASK
.mod1
    and $ffff,x
    sta play_l_BLANK_SPRITE_FLAG,y
    iny
    iny
    iny
    inx
    cpx MATHS3
    bne -

    ; Now just finish drawing the flagpole.  Source byte should be $c0 until
    ; the end of the sprite.
-
    lda #$80
    and .TRUNK_MASK
.mod2
    and $ffff,x
    ; Nothing else to do if flagpole is obscured by a tree trunk!
;    beq .end
;-
    sta play_l_BLANK_SPRITE_FLAG,y
    iny
    iny
    iny
    inx
    cpy #63
    bne -

.end
    rts
; end sub target_s_build_sprite
} ; !zone

; **************************************************

!zone {
target_s_prepare_hole_sprite
    lda #BLACK
    sta spr_v_color+target_c_SW_SPR_NUM

    +utils_m_clear_sprite_data play_l_BLANK_SPRITE_FLAG    

    ; NOTE: use bit pair '10' for the sprite's own individual color
    ; (when a multicolor sprite).
    lda #$a0
    sta play_l_BLANK_SPRITE_FLAG+60
;    ldx #target_c_SW_SPR_NUM

    rts
; end sub target_s_prepare_hole_sprite
} ; !zone

; **************************************************

!zone {
target_s_calc_overlapping_chars
    ; TODO: must also pick out (and store) remainder...
    ; Left-hand column:
    lda spr_v_x_lo+target_c_SW_SPR_NUM
    sec
    sbc #spr_c_VISIBLE_ALL_L 
    sta MATHS0
    lda spr_v_x_hi+target_c_SW_SPR_NUM
    sbc #0
    sta MATHS1
    ; Pick out and store remainder before division by 8.
    lda MATHS0
    and #$07
    sta target_v_overlap_offset_x   

    lsr MATHS1
    ror MATHS0
    lsr MATHS1
    ror MATHS0
    lsr MATHS1
    ror MATHS0
    ldx MATHS0
    stx target_v_overlap_char_l
    ; Right-hand column:
    ; For now, just assume it's +1.  This is maybe all we'll ever need to do.
    inx
    stx target_v_overlap_char_r

    ; Top row:
    lda spr_v_y+target_c_SW_SPR_NUM
    sec
    sbc #spr_c_VISIBLE_ALL_T 
    ; Record remainder from division by 8 (before doing the division!).
    pha
    and #$07
    sta target_v_overlap_offset_y   
    pla
    lsr
    lsr
    lsr
    sta target_v_overlap_char_t
    ; And then +3 to get the bottom row.
    clc
    adc #3
    sta target_v_overlap_char_b

    ; Already here we can also work out what offset we'll need into the 
    ; 'foliage_masks' table once it's finalized.
    ldx target_v_current_depth
    lda #21
    clc
    adc target_v_overlap_offset_y   
    sec
    sbc target_l_POLE_HEIGHTS,x
    sta target_v_foliage_masks_offset   

    rts
; end sub target_s_calc_overlapping_chars
} ; !zone

; **************************************************

; This routine will place 32 mask bytes into memory starting at 
; target_v_foliage_masks, correctly aligned and ready to be applied.
!zone {
target_s_align_mask_bytes
    ldy target_v_overlap_offset_x
    beq .no_shifts_required

    ; We're shifting from char 1 into char 0, 3->2, 5->4 and 7->6.
.outer_loop
    ldx #7
.inner_loop
    asl target_v_foliage_masks+8,x
    rol target_v_foliage_masks,x
    asl target_v_foliage_masks+24,x
    rol target_v_foliage_masks+16,x
    asl target_v_foliage_masks+40,x
    rol target_v_foliage_masks+32,x
    asl target_v_foliage_masks+56,x
    rol target_v_foliage_masks+48,x
    dex
    bpl .inner_loop
    dey
    bne .outer_loop

.no_shifts_required
    ; The bytes we want are in 8-byte blocks at offsets 0,16,32 and 48.
    ldx #7
-
    lda target_v_foliage_masks+16,x
    sta target_v_foliage_masks+8,x
    lda target_v_foliage_masks+32,x
    sta target_v_foliage_masks+16,x
    lda target_v_foliage_masks+48,x
    sta target_v_foliage_masks+24,x
    dex
    bpl -

    rts
; end sub target_s_align_mask_bytes
} ; !zone

; **************************************************

!zone {
target_s_draw_shadow
    ; Draw only if there's a medium or large flag.
    ldx target_v_current_depth  
    lda target_l_HAS_SHADOW,x 
    beq .end

    ; NOTE: the sprite's stored position is relative to its base (i.e. the
    ; base of the flag pole).

    ; x = (sprx - VISIBLE_ALL_L) / 2
    lda spr_v_x_lo+target_c_SW_SPR_NUM
    sec
    sbc #spr_c_VISIBLE_ALL_L 
    lsr
    tax
    dex
    stx LINE_X0_LO
    dex
    stx LINE_X1_LO

    ; y = spry - 30
    ; Just seems to work!
    lda spr_v_y+target_c_SW_SPR_NUM
    sec
    sbc #30
    tax
    inx
    stx LINE_Y0_LO
    stx LINE_Y1_LO

    ; NOTE: even though we're not going to use them, set destination for
    ; 'edges', otherwise they'll overwrite some random area of memory.
    lda #<quads_edges_from
    sta EDGES_LO
    lda #>quads_edges_from
    sta EDGES_HI
    lda #0
    ldy #0
    jsr dp_s_draw_line

.end
    rts
; end sub target_s_draw_shadow
} ; !zone

; **************************************************

!zone {
target_s_update_hole_color
    lda ball_v_current_speed
    cmp #ball_c_MAX_PUTTING_SPEED+1
    bcs .too_fast
    lda #BLACK
    +skip_2_bytes 
.too_fast
    lda #RED

    ; When putting, h/w sprite number is same as s/w sprite number, with
    ; no chance of changing.  So we can cheat here and just set the register
    ; directly.
    sta SP0COL+target_c_SW_SPR_NUM  

    rts
; end sub target_s_update_hole_color
} ; !zone

; **************************************************
; **************************************************
; **************************************************

target_c_SIZE = *-target_c_BEGIN

