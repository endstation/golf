; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


trees_c_BEGIN = *

; *****************
; *** CONSTANTS ***
; *****************
trees_c_NUM_PATTERNS = 10
trees_c_PATTERNS_BLOCK_SIZE = 512
;trees_c_MAX_TILE_CLASSES = 24
trees_c_MAX_TILES = 16

; Trees beyond this point won't be drawn.
trees_c_MAX_DISTANCE_LO = <(200*hole_c_PIXELS_PER_YARD)
trees_c_MAX_DISTANCE_HI = >(200*hole_c_PIXELS_PER_YARD)

trees_l_DIST_LO
    !byte   <(5*hole_c_PIXELS_PER_YARD)
    !byte   <(8*hole_c_PIXELS_PER_YARD)
    !byte   <(11*hole_c_PIXELS_PER_YARD)
    !byte   <(14*hole_c_PIXELS_PER_YARD)
    !byte   <(18*hole_c_PIXELS_PER_YARD)
    !byte   <(22*hole_c_PIXELS_PER_YARD)
    !byte   <(30*hole_c_PIXELS_PER_YARD)
    !byte   <(50*hole_c_PIXELS_PER_YARD)
    !byte   <(80*hole_c_PIXELS_PER_YARD)
    !byte   <(130*hole_c_PIXELS_PER_YARD)
trees_l_DIST_HI
    !byte   >(5*hole_c_PIXELS_PER_YARD)
    !byte   >(8*hole_c_PIXELS_PER_YARD)
    !byte   >(11*hole_c_PIXELS_PER_YARD)
    !byte   >(14*hole_c_PIXELS_PER_YARD)
    !byte   >(18*hole_c_PIXELS_PER_YARD)
    !byte   >(22*hole_c_PIXELS_PER_YARD)
    !byte   >(30*hole_c_PIXELS_PER_YARD)
    !byte   >(50*hole_c_PIXELS_PER_YARD)
    !byte   >(80*hole_c_PIXELS_PER_YARD)
    !byte   >(130*hole_c_PIXELS_PER_YARD)
trees_c_DIST_N = 10

trees_l_CMLO_ADDR_LO
    !for i,15 {
        !byte <(trees_v_collision_matrix_lo+(i-1)*40)
    } ; !for
trees_l_CMLO_ADDR_HI
    !for i,15 {
        !byte >(trees_v_collision_matrix_lo+(i-1)*40)
    } ; !for
trees_l_CMHI_ADDR_LO
    !for i,15 {
        !byte <(trees_v_collision_matrix_hi+(i-1)*40)
    } ; !for
trees_l_CMHI_ADDR_HI
    !for i,15 {
        !byte >(trees_v_collision_matrix_hi+(i-1)*40)
    } ; !for
trees_c_COLLISION_MATRIX_TOP_ROW    = 4
trees_c_COLLISION_MATRIX_BOTTOM_ROW = 18

trees_l_TILES_ADDR_LO
    !for i,trees_c_MAX_TILES {
        !byte <(trees_v_tiles+(i-1)*8)
    } ; !for
trees_l_TILES_ADDR_HI
    !for i,trees_c_MAX_TILES {
        !byte >(trees_v_tiles+(i-1)*8)
    } ; !for

trees_c_MAX_N = 80

trees_l_SHADOW_MASKS
    !byte   %11001100
    !byte   %00110011
    !byte   %11001100
    !byte   %00110011
    !byte   %11001100
    !byte   %00110011
    !byte   %11001100
    !byte   %11110011
    !byte   %11001100
    !byte   %00110011
    !byte   %11001100
    !byte   %00110011
    !byte   %11001100
    !byte   %00110011
    !byte   %11001100
    !byte   %00110011

trees_l_LO_RES_PIXEL_MASKS
    !byte   %11111100,%11110011,%00111111,%11001111
    !byte   %11111100,%11110011,%00111111,%11001111
    !byte   %11111100,%11110011,%00111111,%11001111
    !byte   %11111111,%11111111,%11111111,%11111111

trees_l_COLOR_PATTERNS
    !byte   %01101010
    !byte   %10011010
    !byte   %10100110
    !byte   %10101001
    !byte   %10101010,%10101010,%10101010,%10101010

; NOTE: this is depth of foliage.
trees_c_DEPTH_IN_FEET = 5*hole_c_PIXELS_PER_FOOT
trees_c_TRUNK_COLOR = RED


; *****************
; *** VARIABLES ***
; *****************
; NOTE: data loaded in from file will be written here!
trees_v_heights            !fill   trees_c_NUM_PATTERNS
trees_v_trunk_heights      !fill   trees_c_NUM_PATTERNS
trees_v_trunk_widths       !fill   trees_c_NUM_PATTERNS
; Offset in pixels (hi-res?!) from char column.
trees_v_trunk_adjusts      !fill   trees_c_NUM_PATTERNS
trees_v_foliage_offsets    !fill   trees_c_NUM_PATTERNS
trees_v_patterns_addr_lo   !fill   trees_c_NUM_PATTERNS
trees_v_patterns_addr_hi   !fill   trees_c_NUM_PATTERNS
trees_v_foliage_modulation_colors  !fill   4
; NOTE: if the number of 01 bit pairs in the given cell is >= this threshold,
; the cell's color will NOT be modulated.
trees_v_modulation_threshold   !byte   0
trees_v_collision_cut_off      !byte   0

trees_v_patterns           !fill   trees_c_PATTERNS_BLOCK_SIZE
trees_v_tiles              !fill   trees_c_MAX_TILES * 8
; And here is the end of the data loaded in by file...

trees_v_depth_in_feet      !byte   5*hole_c_PIXELS_PER_FOOT
;trees_v_trunk_color        !byte   RED
;trees_v_trunk_color_shifted_up !byte   RED<<4

trees_v_n          !byte   0
trees_v_x_lo       !fill   trees_c_MAX_N
trees_v_x_hi       !fill   trees_c_MAX_N
trees_v_z_lo       !fill   trees_c_MAX_N
trees_v_z_hi       !fill   trees_c_MAX_N
trees_v_rot_x_lo   !fill   trees_c_MAX_N
trees_v_rot_x_hi   !fill   trees_c_MAX_N
trees_v_rot_z_lo   !fill   trees_c_MAX_N
trees_v_rot_z_hi   !fill   trees_c_MAX_N

trees_v_drawing_order  !fill   trees_c_MAX_N

; Matrix from rows 4 to 18.
trees_v_collision_matrix_lo    !fill   15*40
trees_v_collision_matrix_hi    !fill   15*40

trees_v_iter   !byte   0
; While checking depth, store it here for current tree.  Then if we
; need to write to the collision matrix, this is the value we need.
trees_v_temp_z_lo  !byte   0
trees_v_temp_z_hi  !byte   0

; Some variables to hold intermediate results for the tree we're 
; currently drawing.
trees_v_proj_x         !byte   0
trees_v_proj_y         !byte   0
trees_v_depth_index    !byte   0
; Remainder after dividing by 8.
trees_v_extra_height   !byte   0
trees_v_row0           !byte   0
trees_v_col0           !byte   0
; NOTE: these are 'video-matrix' coordinates.
trees_v_trunk_row0     !byte   0
trees_v_trunk_col0     !byte   0
trees_v_temp_in_front_of_target    !byte   0

; Used for determining which color code to use to draw tree trunks.
trees_v_color_code_totals  !fill   4

trees_v_current_mask   !fill   8


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   P0-P1 = address of first byte of trees data.
!zone {
trees_s_load
    ldy #0
    lda (P0),y
    sta trees_v_n
    pha

    ; Make sure the pointer is updated before possibly exiting routine.
    lda P0
    clc
    adc #1
    sta P0
    lda P1
    adc #0
    sta P1

    pla
    beq .end

    ; X keeps track of tree number/index.
    ldx #0

.loop   
    lda (P0),y
    sta trees_v_x_lo,x
    iny
    lda (P0),y
    sta trees_v_x_hi,x
    iny
    lda (P0),y
    sta trees_v_z_lo,x
    iny
    lda (P0),y
    sta trees_v_z_hi,x
;    iny
;    lda (P0),y
;    sta trees_terrain,x

    ldy #0
    +utils_m_advance_zp_iter P0,4

    inx
    cpx trees_v_n
    bne .loop
    
    ; This only needs to be called once per hole.
    jsr trees_s_init_drawing_order

.end
    rts
; end sub trees_s_load
} ; !zone

; **************************************************

!zone {
trees_s_init_drawing_order
    ldx #0
-   txa
    sta trees_v_drawing_order,x
    inx
    cpx #trees_c_MAX_N
    bne -
    rts
; end sub trees_s_init_drawing_order
} ; !zone

; **************************************************

; NOTE: store axis-aligned coords in the 'rot' variables.  We don't need to
; store aa coords for collisions.
!zone {
trees_s_set_aa_coords
    ldx #0

.loop
    lda trees_v_x_lo,x
    sec
    sbc ball_world_x_lo
    sta trees_v_rot_x_lo,x
    lda trees_v_x_hi,x
    sbc ball_world_x_hi
    sta trees_v_rot_x_hi,x

    lda trees_v_z_lo,x
    sec
    sbc ball_world_z_lo
    sta trees_v_rot_z_lo,x
    lda trees_v_z_hi,x
    sbc ball_world_z_hi
    sta trees_v_rot_z_hi,x

    inx
    cpx trees_v_n
    bne .loop
    
    rts
; end sub trees_s_set_aa_coords
} ; !zone

; **************************************************

; NOTE: must call trees_set_aa_coords before calling this!
!zone {
.iter   !byte   0
.buffer !fill   4
.X_LO = 0
.X_HI = 1
.Z_LO = 2
.Z_HI = 3

trees_s_rotate
    ldx #0
    stx .iter

.loop
    ; Fill up buffer and use these variables for maths routines.
    lda trees_v_rot_x_lo,x
    sta .buffer+.X_LO
    lda trees_v_rot_x_hi,x
    sta .buffer+.X_HI
    lda trees_v_rot_z_lo,x
    sta .buffer+.Z_LO
    lda trees_v_rot_z_hi,x
    sta .buffer+.Z_HI

    lda #<(.buffer+.X_LO)
    sta MATHS0
    lda #>(.buffer+.X_LO)
    sta MATHS1
    lda #<(.buffer+.Z_LO)
    sta MATHS2
    lda #>(.buffer+.Z_LO)
    sta MATHS3
    clc
    jsr maths_s_rotate

    ; Write the (transformed) buffer values back into the array.
    ldx .iter
    lda .buffer+.X_LO
    sta trees_v_rot_x_lo,x
    lda .buffer+.X_HI
    sta trees_v_rot_x_hi,x
    lda .buffer+.Z_LO
    sta trees_v_rot_z_lo,x
    lda .buffer+.Z_HI
    sta trees_v_rot_z_hi,x

    inx
    stx .iter
    cpx trees_v_n
    bne .loop

    rts
; end sub trees_s_rotate
} ; !zone

; **************************************************

!zone {
; This is the index into trees_drawing_order.
.list_iter  !byte   0

trees_s_draw
    jsr trees_s_sort
    jsr trees_s_clear_collision_matrix
    jsr ttrunks_s_reset

    ldx #0
    stx .list_iter
    lda trees_v_drawing_order,x
    sta trees_v_iter
    tax

.loop
    ; We can quickly reject any trees which are either behind the camera or
    ; too far away.
    lda trees_v_rot_z_lo,x
    sta trees_v_temp_z_lo 
    sec
    sbc #trees_c_MAX_DISTANCE_LO
    lda trees_v_rot_z_hi,x
    sta trees_v_temp_z_hi 
    bmi .next
    sbc #trees_c_MAX_DISTANCE_HI
    +bge_s .next

    jsr trees_s_project
    bcs .next

    jsr trees_s_select_depth_index
    jsr trees_s_setup_draw
    jsr trees_s_draw_foliage

    ldx trees_v_iter
    jsr trees_s_draw_trunk

.next
    inc .list_iter
    ldx .list_iter
    cpx trees_v_n
    beq .end
    lda trees_v_drawing_order,x
    sta trees_v_iter
    tax
    jmp .loop

.end
    rts
; end sub trees_s_draw
} ; !zone

; **************************************************

!zone {
trees_s_sort
    lda #<trees_v_drawing_order
    sta P0
    lda #>trees_v_drawing_order
    sta P1
    lda trees_v_n
    sta P2
    ; Write address of 'compare' routine into bubble sort code.
    lda #<trees_s_compare
    sta utils_s_comp+1
    lda #>trees_s_compare
    sta utils_s_comp+2

    jsr utils_s_bubble_sort

    rts
; end sub trees_s_sort
} ; !zone

; **************************************************

; INPUTS:   P3 = index #1, P4 = index #2
; OUTPUTS:  C flag set if should be swapped, otherwise clear
!zone {
trees_s_compare
    ldx P3
    ldy P4

    ; Do the calculation A-B.  If A<B, items should be swapped.
    lda trees_v_rot_z_lo,x
    sec
    sbc trees_v_rot_z_lo,y
    lda trees_v_rot_z_hi,x
    sbc trees_v_rot_z_hi,y
    +blt_s .swap

    ; No swap.
    clc
    rts ; EXIT POINT.

.swap
    sec
    rts
; end sub trees_s_compare
} ; !zone

; **************************************************

!zone {
trees_s_clear_collision_matrix
    ldx #200
    lda #0
-
    sta trees_v_collision_matrix_lo-1,x
    sta trees_v_collision_matrix_lo+5*40-1,x
    sta trees_v_collision_matrix_lo+10*40-1,x
    sta trees_v_collision_matrix_hi-1,x
    sta trees_v_collision_matrix_hi+5*40-1,x
    sta trees_v_collision_matrix_hi+10*40-1,x
    dex
    bne -

    rts
; end sub trees_s_clear_collision_matrix
} ; !zone

; **************************************************

; INPUTS:   trees_v_iter = the tree we're dealing with
; OUTPUTS:  C clear if onscreen; C set if not visible
!zone {
trees_s_project
    ldx trees_v_iter

    lda #0
    sta P0
    sta P1
    lda trees_v_rot_z_lo,x
    sta P2
    lda trees_v_rot_z_hi,x
    sta P3
    jsr camera_project_onto_plate_y
    bcs .end
    ; TODO: store result somewhere!
    lda P0
    sta trees_v_proj_y

    ldx trees_v_iter
    lda trees_v_rot_x_lo,x
    sta CAMERA0
    lda trees_v_rot_x_hi,x
    sta CAMERA1
    lda trees_v_rot_z_lo,x
    sta CAMERA2
    lda trees_v_rot_z_hi,x
    sta CAMERA3
    jsr camera_project_onto_plate_x

    ; Check if result is in bounds.
    ; Should be positive.
    lda CAMERA1
    bmi .out_of_bounds
    ; Divide by 2 (cf. m/c bitmap).
    lsr CAMERA1
    ror CAMERA0
    ; High byte should now be 0.
    lda CAMERA1
    bne .out_of_bounds
    ; And low byte < 160.
    lda CAMERA0
    cmp #160
    bcs .out_of_bounds
    sta trees_v_proj_x
    ; OK.
    clc
    rts ; EXIT POINT.

.out_of_bounds
    sec

.end
    rts
; end sub trees_s_project
} ; !zone

; **************************************************

!zone {
trees_s_select_depth_index
    ldx trees_v_iter
    ; Use Y as index into DIST table.
    ldy #0  

.loop
    lda trees_v_rot_z_lo,x
    sec
    sbc trees_l_DIST_LO,y
    lda trees_v_rot_z_hi,x
    sbc trees_l_DIST_HI,y
    +blt_s .found

    cpy #trees_c_DIST_N-1
    beq .found
    iny
    jmp .loop

.found
    sty trees_v_depth_index
    ; If this is smallest size (i=9), we'll occasionally make it one size
    ; bigger.  This will break up a distant b/g where all trees are same size.
    cpy #trees_c_NUM_PATTERNS-1
    bne .end
    jsr rand_s_get_fast
    cmp #64
    bcs .end
    dec trees_v_depth_index
.end
    rts
; end sub trees_s_select_depth_index
} ; !zone

; **************************************************

!zone {
trees_s_setup_draw
    ; NOTE: this sometimes allows certain trees to gain an extra 7 pixels
    ; in height, whereas others at the same depth get nothing.  No trivial 
    ; fix (that I can find) so keeping it as it is for now...
    lda trees_v_proj_y
    and #$07
    sta trees_v_extra_height

    ; Divide Y-position by 8 to find out what row this is.  Then subtract
    ; the height (measured in rows) to find the top row of the tree.
    lda trees_v_proj_y
    lsr
    lsr
    lsr
    ; We'll do something else with this value shortly.
    pha
    ldx trees_v_depth_index
    sec
    sbc trees_v_heights,x
    sta trees_v_row0
    ; Let's record where the trunk starts...
    pla
    sec
    sbc trees_v_trunk_heights,x
    sta trees_v_trunk_row0

    ; NOTE: 4 pixels per byte!  So divide X-position by 4 to get the column.
    ; Then subtract half the width to get the first column.
    lda trees_v_proj_x
    lsr
    lsr
    sta trees_v_trunk_col0
    sec
    sbc trees_v_foliage_offsets,x
    sta trees_v_col0

    rts
; end sub trees_s_setup_draw
} ; !zone

; **************************************************

!zone {
trees_s_draw_foliage
    ; Record whether the current tree is behind or in front of the target.
    +clr trees_v_temp_in_front_of_target 
    lda trees_v_temp_z_lo
    cmp target_z_lo
    lda trees_v_temp_z_hi
    sbc target_z_hi
    +bge_s +
    inc trees_v_temp_in_front_of_target 
+

    ldx trees_v_depth_index
    ; Pattern matrix into P4-P5.
    lda trees_v_patterns_addr_lo,x
    sta P4
    lda trees_v_patterns_addr_hi,x
    sta P5
    lda trees_v_row0
    sta P0
    lda trees_v_col0
    sta P1

    lda #<trees_s_draw_tile
    sta ingm_mod0 
    lda #>trees_s_draw_tile
    sta ingm_mod0+1

    jsr ingm_s_draw_tile_pattern
    rts
; end sub trees_s_draw_foliage
} ; !zone

; **************************************************

; NOTE: write this custom routine directly into ingm_s_draw_tile_pattern.
; INPUTS:   X = tile ID, BITMAP_LO/HI = destination,
;           FADE_CR_LO = column, FADE_CR_HI = row.
!zone {
.SELECTED_COLOR = POWARC_LO
.DEST_COL       = FADE_CR_LO
.DEST_ROW       = FADE_CR_HI
.TILE_ID        = FADE_CR_SRC_LO    ;TXTREN_CURRENT_CHAR_LO
.TEMP_INDEX     = FADE_CR_SRC_HI    ;TXTREN_CURRENT_CHAR_HI
.tile_copy      !fill   8

trees_s_draw_tile
    stx .TILE_ID

    lda trees_l_TILES_ADDR_LO,x
    sta TREES_LO
    lda trees_l_TILES_ADDR_HI,x
    sta TREES_HI

    ; Make local copy of tile data.
    ldy #7
-
    lda (TREES_LO),y
    sta .tile_copy,y
    dey
    bpl -
    
    ; Iterate over the tile data bytes and decide whether we are going to 
    ; 'texture' it...
    ldx #7
-
    lda .tile_copy,x
    bne +
    lda #$ff
    bne .set_mask
+
    jsr rand_s_get_fast
    and #$07
    sta .TEMP_INDEX
    jsr rand_s_get_fast
    and #$0f
    tay
    lda trees_l_LO_RES_PIXEL_MASKS,y
    and .tile_copy,x
    ldy .TEMP_INDEX
    and trees_l_COLOR_PATTERNS,y    
    sta .tile_copy,x

.next_byte
    ; Build mask for this byte.
    lda .tile_copy,x
    sta P6
    jsr dp_s_build_mask
    lda P7
.set_mask
    sta trees_v_current_mask,x

    dex
    bpl -

    jsr trees_s_write_foliage_masks

    ; Copy 8 bytes from source->destination.
    ; As we're copying, for each byte, count how many 01 bit pairs there
    ; are.  We'll use this to determine whether or not to modulate the
    ; color for this cell.  Only need to clear the bit pair total we're
    ; interested in (= 01).
    +utils_m_kernal_out
    ldy #0
    sty trees_v_color_code_totals+1
-
    lda (BITMAP_LO),y
    and trees_v_current_mask,y
    ora .tile_copy,y
    sta (BITMAP_LO),y
    ; Don't call 'count' routine unless need to!
    ldx .TILE_ID
    bne +
    jsr trees_s_count_color_codes
+
    iny
    cpy #8
    bne -
    +utils_m_kernal_in

    ; If this is the 'full' tile (ID=0), we may modulate the color.
    lda .TILE_ID
    bne .coll_matrix

    ; Modulate color?
    ldy .DEST_ROW
    lda trees_v_color_code_totals+1
    cmp trees_v_modulation_threshold
    bcs .coll_matrix

    jsr rand_s_get_fast
    and #$03
    tax
    lda trees_v_foliage_modulation_colors,x
    sta .SELECTED_COLOR

    lda dp_l_VIDEO_RAM_ROWS_LO,y
    clc
    adc .DEST_COL
    sta COLORS_LO
    lda dp_l_VIDEO_RAM_ROWS_HI,y
    adc #0
    sta COLORS_HI
    ldy #0
    lda (COLORS_LO),y
    and #$0f
    ora .SELECTED_COLOR
    sta (COLORS_LO),y

.coll_matrix
    lda .TILE_ID
    cmp trees_v_collision_cut_off
    bcs .end
    lda .DEST_ROW
    sta MATHS0
    lda .DEST_COL
    sta MATHS1
    jsr trees_s_write_collision_matrix

.end
    rts
; end sub trees_s_draw_tile
} ; !zone

; **************************************************

; INPUTS:   FADE_CR_LO = column, FADE_CR_HI = row,
;           VM_LO/HI = pointer to mask data
!zone {
trees_s_write_foliage_masks
    lda trees_v_temp_in_front_of_target 
    +branch_if_false .end
    lda FADE_CR_HI
    sec
    sbc target_v_overlap_char_b 
    clc
    adc #3
    bmi .end
    cmp #4
    bcs .end
    ; We have row offset - multiply by 16 to get index.
    asl
    asl
    asl
    asl
    sta CURSOR_POS_LO
    lda FADE_CR_LO
    sec
    sbc target_v_overlap_char_r
    clc
    adc #1
    bmi .end
    cmp #2
    bcs .end
    asl
    asl
    asl
    clc
    adc CURSOR_POS_LO
    tax
    ; Valid index now in X.
    ldy #0
-
;    lda (VM_LO),y
    lda trees_v_current_mask,y
    and target_v_foliage_masks,x
    sta target_v_foliage_masks,x
    inx
    iny
    cpy #8
    bne -

.end
    rts
; end sub trees_s_write_foliage_masks
} ; !zone

; **************************************************

; INPUTS:   MATHS0 = row, MATHS1 = column
; NOTE: the depth of the current tree is stored in trees_v_temp_z_lo/hi. 
; NOTE: we must preserve the value of X.
!zone {
trees_s_write_collision_matrix
    ; First make sure that the row is in range.
    lda MATHS0
    cmp #trees_c_COLLISION_MATRIX_TOP_ROW
    bcc .end
    cmp #trees_c_COLLISION_MATRIX_BOTTOM_ROW+1
    bcs .end

    ; Put the address of low byte (of collision matrix) into P2-P3,
    ; and high byte into P4-P5.
    ; NOTE: this is the row address at 0th column.  Offset with value 
    ; stored in MATHS1 (= the column).
    ; NOTE: remember to subtract 4 from the row!!!
    lda MATHS0
    sec
    sbc #trees_c_COLLISION_MATRIX_TOP_ROW
    tay
    lda trees_l_CMLO_ADDR_LO,y
    sta P2
    lda trees_l_CMLO_ADDR_HI,y
    sta P3
    lda trees_l_CMHI_ADDR_LO,y
    sta P6
    lda trees_l_CMHI_ADDR_HI,y
    sta P7

    ; Now let's see if the current depth (i.e. z-position) is less than the 
    ; one stored.  If yes, we'll overwrite it; as we will also do if the
    ; matrix contains 0 (meaning it's empty).
    ldy MATHS1
    lda (P2),y
    ora (P6),y
    beq .write

    lda trees_v_temp_z_lo
    sec
    sbc (P2),y
    lda trees_v_temp_z_hi
    sbc (P6),y
    +bge_s .end

.write
    ; BUG (fixed): sometimes tree will be at z=0, but if this is written into 
    ; the matrix it'll be interpreted as no foliage present.
    ; Quick hack-y fix - always add 1 to the z-position to avoid this error...
    lda trees_v_temp_z_lo
    clc
    adc #1
    sta (P2),y
    lda trees_v_temp_z_hi
    adc #0
    sta (P6),y

.end
    rts
; end sub trees_s_write_collision_matrix
} ; !zone

; **************************************************

; INPUTS:   X = tree index.
!zone {
.row_iter   !byte   0
.row_end    !byte   0
.char_row_count !byte   0
.trunk_width    !byte   0
.start_x    !byte   0
.end_x      !byte   0
.x_iter     !byte   0
.tree_index !byte   0

trees_s_draw_trunk
    stx .tree_index

    ldx trees_v_depth_index
    lda trees_v_trunk_heights,x
    clc
    adc trees_v_trunk_row0
    sta .row_end
    lda trees_v_trunk_row0
    sta .row_iter
    lda trees_v_trunk_widths,x
    sta .trunk_width
    ; Start and end columns (hi-res) for trunk.
    lda trees_v_proj_x
    and #$fc
    ora trees_v_trunk_adjusts,x
    sta .start_x
    clc
    adc .trunk_width
    sta .end_x

    lda trees_v_depth_index
    cmp #6
    bcs .no_shadow
    lda .row_end
    sta P0
    lda trees_v_trunk_col0
    sta P1
    jsr trees_s_draw_shadow

.no_shadow
    ; Now draw the actual pixels.  The .row_iter and .row_end variables will
    ; now be used to hold bitmap-resolution rows.
    lda trees_v_trunk_row0
    asl
    asl
    asl
    sta .row_iter
    lda .row_end
    asl
    asl
    asl
    clc
    adc trees_v_extra_height
    sta .row_end

    ; TODO: record tree trunk depth and position now for this tree!
    ldx .tree_index
    ldy .start_x
-
    lda trees_v_rot_z_lo,x  
    sta ttrunks_v_depths_z_lo,y   
    lda trees_v_rot_z_hi,x  
    sta ttrunks_v_depths_z_hi,y   
    lda .row_iter
    sta ttrunks_v_y0,y            
    iny
    cpy .end_x
    bne -

    ; Loop starts here:
    lda .start_x
    sta .x_iter

-
    ldy .row_iter
    ldx .x_iter

    jsr trees_s_determine_color_code_for_trunk

    ; NOTE: if drawing into foreground, tree terrain must be respected - so
    ; color code is whatever's in MATHS6.  If drawing into the b/g, always use
    ; color code 3 to avoid clashes with the b/g line of trees.  (This is taken
    ; care of by call to trees_s_determine_color_code_for_trunk.)
    lda MATHS6
    jsr dp_s_draw_pixel

    inc .x_iter
    lda .x_iter
    cmp .end_x
    bne -

    inc .row_iter
    ldy .row_iter
    cpy .row_end
    beq .end
    ; Reset the pixel's x-coordinate.
    lda .start_x
    sta .x_iter
    jmp -
    
.end
    rts
; end sub trees_s_draw_trunk
} ; !zone

; **************************************************

; INPUTS:   P0 = bitmap row, P1 = char col + 1
!zone {
.MASK_BYTE = EDGES_LO

trees_s_draw_shadow
    ldx P1
    ; Don't bother if trunk is in column #0.
    beq .end
    dex
    txa
    asl
    asl
    tax
    lda P0
    asl
    asl
    asl
    tay

    ; Bitmap destination address into BITMAP_LO/HI.
    lda dp_l_BITMAP_ROWS_LO,y
    clc
    adc dp_l_BITMAP_COLS_LO,x
    sta BITMAP_LO
    lda dp_l_BITMAP_ROWS_HI,y
    adc dp_l_BITMAP_COLS_HI,x
    sta BITMAP_HI

    +utils_m_kernal_out 
    ldy #0
    ldx #0
-
    lda (BITMAP_LO),y
    and trees_l_SHADOW_MASKS,x
    sta (BITMAP_LO),y
    inx
    iny
    cpy #16
    bne -
    +utils_m_kernal_in

.end
    rts
; end sub trees_s_draw_shadow
} ; !zone

; **************************************************

; INPUTS:   Y = row (bitmap), trees_trunk_col0 (cf. 40*25 matrix)
; OUTPUTS:  MATHS6 = color code
!zone {
.BITMAP_ROW = PARTSYS_LO
.MATRIX_ROW = PARTSYS_HI

trees_s_determine_color_code_for_trunk
    ; Only if we're entering a new cell (where bitmap row is a multiple of
    ; 8) do we need to do anything...
    tya
    and #$07
    beq .work_to_do
    rts ; EXIT POINT.

.work_to_do
    +utils_m_save_xy_to_stack 

    ; Need matrix row (so divide bitmap row by 8).
    sty .BITMAP_ROW
    tya
    lsr
    lsr
    lsr
    sta .MATRIX_ROW
    cmp #16
    bcs +
    tay
    jsr trees_s_prepare_for_trunk_above_horizon
    jmp .clean_up
+

    ; Let's check if either color RAM or video RAM already set to trunk color.
    ; If they are, we'll go with that choice.
    ; Put color RAM address into P0-P1... 
    ; NOTE: matrix row is still in the accumulator.
    tay
    lda dp_l_COLOR_RAM_ROWS_LO,y
    clc
    adc trees_v_trunk_col0
    sta P0
    lda dp_l_COLOR_RAM_ROWS_HI,y
    adc #0
    sta P1
    ; ... and video RAM into P2-P3.
    lda dp_l_VIDEO_RAM_ROWS_LO,y
    clc
    adc trees_v_trunk_col0
    sta P2
    lda dp_l_VIDEO_RAM_ROWS_HI,y
    adc #0
    sta P3

    ldy #0
    lda (P0),y
    and #$0f
    cmp #trees_c_TRUNK_COLOR
    beq .use_code_3
    lda (P2),y
    and #$f0
    cmp #(trees_c_TRUNK_COLOR<<4)
    bne .analyze_bitmap

    ; So use code 1.
    lda #1
    +skip_2_bytes 
.use_code_3
    lda #3
    sta MATHS6
    jmp .clean_up

.analyze_bitmap
    ; Clear totals - we're only interested in slots 1 and 3.
    lda #0
    sta trees_v_color_code_totals+1   
    sta trees_v_color_code_totals+3   
    ; Base address of bitmap into PATTERN_LO-PATTERN_HI.
    ; Column must be multiplied by 4 for bitmap coordinates.
    lda trees_v_trunk_col0 
    asl
    asl
    tax
    ldy .BITMAP_ROW
    lda dp_l_BITMAP_ROWS_LO,y
    clc
    adc dp_l_BITMAP_COLS_LO,x
    sta PATTERN_LO
    lda dp_l_BITMAP_ROWS_HI,y
    adc dp_l_BITMAP_COLS_HI,x
    sta PATTERN_HI
    
    +utils_m_kernal_out
    ldy #7
-
    lda (PATTERN_LO),y
    jsr trees_s_count_color_codes
    dey
    bpl -
    +utils_m_kernal_in

    lda trees_v_color_code_totals+3
    ; If zero, there's no water here so use code 11.
    beq .in_front_of_sand
    lda trees_v_color_code_totals+1
    ; If zero, no sand here so use code 01. 
    beq .in_front_of_water

    nop
    nop
    nop

    ; If program gets to here, there is both water and sand in this cell.
    ; Sand (01) is already in the accumulator.
    cmp trees_v_color_code_totals+3
    ; State of C flag affects behaviour of 'fix_color_clash'.  Preserve its
    ; value here so we know which color code to choose.
    php
;    inc EXTCOL
    jsr trees_s_fix_color_clash
    plp
    bcs .in_front_of_sand

;    cmp trees_v_color_code_totals+3
;    beq .default
;    bcs .in_front_of_sand
.in_front_of_water
    lda #1
    sta MATHS6
    ; NOTE: video RAM address is in P2-P3.
    ldy #0
    lda (P2),y
    and #$0f
    ora #(trees_c_TRUNK_COLOR<<4)
    sta (P2),y
    bne .clean_up
.in_front_of_sand
    lda #3
    sta MATHS6
    ; NOTE: color RAM address is in P0-P1.
    ldy #0
    lda #trees_c_TRUNK_COLOR
    sta (P0),y

.clean_up
    +utils_m_restore_xy_from_stack 
    rts
; end sub trees_s_determine_color_code_for_trunk
} ; !zone

; **************************************************

; Convenience routine - current cell needs to be prepared for trunk drawing.
; INPUTS:   Y = row (matrix)
!zone {
trees_s_prepare_for_trunk_above_horizon
    ; Put color RAM address into P0-P1.
    lda dp_l_COLOR_RAM_ROWS_LO,y
    clc
    adc trees_v_trunk_col0
    sta P0
    lda dp_l_COLOR_RAM_ROWS_HI,y
    adc #0
    sta P1

    ldy #0
;    lda trees_v_trunk_color
    lda #trees_c_TRUNK_COLOR
    sta (P0),y
    ; And color code to be used goes in MATHS6.
    lda #3
    sta MATHS6

    rts
; end sub trees_s_prepare_for_trunk_above_horizon
} ; !zone

; **************************************************

; INPUTS:   A = bitmap byte.
; NOTE: preserves value of Y.
!zone {
.mybyte !byte   0

trees_s_count_color_codes
    sta .mybyte
    +utils_m_save_y_to_stack 

    ldy #4
    lda .mybyte

    pha
.again
    ; Isolate leftmost pixel and use as index into 'totals' table.
    and #$03
    tax
    inc trees_v_color_code_totals,x
    pla
    dey
    beq .done
    pha
    lsr
    lsr
    jmp .again

.done
    +utils_m_restore_y_from_stack 
    rts
; end sub trees_s_count_color_codes
} ; !zone

; **************************************************

; INPUTS:   PATTERN_LO/HI = bitmap address;
;           C flag set = turn water ORANGE, else sand ORANGE.
!zone {
; Four pixel positions, starting from left (l.s. bit).
.MASKS_WATER    !byte   %00000011,%00001100,%00110000,%11000000
.SAND           !byte   %00000001,%00000100,%00010000,%01000000
.MY_BYTE = VM_LO
.OPCODE_CMP_ABS_X = $dd

trees_s_fix_color_clash
    ; First set up which tables will be used where.
    ; WARNING: self-modifying code!
    bcs .water_to_orange

    ; So sand to orange (00).
    lda #<.SAND
    sta .mod1
    lda #>.SAND
    sta .mod1+1
    bne .start
.water_to_orange
    lda #<.MASKS_WATER
    sta .mod1
    lda #>.MASKS_WATER
    sta .mod1+1

.start
    +utils_m_kernal_out
    ; Y keeps track of byte (0-7).
    ldy #7

.loop
    ; X keeps track of bit pairs in each byte.
    ldx #0
    stx .MY_BYTE

.loop_inner
    lda (PATTERN_LO),y
    and .MASKS_WATER,x

    ; cmp <'from' table>,x
    !byte .OPCODE_CMP_ABS_X
.mod1 !byte 0,0

    beq +
    ; Not the bit-pair we're replacing (with background color), so just
    ; write it back into the byte.
    ora .MY_BYTE
    sta .MY_BYTE
+
    inx
    cpx #4
    bne .loop_inner

    ; Write this new fixed byte to the bitmap.
    lda .MY_BYTE
    sta (PATTERN_LO),y

    dey
    bpl .loop

    +utils_m_kernal_in
    rts
; end sub trees_s_fix_color_clash
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

trees_c_SIZE = *-trees_c_BEGIN

