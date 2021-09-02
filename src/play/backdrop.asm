; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


bdrop_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
bdrop_c_MAX_TILES = 48
bdrop_c_MAX_DISTANT_OBJECTS = 5
bdrop_c_PATTERN_BLOCK_SIZE = 288
bdrop_c_MAX_VISIBLE_DISTANT_OBJECTS = 3

; NOTE: assume object is 1000yds away.
; Rotate vector by PI/32 radians each time from 12 o'clock (cw and ccw).  This 
; table represents where the point is projected onto the x-axis.
bdrop_l_POSITIONS_X
    !byte   (-2),0,3,6,8,10,12,14,15,16,17,18
    !byte   20
    !byte   21,22,23,24,25,27,29,31,33,36,38,39
bdrop_l_SPRITE_POSITIONS_X_LO
    !byte   <8,<24,<48,<72,<88,<104,<120,<136,<144,<152,<160,<168
    !byte   <184
    !byte   <192,<200,<208,<216,<224,<240,<256,<272,<288,<312,<328,<336
bdrop_l_SPRITE_POSITIONS_X_HI
    !byte   >8,>24,>48,>72,>88,>104,>120,>136,>144,>152,>160,>168
    !byte   >184
    !byte   >192,>200,>208,>216,>224,>240,>256,>272,>288,>312,>328,>336

; Lookup table for tiles.
bdrop_l_TILES_ADDR_LO
    !for i,bdrop_c_MAX_TILES {
        !byte <bdrop_v_tiles+((i-1)*8)
    } ; !for
bdrop_l_TILES_ADDR_HI
    !for i,bdrop_c_MAX_TILES {
        !byte >bdrop_v_tiles+((i-1)*8)
    } ; !for

bdrop_l_AVAILABLE_HW_SPRITES    !byte   0,4,3
bdrop_l_AVAILABLE_SW_SPRITES    !byte   11,12,13

bdrop_c_ANIM_LOOP       = 0
bdrop_c_ANIM_PINGPONG   = 1


; *****************
; *** VARIABLES ***
; *****************
bdrop_v_data_filename  !pet    "b00.prg",0

bdrop_v_num_distant_objects    !byte   0
; List terminated by (-1).  At which columns should we draw the repeating
; backdrop pattern?  Minimum width of that pattern is 8 (chars) - must be
; a multiple of 40.
bdrop_v_columns             !fill   ((40/8) + 1)
bdrop_v_pattern_start_rows  !fill   bdrop_c_MAX_DISTANT_OBJECTS+1
bdrop_v_tiles               !fill   bdrop_c_MAX_TILES*8
bdrop_v_repeating_pattern_addr_lo   !byte   0
bdrop_v_repeating_pattern_addr_hi   !byte   0
; These addresses for the distant object patterns...
bdrop_l_patterns_addr_lo    !fill   bdrop_c_MAX_DISTANT_OBJECTS
bdrop_l_patterns_addr_hi    !fill   bdrop_c_MAX_DISTANT_OBJECTS
bdrop_v_patterns            !fill   bdrop_c_PATTERN_BLOCK_SIZE
bdrop_v_angles              !fill   bdrop_c_MAX_DISTANT_OBJECTS             
; NOTE: which patterns are we to draw at the above angles?
; This does not include the repeating 'base' backdrop pattern, so indices will
; start at 1.
bdrop_v_pattern_indices     !fill   bdrop_c_MAX_DISTANT_OBJECTS

; Arrays holding information about any accompanying sprites...
; These will also be loaded in by file per course.
bdrop_v_spr_y           !fill   bdrop_c_MAX_DISTANT_OBJECTS 
bdrop_v_spr_x_offset_lo !fill   bdrop_c_MAX_DISTANT_OBJECTS 
bdrop_v_spr_x_offset_hi !fill   bdrop_c_MAX_DISTANT_OBJECTS 
bdrop_v_spr_ptr_from    !fill   bdrop_c_MAX_DISTANT_OBJECTS 
bdrop_v_spr_ptr_to      !fill   bdrop_c_MAX_DISTANT_OBJECTS 
; If this is set to zero, the sprite is treated as a static image:
bdrop_v_spr_framerate   !fill   bdrop_c_MAX_DISTANT_OBJECTS 
bdrop_v_spr_hires       !fill   bdrop_c_MAX_DISTANT_OBJECTS 
bdrop_v_spr_color       !fill   bdrop_c_MAX_DISTANT_OBJECTS
bdrop_v_spr_anim_type   !fill   bdrop_c_MAX_DISTANT_OBJECTS

; These arrays should be indexed by the distant object number, not the pattern
; number as above!
bdrop_v_spr_active !fill   bdrop_c_MAX_DISTANT_OBJECTS,0
bdrop_v_spr_sw_num !fill   bdrop_c_MAX_DISTANT_OBJECTS
bdrop_v_spr_hw_num !fill   bdrop_c_MAX_DISTANT_OBJECTS 

; NOTE: use this for when drawing accompanying sprite.  Position (index) of
; current distant object temporarily stored here.
bdrop_v_current_position    !byte   0

bdrop_v_golfer_angle    !byte   0

; Index into the available h/w and s/w sprite numbers:
bdrop_v_current_sprite_slot !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
.PETSCII_B = 66
.PETSCII_S = 83
.PETSCII_T = 84

bdrop_s_init
    ; Need to load in the data file.  Filename determined by course index.
    lda shared_v_course_index
    ; TODO: only one course at the moment!!!
    lda #1

    sta P0
    lda #0
    sta P1
    jsr utils_s_16bit_hex_to_dec
    ; Result in utils_v_dec_digits. We need to look at the first two bytes only.
    lda utils_v_dec_digits
    clc
    adc #SCR_CODE_0
    sta bdrop_v_data_filename+2
    lda utils_v_dec_digits+1
    clc
    adc #SCR_CODE_0
    sta bdrop_v_data_filename+1

    ; Load in the 'backdrop' file.
    lda #.PETSCII_B
    sta bdrop_v_data_filename
    ldx #<bdrop_v_data_filename
    ldy #>bdrop_v_data_filename
    jsr CB_LOADFILE_EXOMIZER
    ; And the sprite file...
    lda #.PETSCII_S
    sta bdrop_v_data_filename
    ldx #<bdrop_v_data_filename
    ldy #>bdrop_v_data_filename
    jsr CB_LOADFILE_EXOMIZER
    ; And the trees file... ???!!!
    lda #.PETSCII_T
    sta bdrop_v_data_filename
    ldx #<bdrop_v_data_filename
    ldy #>bdrop_v_data_filename
    jsr CB_LOADFILE_EXOMIZER

    +utils_m_turn_on_supercpu

    rts
; end sub bdrop_s_init
} ; !zone

; **************************************************

!zone {
.COLUMN_ITER = CURSOR_POS_LO

bdrop_s_draw_bg
    ldx #0
.loop
    stx .COLUMN_ITER

    lda bdrop_v_columns,x
    bmi .end
    sta P1
    lda bdrop_v_pattern_start_rows
    sta P0
    lda bdrop_v_repeating_pattern_addr_lo   
    sta P4
    lda bdrop_v_repeating_pattern_addr_hi   
    sta P5
    jsr ingm_s_draw_tile_pattern

    ldx .COLUMN_ITER
    inx
    bne .loop

.end
    rts
; end sub bdrop_s_draw_bg
} ; !zone

; **************************************************

; INPUT:    X = object index
; OUTPUT:   C flag clear if visible (and MATHS0 holds index); else set.
; NOTE: initial value of X is preserved.
!zone {
.distant_object_angle   !byte   0
.SIGNED_DIFF = MATHS0

bdrop_s_check_rotation
    ; NOTE: a negative value indicates that there is no distant object for
    ; this 'slot'.
    lda bdrop_v_angles,x
    bmi .not_visible
    sta .distant_object_angle

    lda bdrop_v_golfer_angle
    sec
    sbc .distant_object_angle
    sta .SIGNED_DIFF
    ; Comparison with absolute value.
    bpl +
    +nega
+
    cmp #33
    bcc .check_difference

    ; Difference is > 32, so adjustment is necessary.
    lda .SIGNED_DIFF
    bpl +
    ; Negative so ADD 64.
    clc
    adc #64
    jmp ++
+
    ; Positive so SUBTRACT 64.
    sec
    sbc #64
++
    sta .SIGNED_DIFF
    ; Put absolute value into the accumulator.
    bpl +
    +nega
+

.check_difference
    ; NOTE: absolute difference is in the accumulator...
    cmp #13
    bcs .not_visible
    ; Add 12 to the signed difference so it can be used as an index.
    ; Index will then be in the range [0,25).
    lda .SIGNED_DIFF
    clc
    adc #12
    sta .SIGNED_DIFF
    ; C flag clear indicates that distant object is visible.
    clc
    rts ; EXIT POINT.

.not_visible
    sec
    rts

; end sub bdrop_s_check_rotation
} ; !zone

; **************************************************

!zone {
;.iter   !byte   0
.MATHS0_16bit = $000a
.CURRENT_PATTERN_I = CURSOR_POS_HI
.ITER = CURSOR_POS_SR_LO 

bdrop_s_draw_distant_objects
    jsr bdrop_s_deactivate_sprites

    ; NOTE: only do this once each 'scene'.
    ldx #3
-
    lda golfer_v_direction_x_lo,x
    sta .MATHS0_16bit,x
    dex
    bpl -
    jsr maths_s_atan2
    stx bdrop_v_golfer_angle

    ldx #0
-
    stx .ITER

    jsr bdrop_s_check_rotation
    bcs .next

    ; Visible, so draw this one.
    lda bdrop_v_pattern_indices,x
    sta .CURRENT_PATTERN_I
    sta P0
    ldx MATHS0
    stx bdrop_v_current_position
    lda bdrop_l_POSITIONS_X,x 
    sta P1
    jsr bdrop_s_draw_pattern
    ; Is there a sprite associated with this object?
    ldx .CURRENT_PATTERN_I
    lda bdrop_v_spr_y,x
    beq .next
    jsr bdrop_s_init_sprite

.next
    ldx .ITER
    inx
    cpx #bdrop_c_MAX_DISTANT_OBJECTS ;bdrop_v_num_distant_objects
    bne -

    rts
; end sub bdrop_s_draw_distant_objects
} ; !zone

; **************************************************

; INPUTS:   P0 = pattern type, P1 = column
!zone {
bdrop_s_draw_pattern
    ldx P0
    lda bdrop_l_patterns_addr_lo,x
    sta P4
    lda bdrop_l_patterns_addr_hi,x
    sta P5
    lda bdrop_v_pattern_start_rows+1,x
    sta P0
    jsr ingm_s_draw_tile_pattern

    rts
; end sub bdrop_s_draw_pattern
} ; !zone

; **************************************************

!zone {
bdrop_s_prepare_draw
    lda #<ingm_s_default_draw_tile
    sta ingm_mod0 
    lda #>ingm_s_default_draw_tile
    sta ingm_mod0+1
    rts
; end sub bdrop_s_prepare_draw
} ; !zone

; **************************************************

!zone {
bdrop_s_deactivate_sprites
    ldx #bdrop_c_MAX_DISTANT_OBJECTS-1
    lda #0
    sta bdrop_v_current_sprite_slot
-
    sta bdrop_v_spr_active,x
    dex
    bpl -
    rts
; end sub bdrop_s_deactivate_sprites
} ; !zone

; **************************************************

; INPUTS:   CURSOR_POS_HI = pattern index in range [0,5),
;           CURSOR_POS_LO = distant object index.
!zone {
.PATTERN_I = CURSOR_POS_HI
.DOBJ_I = CURSOR_POS_SR_LO

bdrop_s_init_sprite
    ; Activate the sprite.
    ldx .DOBJ_I
    inc bdrop_v_spr_active,x

    ; Select the h/w and s/w sprite numbers from next available.
    ldy bdrop_v_current_sprite_slot
    inc bdrop_v_current_sprite_slot
    lda bdrop_l_AVAILABLE_HW_SPRITES,y    
    sta bdrop_v_spr_hw_num,x
    lda bdrop_l_AVAILABLE_SW_SPRITES,y    
    sta bdrop_v_spr_sw_num,x
    ; S/W number into Y for following...
    tay

    ; Now we're copying from pattern's sprite spec to this particular 
    ; distant object's s/w sprite details.
    ldx .PATTERN_I
    ; Y-position - a simple lookup.
    lda bdrop_v_spr_y,x
    sta spr_v_y,y

    lda bdrop_v_spr_hires,x
    sta spr_v_hires,y

    lda bdrop_v_spr_color,x
    sta spr_v_color,y
    ; Animation setup.
    lda bdrop_v_spr_ptr_from,x
    sta spr_v_current_ptr,y
    sta spr_v_anim_start_ptr,y
    lda bdrop_v_spr_ptr_to,x
    sta spr_v_anim_end_ptr,y
    lda bdrop_v_spr_framerate,x
    sta spr_v_anim_timer,y
    sta spr_v_framerate,y
    lda #0
    sta spr_v_yxpand,y
    sta spr_v_xxpand,y
    lda #1
    sta spr_v_bg_priority,y
    sta spr_v_anim_seq_inc,y        

    ; X-position - also a lookup...
    lda bdrop_v_spr_x_offset_lo,x
    sta MATHS0
    lda bdrop_v_spr_x_offset_hi,x
    sta MATHS1
    ldx bdrop_v_current_position
    lda bdrop_l_SPRITE_POSITIONS_X_LO,x
    clc 
    adc MATHS0
    sta spr_v_x_lo,y
    lda bdrop_l_SPRITE_POSITIONS_X_HI,x
    adc MATHS1
    sta spr_v_x_hi,y

    rts
; end sub bdrop_s_init_sprite
} ; !zone

; **************************************************

!zone {
.ITER = MATHS0

bdrop_s_update
    ; Which distant objects are currently visible?
    ldx #0
-
    stx .ITER

    lda bdrop_v_spr_active,x
    beq .next

    ; Make sure sprite is enabled.
    ldy bdrop_v_spr_hw_num,x
    lda utils_l_BIT_LOOKUP,y        
    ora SPENA
    sta SPENA

    lda bdrop_v_spr_sw_num,x
    pha
    ; Does this object's pattern have an animated sprite?
    ldy bdrop_v_pattern_indices,x
    lda bdrop_v_spr_framerate,y
    bne .animated
    ; Sprite is static.  Put s/w spr# in Y and then jump over animation
    ; routine.
    pla
    tay
    bne +

.animated
    lda bdrop_v_spr_anim_type,y
    beq .loop
    ; So it's pingpong.
    pla
    tax
    tay
    jsr spr_s_animate_pingpong
    jmp ++
.loop
    pla
    tax
    tay
    ; 'spr_animate_loop' wants s/w number in X.
    jsr spr_animate_loop
++
    ldx .ITER
+
    lda bdrop_v_spr_hw_num,x
    tax
    ; 'spr_s_write_to_vic_ii' wants s/w num in Y, h/w num in X.
    jsr spr_s_write_to_vic_ii
    ldx .ITER

.next
    inx
    ; NOTE: we must check all slots here as there will often be wraparound.
    cpx #bdrop_c_MAX_DISTANT_OBJECTS
    bne -

    rts
; end sub bdrop_s_update
} ; !zone

; **************************************************

; OUTPUT:   MATHS0 = active h/w sprites have relevant bit set.
!zone {
bdrop_s_find_active_hw_sprites
    ldx #bdrop_c_MAX_DISTANT_OBJECTS-1
    lda #0
    sta MATHS0

.loop
    lda bdrop_v_spr_active,x
    beq .next
    ldy bdrop_v_spr_hw_num,x
    lda utils_l_BIT_LOOKUP,y
    ora MATHS0
    sta MATHS0

.next
    dex
    bpl .loop

    rts
; end sub bdrop_s_find_active_hw_sprites
} ; !zone

; **************************************************

!zone {
.DELTA = 3

bdrop_s_rotate_objects
    ldx #0

-
    lda bdrop_v_angles,x
    clc
    adc #.DELTA
    cmp #64
    bcc +
    sec
    sbc #64
+
    sta bdrop_v_angles,x
    inx
    cpx #bdrop_c_MAX_DISTANT_OBJECTS
    bne -

    rts
; end sub bdrop_s_rotate_objects
} ; !zone

; **************************************************

; NOTE: this goes here because there's not enough room for it in the 'core' 
; section.
; FIXME: but maybe there is now?!
; X = sprite number (0-15).
!zone {
spr_s_animate_pingpong
    dec spr_v_anim_timer,x
    bne .end

    lda spr_v_framerate,x
    sta spr_v_anim_timer,x

    lda spr_v_anim_seq_inc,x
    beq .decrease

    ; Increment sprite data pointer.
    inc spr_v_current_ptr,x
    lda spr_v_current_ptr,x
    cmp spr_v_anim_end_ptr,x
    bne .end
    lda #0
    sta spr_v_anim_seq_inc,x
    beq .end

.decrease
    dec spr_v_current_ptr,x
    lda spr_v_current_ptr,x
    cmp spr_v_anim_start_ptr,x
    bne .end
    lda #1
    sta spr_v_anim_seq_inc,x

.end
    rts
; end sub spr_s_animate_pingpong
} ; !zone

; **************************************************

!zone {
bdrop_s_draw_all
    jsr bdrop_s_prepare_draw
    jsr bdrop_s_draw_distant_objects
    jsr bdrop_s_draw_bg
    rts
; end sub bdrop_s_draw_all
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

bdrop_c_SIZE = *-bdrop_c_BEGIN

