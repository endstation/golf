; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


powarc_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; Where we start drawing from.
powarc_c_TOP_ROW = 17
; NOTE: e.g. if two chars in, 2*8 bytes.
powarc_c_X_OFFSET_BYTES = 4*8

;POWARC_BASE_ADDR = gfxs_c_BITMAP_BASE+18*8*40+8
POWARC_POWER_STEPS_DATA
;    !bin "../../assets/tables/powsteps.bin"
    !bin    "../../assets/powerarc/power_steps.bin"
POWARC_PRECISION_STEPS_DATA
;    !bin "../../assets/tables/powarc_precision_steps3.bin"
    !bin    "../../assets/powerarc/precision_steps.bin"
POWARC_STEP_THRESHOLD = 3
POWARC_POWERING_UP_NUM_STEPS = 36;44;14

powarc_l_BITMAP_COPY_ROWS_LO
    !for i,6 {
        !byte <(powarc_v_bitmap_copy+((i-1)*48)-powarc_c_X_OFFSET_BYTES)
    } ; !for
powarc_l_BITMAP_COPY_ROWS_HI
    !for i,6 {
        !byte >(powarc_v_bitmap_copy+((i-1)*48)-powarc_c_X_OFFSET_BYTES)
    } ; !for

powarc_c_PRECISION_DELTA = 40
powarc_c_INITIAL_DELAY_LO = 50
powarc_c_INITIAL_DELAY_HI = 5

powarc_l_BASE_BITMAP_ICON
;    !bin    "../../assets/patterns/powarcbase.bin"
    !bin    "../../assets/powerarc/base_icon.bin"

powarc_c_FULL_POWER_OFFSET = 31
powarc_l_POWER_REDUCTIONS = *-32
    !byte   24,22,20,18,16

powarc_c_PRECISION_BEGIN    = 12
powarc_c_PRECISION_CENTER   = 20
powarc_c_PRECISION_END      = 28
powarc_c_PRECISION_BUNKER_BEGIN = 16
powarc_c_PRECISION_BUNKER_END   = 24

powarc_c_MAX_SW_SPR_NUM = 15
powarc_c_MAX_HW_SPR_NUM = ball_c_SHADOW_SW_SPR_NUM 
powarc_c_MAX_SPR_X = 19+(3*8)+spr_c_VISIBLE_ALL_L 
;powarc_c_MAX_SPR_Y = 139+spr_c_VISIBLE_ALL_T
powarc_c_MAX_SPR_Y = 156+spr_c_VISIBLE_ALL_T
powarc_c_MAX_SPR_PTR = (play_l_MAX_STR_SPRITE-$c000)/64
powarc_c_MAX_ANIMATION_FRAME_RATE = 5
powarc_l_MAX_ANIM_COLOR_CYCLE   !byte WHITE,YELLOW,ORANGE,RED,ORANGE,YELLOW
                                !byte WHITE,LIGHT_BLUE,(-1)
;powarc_l_TOP_PULSE_COLORS
;    !byte   (GREY3<<4)|GREY3
;    !byte   (GREY2<<4)|GREY3
;    !byte   (ORANGE<<4)|GREY3
;    !byte   (BROWN<<4)|GREY3
;    !byte   (ORANGE<<4)|GREY3
;    !byte   (GREY2<<4)|GREY3
;    !byte   0
;; Where in video text RAM the color should be pulsed.
;powarc_c_TOP_PULSE_ADDR0 = gfxs_c_DISPLAY_BASE+17*40+4 
;powarc_c_TOP_PULSE_ADDR1 = gfxs_c_DISPLAY_BASE+18*40+4 
;powarc_c_TOP_PULSE_FRAME_RATE = 6

; NOTE: these tables are used when we're pulsing the suggested power step
; for putts on 'easy' greens.
powarc_l_PUTT_ASSIST_STEP_MARKERS
    !bin    "../../assets/powerarc/putting_markers.bin"
powarc_l_PUTT_ASSIST_STEP_MARKER_OFFSETS    !byte   0,19,35,42,61,80
powarc_l_PUTT_ASSIST_COLORS
    !byte   %11111111   ; black
    !byte   %00000000   ; orange
    !byte   %10101010   ; grey3
    !byte   %01010101   ; white
    !byte   %10101010   ; grey3
    !byte   %00000000   ; orange
powarc_c_PUTT_ASSIST_NUM_COLORS = 6
;powarc_c_PUTT_ASSIST_FRAME_RATE = 4
; For power steps: 6,11,16,21,26,31.
powarc_l_PUTT_ASSIST_AVG_DISTANCE_FT
    !byte   2,6,14,24,37
powarc_c_PUTT_ASSIST_NUM_STEPS = 6

; For use when putting.  Otherwise speed is always 'normal'.
powarc_c_POWER_UP_SPEED_NORMAL  = 0
powarc_c_POWER_UP_SPEED_SLOW    = 1
powarc_c_PUTT_ASSIST_FRAME_RATE = 4
; NOTE: indexed by 'powarc_v_power_up_speed'.
powarc_l_POWER_UP_FRAME_RATES       !byte   1,2

; NOTE: 6 bytes only.  Top & bottom rows of char don't change.
powarc_c_SLOW_MARKER_NUM_BYTES = 6
powarc_c_SLOW_MARKER_DATA       !byte   $2b,$83,$ab,$0b,$8b,$a3
powarc_c_SLOW_MARKER_CLEAR_DATA !byte   $03,$03,$03,$03,$03,$03
powarc_c_PUNCH_MARKER_DATA      !byte   $2b,$8b,$8b,$a3,$83,$83
powarc_c_SLOW_MARKER_DESTINATION = gfxs_c_BITMAP_BASE+22*40*8+9*8+1 


; *****************
; *** VARIABLES ***
; *****************
powarc_v_frame_count  !byte   0

; NOTE: keep these six variables together because they're cleared in a loop!
powarc_iter         !byte   0
powarc_v_step_count   !byte   0
powarc_v_power_offset   !byte   0
; NOTE: keep hold of this value for an easy test to see if spin was in a valid
; range for bunker shot...
powarc_v_precision_offset !byte   0
powarc_v_power_maxed_out    !byte   0
powarc_v_precision_ended    !byte   0

powarc_v_bitmap_copy  !fill   (6*6*8),0

powarc_v_precision_delay_lo !byte   0
powarc_v_precision_delay_hi !byte   0
powarc_v_min_delay_reached  !byte   0

powarc_v_max_animation_active   !byte   0
powarc_v_max_anim_frame_count   !byte   0
powarc_v_max_anim_iter          !byte   0

;powarc_v_top_pulse_iter     !byte   0
;powarc_v_top_pulse_count    !byte   0
;powarc_v_top_pulse_active   !byte   0

powarc_v_putt_assist_frame_count    !byte   0
powarc_v_putt_assist_color_index    !byte   0
; NOTE: make this the index into the table!
powarc_v_putt_assist_index          !byte   0
powarc_v_putt_assist_suggested_step !byte   0
powarc_v_putt_assist_is_active      !byte   0

powarc_v_power_up_speed !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
powarc_s_update
    lda golfer_v_current_state
    cmp #golfer_c_STATE_POWERING_UP
    beq .powering_up
    cmp #golfer_c_STATE_PRECISION
    beq .precision

    ; FIXME: why is this routine being called at all if not in middle of 
    ; golf swing?!  (Answer: holding?)
    bne .end

.powering_up
    dec powarc_v_frame_count
    bne .end
    ldx powarc_v_power_up_speed
    lda powarc_l_POWER_UP_FRAME_RATES,x
    sta powarc_v_frame_count
    jsr powarc_s_advance_power
    rts ; EXIT POINT.

.precision
    lda powarc_v_min_delay_reached 
    +branch_if_true +

    ; We haven't yet reached a delay of one frame, so keep subtracting...
    lda powarc_v_precision_delay_lo 
    sec
    sbc #powarc_c_PRECISION_DELTA 
    sta powarc_v_precision_delay_lo 
    lda powarc_v_precision_delay_hi 
    sbc #0
    sta powarc_v_precision_delay_hi 
    bne +
    inc powarc_v_precision_delay_hi
    inc powarc_v_min_delay_reached
+
    ; Time to advance marker (and go to next step)?
    dec powarc_v_frame_count
    bne .end
    lda powarc_v_precision_delay_hi
    sta powarc_v_frame_count
    jsr powarc_s_advance_precision

.end
    rts
; end sub powarc_s_update
} ; !zone

; **************************************************

!zone {
powarc_s_reset
    ; NOTE: set 'frame_count' to 1 initially so that at least one segment of
    ; the power-arc is always filled in, no matter how briefly the player
    ; holds down the fire button.
    lda #1
    sta powarc_v_frame_count
    lda #powarc_c_POWER_UP_SPEED_NORMAL
    sta powarc_v_power_up_speed

;    sta powarc_v_top_pulse_active
;    lda #powarc_c_TOP_PULSE_FRAME_RATE
;    sta powarc_v_top_pulse_count
    lda #0
    sta powarc_v_max_animation_active   
    ; NOTE: disable this because it's still being updated while 'SHOT_COMPLETE'.
    ; This means an active putt assist could corrupt clean bitmap icon after
    ; it's been drawn (below).
    sta powarc_v_putt_assist_is_active

;    sta powarc_v_top_pulse_iter
    tax
-
    sta powarc_iter,x
    inx
    cpx #(powarc_v_precision_ended-powarc_iter+1)
    bne -

    ; NOTE: POWARC_FILL_SRC_ITER_LO/HI are zero-page variables.
    lda #<POWARC_POWER_STEPS_DATA
    sta POWARC_FILL_SRC_ITER_LO 
    lda #>POWARC_POWER_STEPS_DATA
    sta POWARC_FILL_SRC_ITER_HI 

    lda #<powarc_l_BASE_BITMAP_ICON 
    sta P0
    lda #>powarc_l_BASE_BITMAP_ICON 
    sta P1
    lda #$ff
    sta P2
    jsr icon_s_draw

    ; Make off-screen copy of bitmap data.
    ; 6*6*8 = 288 bytes.  Must skip over 5-byte header.
    ldx #5
-
    lda powarc_l_BASE_BITMAP_ICON,x
    sta powarc_v_bitmap_copy-5,x  
    lda powarc_l_BASE_BITMAP_ICON+144,x
    sta powarc_v_bitmap_copy+144-5,x
    inx
    cpx #149
    bne -

    jsr powarc_s_reset_putt_assist

    rts
; end sub powarc_s_reset
} ; !zone

; **************************************************

!zone {
powarc_s_advance_power
    ldy #0
    
.loop_top
    ; First the row.  If it's 0, zp is already correctly set.  If it's $ff,
    ; the 'step' is complete.
    lda (POWARC_FILL_SRC_ITER_LO),y
    beq .offset
    cmp #$ff
    beq .end
    ; Transfer row number from A to X and set up zero page.
    tax
    lda dp_l_BITMAP_ROWS_LO,x
    sta POWARC_LO 
    lda dp_l_BITMAP_ROWS_HI,x
    sta POWARC_HI 
    ; ... and same for the off-screen copy.
    ; Divide by 8 to get correct index.
    txa
    lsr
    lsr
    lsr
    tax
    lda powarc_l_BITMAP_COPY_ROWS_LO-powarc_c_TOP_ROW,x
    sta POWARC_COPY_LO 
    lda powarc_l_BITMAP_COPY_ROWS_HI-powarc_c_TOP_ROW,x
    sta POWARC_COPY_HI 

.offset
    iny
    lda (POWARC_FILL_SRC_ITER_LO),y
    ; Store offset temporarily (- i.e. offset from beginning of current row).
    sta MATHS0

    ; Now get the pattern and draw it.
    iny
    ; Need to store iterator temporarily.
    sty MATHS1
    lda (POWARC_FILL_SRC_ITER_LO),y
    ; Load offset into Y.
    ldy MATHS0
;    ora (POWARC_LO),y 
    sta (POWARC_LO),y
;    dey
    sta (POWARC_COPY_LO),y

    ; Load iterator back into Y.
    ldy MATHS1
    iny
    jmp .loop_top

.end

    iny
    ; Add this offset to z.p. pointer, ready for next time.
    tya
    clc
    adc POWARC_FILL_SRC_ITER_LO
    sta POWARC_FILL_SRC_ITER_LO
    lda POWARC_FILL_SRC_ITER_HI
    adc #0
    sta POWARC_FILL_SRC_ITER_HI

    inc powarc_v_step_count
    lda powarc_v_step_count
    cmp #POWARC_POWERING_UP_NUM_STEPS
    bne +
    ; Already at max power so set flag.
    sta powarc_v_power_maxed_out
+
    rts
; end sub powarc_s_advance_power
} ; !zone

; **************************************************

!zone {
.ITER = MATHS1

powarc_s_advance_precision
    ldy #0
.loop_erase
    ; Get the row.  Zero means the same row as last time; $ff means this
    ; section has ended.
    lda (POWARC_FILL_SRC_ITER_LO),y
    beq .offset
    cmp #$ff
    beq .draw
    ; Must set z.p. pointers for destination.
    tax
    lda dp_l_BITMAP_ROWS_LO,x
    sta POWARC_LO 
    lda dp_l_BITMAP_ROWS_HI,x
    sta POWARC_HI 
    ; Use copy as source (cf. masking).
    txa
    lsr
    lsr
    lsr
    tax
    lda powarc_l_BITMAP_COPY_ROWS_LO-powarc_c_TOP_ROW,x
    sta POWARC_COPY_LO
    lda powarc_l_BITMAP_COPY_ROWS_HI-powarc_c_TOP_ROW,x
    sta POWARC_COPY_HI

.offset
    iny
    ; Store offset temporarily in MATHS0.
    lda (POWARC_FILL_SRC_ITER_LO),y
    sta MATHS0
    ; Get the mask.
    iny
    lda (POWARC_FILL_SRC_ITER_LO),y
    ; Put iterator temporarily in MATHS1.
    sty .ITER
    ldy MATHS0
    ; Now we have mask in A and offset in Y.
    ; AND with bitmap and then write the result.
    and (POWARC_COPY_LO),y
    sta (POWARC_LO),y
    ; Skip over 'pattern', then increment again so ready for next entry.
    ; Restore iterator into Y.
    ldy .ITER
    iny
    iny
    ; This is the 'restore' pattern:
    ; FIXME: is an AND required here?!
    lda (POWARC_FILL_SRC_ITER_LO),y
    sty .ITER
    ldy MATHS0
    ora (POWARC_COPY_LO),y
    sta (POWARC_LO),y

    ldy .ITER
    iny
    jmp .loop_erase
    
.draw
    ; Add Y+1 to the source pointer.  This is what we'll erase next time
    ; round.
    iny
    tya
    clc
    adc POWARC_FILL_SRC_ITER_LO
    sta POWARC_FILL_SRC_ITER_LO
    lda POWARC_FILL_SRC_ITER_HI
    adc #0
    sta POWARC_FILL_SRC_ITER_HI

    ; The z-p address pointer has now been reset, so begin to index from
    ; 0 again...
    ldy #0
.loop_draw
    lda (POWARC_FILL_SRC_ITER_LO),y
    beq .offset2
    cmp #$ff
    beq .end
    tax
    lda dp_l_BITMAP_ROWS_LO,x
    sta POWARC_LO 
    lda dp_l_BITMAP_ROWS_HI,x
    sta POWARC_HI 
    txa
    lsr
    lsr
    lsr
    tax
    lda powarc_l_BITMAP_COPY_ROWS_LO-powarc_c_TOP_ROW,x
    sta POWARC_COPY_LO
    lda powarc_l_BITMAP_COPY_ROWS_HI-powarc_c_TOP_ROW,x
    sta POWARC_COPY_HI
.offset2
    iny
    lda (POWARC_FILL_SRC_ITER_LO),y
    sta MATHS0
    ; Get pattern.  Remember to skip over mask!
    iny
    iny
    lda (POWARC_FILL_SRC_ITER_LO),y
    sty .ITER
    ldy MATHS0
    ora (POWARC_COPY_LO),y
    sta (POWARC_LO),y

    ; Restore iterator, advance to next entry & go again.
    ldy .ITER
    iny
    iny
    jmp .loop_draw

.end
    ; See if next byte in table is $fe.  If it is, we're at the end of
    ; the table and won't be able to process it any further.
    inc powarc_v_step_count
    iny
    lda (POWARC_FILL_SRC_ITER_LO),y
    cmp #$fe
    bne +

    ; FIXME: call a routine on golfer module instead...
    ;inc golfer_v_shot_spin_set
    sta powarc_v_precision_ended 
+
    rts
; end sub powarc_s_advance_precision
} ; !zone

; **************************************************

!zone {
powarc_s_init_precision
    lda #0
    sta powarc_iter
    sta powarc_v_step_count
    sta powarc_v_min_delay_reached

    lda #powarc_c_INITIAL_DELAY_LO 
    sta powarc_v_precision_delay_lo 
    lda #powarc_c_INITIAL_DELAY_HI 
    sta powarc_v_precision_delay_hi 
    sta powarc_v_frame_count

    lda #<POWARC_PRECISION_STEPS_DATA
    sta POWARC_FILL_SRC_ITER_LO 
    lda #>POWARC_PRECISION_STEPS_DATA
    sta POWARC_FILL_SRC_ITER_HI 

    rts
; end sub powarc_s_init_precision
} ; !zone

; **************************************************

; INPUTS:   A = value of 'powarc_v_power_offset'
!zone {
powarc_s_init_max_animation
    ; We'll go through with this only if at max power!
    cmp #powarc_c_FULL_POWER_OFFSET
    beq +
    rts ; EXIT POINT.

+
    lda #<powarc_c_MAX_SPR_X
    sta spr_v_x_lo+powarc_c_MAX_SW_SPR_NUM
    lda #>powarc_c_MAX_SPR_X
    sta spr_v_x_hi+powarc_c_MAX_SW_SPR_NUM
    lda #powarc_c_MAX_SPR_Y
    sta spr_v_y+powarc_c_MAX_SW_SPR_NUM
    lda #powarc_c_MAX_SPR_PTR
    sta spr_v_current_ptr+powarc_c_MAX_SW_SPR_NUM
    lda #WHITE
    sta spr_v_color+powarc_c_MAX_SW_SPR_NUM
    sta spr_v_hires+powarc_c_MAX_SW_SPR_NUM
    sta spr_v_xxpand+powarc_c_MAX_SW_SPR_NUM 
    sta powarc_v_max_animation_active

    lda #powarc_c_MAX_ANIMATION_FRAME_RATE
    sta powarc_v_max_anim_frame_count
    lda #0
    sta powarc_v_max_anim_iter

    ldy #sfx_c_MAX_POWER
    jsr snd_s_init_sfx

    rts
; end sub powarc_s_init_max_animation
} ; !zone

; **************************************************

!zone {
powarc_s_update_max_animation
    lda powarc_v_max_animation_active
    +branch_if_false .end

    dec powarc_v_max_anim_frame_count
    bne .end
    ldx powarc_v_max_anim_iter
    inx
    lda powarc_l_MAX_ANIM_COLOR_CYCLE,x
    bpl +

    ; Animation has ended.
    lda #0
    sta powarc_v_max_animation_active

    ; FIXME: hack!
    ; Push h/w sprite offscreen to hide it.
    lda #spr_c_OFFSCREEN_B 
    sta SP0Y+2*powarc_c_MAX_HW_SPR_NUM

    rts ; EXIT POINT.

+
    sta spr_v_color+powarc_c_MAX_SW_SPR_NUM
    stx powarc_v_max_anim_iter
    lda #powarc_c_MAX_ANIMATION_FRAME_RATE
    sta powarc_v_max_anim_frame_count

.end
    rts
; end sub powarc_s_update_max_animation
} ; !zone

; **************************************************

!zone {
powarc_s_draw_max_animation
    lda powarc_v_max_animation_active
    +branch_if_false .end

    ldy #powarc_c_MAX_SW_SPR_NUM
    ldx #powarc_c_MAX_HW_SPR_NUM
    jsr spr_s_write_to_vic_ii

.end
    rts
; end sub powarc_s_draw_max_animation
} ; !zone

; **************************************************

;!zone {
;powarc_s_update_top_pulse
;    lda powarc_v_top_pulse_active
;    +branch_if_false .end
;
;    dec powarc_v_top_pulse_count
;    bne .end
;    lda #powarc_c_TOP_PULSE_FRAME_RATE
;    sta powarc_v_top_pulse_count
;    
;    ldx powarc_v_top_pulse_iter
;-
;    lda powarc_l_TOP_PULSE_COLORS,x
;    bne +
;    ldx #0
;    beq -
;+
;    sta powarc_c_TOP_PULSE_ADDR0
;    sta powarc_c_TOP_PULSE_ADDR1
;
;    inx
;    stx powarc_v_top_pulse_iter
;    
;.end
;    rts
;; end sub powarc_s_update_top_pulse
;} ; !zone

; **************************************************

;!zone {
;powarc_s_deactivate_top_pulse
;    lda #0
;    sta powarc_v_top_pulse_active
;    lda #(GREY3<<4)|GREY3
;    sta powarc_c_TOP_PULSE_ADDR0
;    sta powarc_c_TOP_PULSE_ADDR1
;    rts
;; end sub powarc_s_deactivate_top_pulse
;} ; !zone

; **************************************************

!zone {
powarc_s_reset_putt_assist
    lda round_v_must_putt
    +branch_if_false .no_assist

    ; Activate the putting assistant & initialize variables.
    lda #1
    sta powarc_v_putt_assist_is_active
;    lda #powarc_c_PUTT_ASSIST_FRAME_RATE
    ; NOTE: not the full frame rate value here - just let it begin to cycle
    ; straight away.
    sta powarc_v_putt_assist_frame_count
    lda #0
    sta powarc_v_putt_assist_color_index
    jsr powarc_s_select_best_power_step
    rts ; EXIT POINT.

.no_assist
    lda #0
    sta powarc_v_putt_assist_is_active

    rts
; end sub powarc_s_reset_putt_assist
} ; !zone

; **************************************************

!zone {
.MYCOLOR        = MATHS0
.COMP_MASK      = MATHS1
.MASKED_COLOR   = MATHS2
; NOTE: i.e. the power-arc icon is drawn at char row 17.
.TOP_CHAR_ROW_BITMAP = 17

powarc_s_update_putt_assist
    lda powarc_v_putt_assist_is_active
    +branch_if_false .end

    dec powarc_v_putt_assist_frame_count
    bne .end

    ; Go to the next color in the cycle.  Wraparound if we've reached the end
    ; of the table.
    ldx powarc_v_putt_assist_color_index
    inx
    cpx #powarc_c_PUTT_ASSIST_NUM_COLORS 
    bne +
    ldx #0
+
    stx powarc_v_putt_assist_color_index
    lda powarc_l_PUTT_ASSIST_COLORS,x
    sta .MYCOLOR
    ; Reset frame count.
;    ldx powarc_v_power_up_speed
;    lda powarc_l_PUTT_ASSIST_FRAME_RATES,x
    lda #powarc_c_PUTT_ASSIST_FRAME_RATE
    sta powarc_v_putt_assist_frame_count

    ; X register will hold index into the 'powarc_l_PUTT_ASSIST_STEP_MARKERS'
    ; table for the rest of the subroutine.
    ldx powarc_v_putt_assist_suggested_step 
.loop_top
    ; Load row directly into Y register.
    ldy powarc_l_PUTT_ASSIST_STEP_MARKERS,x
    beq .same_row
    cpy #$ff
    beq .end

    ; So Y now holds the bitmap row.  This can be used directly with 
    ; 'dp_l_BITMAP_ROWS_LO/HI' but will need to be divided by 8 for use with
    ; 'powarc_l_BITMAP_COPY_ROWS_LO/HI'.  Will also need to subtract 17 from
    ; that latter lookup table as well.
    lda dp_l_BITMAP_ROWS_LO,y
    sta PUTT_ASSIST_DEST_ZP_LO  
    lda dp_l_BITMAP_ROWS_HI,y
    sta PUTT_ASSIST_DEST_ZP_HI  
    tya
    lsr
    lsr
    lsr
    tay
    lda powarc_l_BITMAP_COPY_ROWS_LO-(.TOP_CHAR_ROW_BITMAP),y
    sta PUTT_ASSIST_SRC_ZP_LO   
    lda powarc_l_BITMAP_COPY_ROWS_HI-(.TOP_CHAR_ROW_BITMAP),y
    sta PUTT_ASSIST_SRC_ZP_HI   

.same_row
    ; NOTE: X is still valid as our index into the main table.
    inx
    ; Load the offset directly into Y register.
    ldy powarc_l_PUTT_ASSIST_STEP_MARKERS,x
    inx
    ; Push the mask onto the stack.
    lda powarc_l_PUTT_ASSIST_STEP_MARKERS,x
    pha
    ; And create complementary mask 'on-the-fly'.
    eor #$ff
    sta .COMP_MASK

    ; Now we are ready to read (from the offscreen copy) and write (to the
    ; bitmap).
    pla
    and .MYCOLOR
    sta .MASKED_COLOR
    lda (PUTT_ASSIST_SRC_ZP_LO),y   
    and .COMP_MASK
    ora .MASKED_COLOR
    sta (PUTT_ASSIST_DEST_ZP_LO),y

    inx
    bne .loop_top

.end
    rts
; end sub powarc_s_update_putt_assist
} ; !zone

; **************************************************

!zone {
powarc_s_select_best_power_step
    ldx round_v_current_player
    ldy #0

.loop
    lda powarc_l_PUTT_ASSIST_AVG_DISTANCE_FT,y
    cmp round_v_current_distance_lo
    bcs .found
    iny
    cpy #(powarc_c_PUTT_ASSIST_NUM_STEPS-1)
    bne .loop
    
.found
    sty powarc_v_putt_assist_index          
    lda powarc_l_PUTT_ASSIST_STEP_MARKER_OFFSETS,y
    sta powarc_v_putt_assist_suggested_step 

    rts
; end sub powarc_s_select_best_power_step
} ; !zone

; **************************************************

!zone {
powarc_s_toggle_power_up_speed
    ; Preserve value of X.
    +utils_m_save_x_to_stack 

    ; Use X for source index.  Slow speed as default.
    ldx #(powarc_c_SLOW_MARKER_NUM_BYTES-1)
    lda powarc_v_power_up_speed
    eor #$01
    sta powarc_v_power_up_speed
    bne .do_copy
    ; So normal speed...
    ldx #((powarc_c_SLOW_MARKER_NUM_BYTES*2)-1)

.do_copy
    jsr powarc_s_display_clear_marker

    +utils_m_restore_x_from_stack 
    
    rts
; end sub powarc_s_toggle_power_up_speed
} ; !zone

; **************************************************

; INPUT:    Z flag clear if punch selected.
!zone {
powarc_s_toggle_punch
    bne .punch_selected
    ldx #((powarc_c_SLOW_MARKER_NUM_BYTES*2)-1)
    bne +
.punch_selected
    ldx #((powarc_c_SLOW_MARKER_NUM_BYTES*3)-1)
+
    jsr powarc_s_display_clear_marker
    rts
; end sub powarc_s_toggle_punch
} ; !zone

; **************************************************

; NOTE: same routine can be used for 'slow' and 'punch' markers.
; INPUTS:   X = offset to final byte from powarc_c_SLOW_MARKER_DATA.
!zone {
powarc_s_display_clear_marker
    ; Use Y to count chars in copy routine.
    ldy #(powarc_c_SLOW_MARKER_NUM_BYTES-1)
-
    lda powarc_c_SLOW_MARKER_DATA,x
    sta powarc_c_SLOW_MARKER_DESTINATION,y
    dex
    dey
    bpl -

    rts
; end sub powarc_s_display_clear_marker
} ; !zone

; **************************************************

; Draw disruptive pattern where device would normally be.  Prevents this area
; from being 'textured' when rendering 3D.  Actual device drawn afterwards,
; even if rendering is skipped.
!zone {
.PATTERN = %00011011

powarc_s_draw_dummy
    lda #.PATTERN
    ldx #((6*8)-1)
-
    sta gfxs_c_BITMAP_BASE+(17*40*8)+(4*8),x
    sta gfxs_c_BITMAP_BASE+(18*40*8)+(4*8),x
    sta gfxs_c_BITMAP_BASE+(19*40*8)+(4*8),x
    sta gfxs_c_BITMAP_BASE+(20*40*8)+(4*8),x
    sta gfxs_c_BITMAP_BASE+(21*40*8)+(4*8),x
    sta gfxs_c_BITMAP_BASE+(22*40*8)+(4*8),x
    dex
    bpl -
    rts
; end sub powarc_s_draw_dummy
} ; !zone

; **************************************************
; **************************************************
; **************************************************

powarc_c_SIZE = *-powarc_c_BEGIN

