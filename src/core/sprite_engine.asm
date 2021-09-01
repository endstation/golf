; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; FIXME: possible optimizations.
; After calling spr_calc_dx/dy, won't always need to call a corresponding
; spr_move routine.  Store dx/dy value in accumulator at end of initial 
; routine to allow user to check it quickly for zero?!
; After a spr_move routine, update bb automatically based on delta or 
; clamped value?!


; *****************
; *** CONSTANTS ***
; *****************
spr_c_VISIBLE_ALL_L = 24
spr_c_VISIBLE_ALL_R = 64  ; With MSB set.
spr_c_VISIBLE_ALL_T = 50
spr_c_VISIBLE_ALL_B = 229
spr_c_OFFSCREEN_L = 0
spr_c_OFFSCREEN_R = 88
spr_c_OFFSCREEN_T = 29
spr_c_OFFSCREEN_B = 250

spr_c_LEFT    = 0
spr_c_RIGHT   = 1
spr_c_UP      = 2
spr_c_DOWN    = 3

spr_c_CURRENT_CHAR_ZP_LO = 59
spr_c_CURRENT_CHAR_ZP_HI = 60


; *****************
; *** VARIABLES ***
; *****************
; Make sure variables are aligned with page boundary because they'll be
; accessed with X register as index.  Room for 32 arrays of 8 bytes each.
;!align 255, 0
spr_v_x_lo                !fill   16,0
spr_v_x_hi                !fill   16,0
spr_v_y                   !fill   16,0
spr_v_vx_lo               !fill   16,0
spr_v_vx_hi               !fill   16,0
spr_v_vy_lo               !fill   16,0
spr_v_vy_hi               !fill   16,0
spr_v_vx_count            !fill   16,0
spr_v_vy_count            !fill   16,0
spr_v_direction_x         !fill   16,0
spr_v_direction_y         !fill   16,0
spr_v_anim_start_ptr      !fill   16,0
spr_v_anim_end_ptr        !fill   16,0
spr_v_anim_seq_inc        !fill   16,0
spr_v_anim_timer          !fill   16,0
spr_v_framerate           !fill   16,0
spr_v_current_ptr         !fill   16,0
spr_v_dx                  !fill   16,0
spr_v_dy                  !fill   16,0
spr_v_color               !fill   16,0
spr_v_yxpand        !fill   16,0
spr_v_xxpand        !fill   16,0
spr_v_bg_priority   !fill   16,0
; NOTE: all sprites are m/c by default.
spr_v_hires         !fill   16,0
; NOTE: should be 16-bit really!
spr_v_fg_priority         !byte   0

; Used with the spr_clamp_ routines.
spr_v_edge    !byte   0

; Used with spr_animate_onetime routine.
; Is set if advanced to next frame in this call.
spr_v_is_next_frame   !byte   0


; **************
; *** MACROS ***
; **************


; *******************
; *** SUBROUTINES ***
; *******************
; X = sprite number (0-7)
; NOTE: sprite will be clamped at left edge where x=0.
!zone {
spr_s_move_left
    lda spr_v_x_lo,x
    sec
    sbc spr_v_dx,x
    sta spr_v_x_lo,x
    lda spr_v_x_hi,x
    sbc #0
    sta spr_v_x_hi,x
    ; If bit #8 (i.e. MSB) goes below 0, we've reached left edge of screen 
    ; (hidden by border).  If C flag set, however, everything's fine.
    bcs .end

    ; Clamp at position X=0.
    lda #0
    sta spr_v_x_lo,x
    sta spr_v_x_hi,x

.end
    rts
; end sub spr_s_move_left
} ; !zone

; **************************************************

; X = sprite number (0-7)
!zone {
spr_s_move_right
    lda spr_v_x_lo,x
    clc
    adc spr_v_dx,x
    sta spr_v_x_lo,x
    lda spr_v_x_hi,x
    adc #0
    sta spr_v_x_hi,x
    ; NOTE: we assume client isn't going to allow x to wrap around at 
    ; right-hand of screen.
    rts
; end sub spr_s_move_right
} ; !zone

; **************************************************

; X = sprite number (0-7).
; NOTE: clamps sprite at 0.
!zone {
spr_s_move_up
    lda spr_v_y,x
    sec
    sbc spr_v_dy,x
    bcc .clamp
    jmp .end

.clamp
    lda #0

.end
    sta spr_v_y,x
    rts
; end sub spr_s_move_up
} ; !zone

; **************************************************

!zone {
; X = sprite number (0-7).
; NOTE: clamps sprite at 255 ($ff).
spr_s_move_down
    lda spr_v_y,x
    clc
    adc spr_v_dy,x
    bcs .clamp
    jmp .end

.clamp
    lda #$ff

.end
    sta spr_v_y,x
    rts
; end sub spr_s_move_down
} ; !zone

; **************************************************

; X = sprite number (0-7)
spr_s_calc_dx
    lda spr_v_vx_count,x
    clc
    adc spr_v_vx_lo,x
    sta spr_v_vx_count,x
    lda spr_v_vx_hi,x
    bcc +
    clc
    adc #1
+   sta spr_v_dx,x
    rts
; end sub spr_s_calc_dx

; **************************************************

; X = sprite number (0-7)
spr_s_calc_dy
    lda spr_v_vy_count,x
    clc
    adc spr_v_vy_lo,x
    sta spr_v_vy_count,x
    lda spr_v_vy_hi,x
    bcc +
    clc
    adc #1
+   sta spr_v_dy,x
    rts
; end sub spr_s_calc_dy

; **************************************************

;spr_s_clamp_top
;    rts
; end sub spr_s_clamp_top

; **************************************************

;spr_s_clamp_bottom
;    rts
; end sub spr_s_clamp_bottom

; **************************************************

; X = sprite number (0-7)
; Put edge in spr_edge variable.  Lowest valid position, so sprite x should be
; >= to it.
; ASSUME MSB NEEDS TO BE CLEAR.
!zone {
spr_s_clamp_left
    lda spr_v_x_hi,x
    bne .end

    lda spr_v_x_lo,x
    cmp spr_v_edge
    bcs .end
    
    lda spr_v_edge
    sta spr_v_x_lo,x

.end
    rts
; end sub spr_s_clamp_left
} ; zone

; **************************************************

; X = sprite number (0-7)
; Put edge in spr_edge variable.  Highest valid position, so sprite x should be
; <= to it.
; ASSUME MSB NEEDS TO BE CLEAR.
; OUTPUT: C flag set if clamping was necessary; else clear.
!zone {
spr_s_clamp_right
    clc

    ; Increment spr_edge because that means we can test if cmp leaves C flag 
    ; clear, in which case we're OK (A < memory).  Otherwise clamp sprite x
    ; to original value of spr_edge.
    inc spr_v_edge

    lda spr_v_x_lo,x
    cmp spr_v_edge
    bcc .end

    dec spr_v_edge
    lda spr_v_edge
    sta spr_v_x_lo,x
    
.end
    rts
; end sub spr_s_clamp_right
} ; zone

; **************************************************

; X = sprite number (0-7).
; NOTE: 'C' flag set if animation complete.
!zone {
spr_s_animate_onetime
    +clr spr_v_is_next_frame

    dec spr_v_anim_timer,x
    bne .end

    lda spr_v_framerate,x
    sta spr_v_anim_timer,x

    ; Check if the current frame is the last frame.  In which case there's
    ; nowhere else to go.
    lda spr_v_current_ptr,x
    cmp spr_v_anim_end_ptr,x
    bne +
    sec
    rts
 
+   ; Set flag to show we're going to the next frame.
    inc spr_v_is_next_frame
    ; Increment or decrement as appropriate.
    lda spr_v_anim_seq_inc,x
    beq .decrease
    inc spr_v_current_ptr,x
    jmp .end
.decrease
    dec spr_v_current_ptr,x

.end
    clc
    rts

; end sub spr_s_animate_onetime
} ; !zone

; **************************************************

; X = sprite number (0-7).
!zone {
spr_animate_loop
    dec spr_v_anim_timer,x
    beq +
    rts

+   lda spr_v_framerate,x
    sta spr_v_anim_timer,x

    ; Check if the current frame is the last frame and if it is, go back to 
    ; the beginning.
    lda spr_v_current_ptr,x
    cmp spr_v_anim_end_ptr,x
    bne +
    lda spr_v_anim_start_ptr,x
    sta spr_v_current_ptr,x
    rts

+   ; Not the last frame so just increment.
    inc spr_v_current_ptr,x
    rts
; end sub spr_animate_loop
} ; !zone

; **************************************************

; X = sprite number (0-7).
;!zone {
;spr_s_animate_pingpong
;    dec spr_v_anim_timer,x
;    bne .end
;
;    lda spr_v_framerate,x
;    sta spr_v_anim_timer,x
;
;    lda spr_v_anim_seq_inc,x
;    beq .decrease
;
;    ; Increment sprite data pointer.
;    inc spr_v_current_ptr,x
;    lda spr_v_current_ptr,x
;    cmp spr_v_anim_end_ptr,x
;    bne .end
;    lda #0
;    sta spr_v_anim_seq_inc,x
;    jmp .end
;
;.decrease
;    dec spr_v_current_ptr,x
;    lda spr_v_current_ptr,x
;    cmp spr_v_anim_start_ptr,x
;    bne .end
;    lda #1
;    sta spr_v_anim_seq_inc,x
;
;.end
;    rts
;; end sub spr_s_animate_pingpong
;} ; !zone

; **************************************************

; X = sprite # (0-7).
; Call this when place sprite at a new position manually to make sure MSIGX
; is set correctly.
spr_s_sync_msb
    lda spr_v_x_hi,x
    beq +
    +spr_m_set_msb
    rts
+   +spr_m_clear_msb
    rts
; end sub spr_s_sync_msb

; **************************************************

!zone {
spr_s_clear_msb_all
    lda #0
    sta MSIGX
    ldx #7
-   sta spr_v_x_hi,x
    dex
    bpl -
    rts
; end sub spr_s_clear_msb_all
} ; !zone

; **************************************************

; Y = sprite number (from)
; X = sprite number (to)
!zone {
spr_s_write_to_vic_ii
    ; YXPAND:
    lda spr_v_yxpand,y
    beq .clear_yxpand
    ; Set YXPAND.
    lda utils_l_BIT_LOOKUP,x
    ora YXPAND
    bne +
.clear_yxpand
    lda utils_l_EOR_BIT_LOOKUP,x
    and YXPAND
+
    sta YXPAND

    ; XXPAND:
    lda spr_v_xxpand,y
    beq .clear_xxpand
    ; Set XXPAND.
    lda utils_l_BIT_LOOKUP,x
    ora XXPAND
    bne +
.clear_xxpand
    lda utils_l_EOR_BIT_LOOKUP,x
    and XXPAND
+
    sta XXPAND

    ; SPBGPR:
    lda spr_v_bg_priority,y
    beq .clear_spbgpr
    ; Set SPBGPR.
    lda utils_l_BIT_LOOKUP,x
    ora SPBGPR
    bne +
.clear_spbgpr
    lda utils_l_EOR_BIT_LOOKUP,x
    and SPBGPR
+
    sta SPBGPR

    ; Multiply value in X by 2 so can index SP0X, etc.
    txa
    asl
    tax

    ; Position.
    lda spr_v_x_lo,y
    sta SP0X,x
    lda spr_v_y,y
    sta SP0X+1,x
    ; For MSB, put X back to where it was.
    txa
    lsr
    tax
    lda spr_v_x_hi,y
    beq +
    +spr_m_set_msb
    jmp ++
+   +spr_m_clear_msb

++  ; Color.
    lda spr_v_color,y
    sta SP0COL,x

    ; Hires or multicolor sprite?
    lda spr_v_hires,y
    +branch_if_false .multicolor
    ; So it's hires.
    lda utils_l_EOR_BIT_LOOKUP,x
    and SPMC
    jmp +
.multicolor
    lda utils_l_BIT_LOOKUP,x
    ora SPMC
+
    sta SPMC

    ; Data pointer.
    lda spr_v_current_ptr,y
    sta gfxs_c_SPR_PTR_BASE,x

    rts
; end sub spr_s_write_to_vic_ii
} ; !zone

; **************************************************

; FIXME: also clears the f/g priority array!
!zone {
spr_s_clear_all_expands
    ldx #((3*16)-1)
    lda #0
-
    sta spr_v_yxpand,x
    dex
    bpl -
    rts
; end sub spr_s_clear_all_expands
} ; !zone

; **************************************************

!zone {
spr_s_hide_all
    ldx #15
    lda #0
-
    sta SP0X,x
    dex
    bpl -

    sta YXPAND
    sta XXPAND

    rts
; end sub spr_s_hide_all
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

