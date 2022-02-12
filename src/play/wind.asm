; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


wind_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
wind_c_ARROW_LEN = 96
wind_l_RANDOM_MASKS     !byte   $01,$01,$03
wind_l_RANDOM_ADDENDS   !byte   $00,$02,$03   
wind_c_INDICATOR_DEST = gfxs_c_BITMAP_BASE+(19*320)+(1*8)+4 
; NOTE: color codes = (00) b/g, (01) white, (10) grey, (11) black
wind_c_INDICATOR_BYTES
    !byte   %11010000,0,0
    !byte   %11010101,0,0
    !byte   %11010101,%01010000,0
    !byte   %11010101,%01010101,0
    !byte   %11010101,%01010101,%01010000
    !byte   %11010101,%01010101,%01010101
wind_l_INDICATOR_BYTES_INDICES = *-1
    !byte   0,3,6,9,12,15

; FIXME: totally arbitrary values!!!  These are the 'multipliers' for when
; calculating the final vector.
;wind_l_SPEEDS   !byte   0,240,240,240,240,240,240
;wind_l_MULTIPLIERS_LO   !byte   <0,<4,<8,<12,<16,<20,<24   
;wind_l_MULTIPLIERS_HI   !byte   >0,>4,>8,>12,>16,>20,>24
wind_l_MULTIPLIERS_LO   !byte   <0,<8,<14,<18,<20,<22,<24   
wind_l_MULTIPLIERS_HI   !byte   >0,>8,>14,>18,>20,>22,>24


; *****************
; *** VARIABLES ***
; *****************
; These variables hold the wind vector relative to world space.
wind_v_vx_lo    !byte   0
wind_v_vx_hi    !byte   0
wind_v_vz_lo    !byte   0
wind_v_vz_hi    !byte   0
wind_v_quadrant !byte   0

wind_v_index    !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
.ANGLE  = CAMERA1

wind_s_init
    jsr rand_s_get
    and #$3f
    sta MATHS4  ; angle for call to 'maths_s_rotate'

    lda #winslp_c_ARROW_LEN 
    sta wind_v_vx_lo
    lda #0
    sta wind_v_vx_hi
    sta wind_v_vz_lo
    sta wind_v_vz_hi

    lda #<wind_v_vx_lo
    sta MATHS0
    lda #>wind_v_vx_lo
    sta MATHS1
    lda #<wind_v_vz_lo
    sta MATHS2
    lda #>wind_v_vz_lo
    sta MATHS3
    clc
    jsr maths_s_rotate
    
    ; Calculate a random wind speed and store index.
    jsr rand_s_get
    ldx shared_v_wind_difficulty      
    and wind_l_RANDOM_MASKS,x
    clc
    adc wind_l_RANDOM_ADDENDS,x
    sta wind_v_index

    rts
; end sub wind_s_init
} ; !zone

; **************************************************

!zone {
wind_s_rotate
    lda #<wind_v_vx_lo
    sta P0
    lda #>wind_v_vx_lo
    sta P1
    jsr winslp_s_rotate

    ldx wind_v_index
    bne +
    ; No wind so set everything to 0.
    stx winslp_v_final_vx_lo
    stx winslp_v_final_vx_hi
    stx winslp_v_final_vz_lo
    stx winslp_v_final_vz_hi
    rts ; EXIT POINT.

;    lda wind_l_SPEEDS,x
;    sta P0
    ; Ignore 'delay'.
;    jsr winslp_s_calc_final_vector

+
    ; Scale up the wind vector starting from base of length=96.
    ; Needs to be fairly big because its purpose is to determine the ball's
    ; apparent velocity (against which air resistance will be applied), rather
    ; than supply delta values to be subtracted from velocity every frame.
    lda wind_l_MULTIPLIERS_LO,x   ;pythag_l_SQUARES_LO,x
    sta P0
    lda wind_l_MULTIPLIERS_HI,x   ;pythag_l_SQUARES_HI,x
    sta P1
    lda winslp_v_rot_vx_lo
    sta P2
    lda winslp_v_rot_vx_hi
    sta P3
    jsr maths_mul16s
    lda P4
    sta winslp_v_final_vx_lo
    lda P5
    sta winslp_v_final_vx_hi
    ; Need to negate result?
    lda SIGN_CHANGED
    beq +
    lda #<winslp_v_final_vx_lo
    sta P0
    lda #>winslp_v_final_vx_lo
    sta P1
    jsr maths_adjust_vec_signs
+
    ldx wind_v_index
    lda wind_l_MULTIPLIERS_LO,x   ;pythag_l_SQUARES_LO,x
    sta P0
    lda wind_l_MULTIPLIERS_HI,x   ;pythag_l_SQUARES_HI,x
    sta P1
    lda winslp_v_rot_vz_lo
    sta P2
    lda winslp_v_rot_vz_hi
    sta P3
    jsr maths_mul16s
    lda P4
    sta winslp_v_final_vz_lo
    lda P5
    sta winslp_v_final_vz_hi
    lda SIGN_CHANGED
    beq +
    lda #<winslp_v_final_vz_lo
    sta P0
    lda #>winslp_v_final_vz_lo
    sta P1
    jsr maths_adjust_vec_signs
+

    rts
; end sub wind_s_rotate
} ; !zone

; **************************************************
 
!zone {
wind_s_draw_speed
    ldy wind_v_index
    beq .end
    ldx wind_l_INDICATOR_BYTES_INDICES,y

    lda wind_c_INDICATOR_BYTES,x
    sta wind_c_INDICATOR_DEST
    sta wind_c_INDICATOR_DEST+1
    lda wind_c_INDICATOR_BYTES+1,x
    sta wind_c_INDICATOR_DEST+8
    sta wind_c_INDICATOR_DEST+9
    lda wind_c_INDICATOR_BYTES+2,x
    sta wind_c_INDICATOR_DEST+16
    sta wind_c_INDICATOR_DEST+17

.end
    rts
; end sub wind_s_draw_speed
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

wind_c_SIZE = *-wind_c_BEGIN

