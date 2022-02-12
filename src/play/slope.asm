; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


slope_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
;slope_l_TILES
;    !bin    "../../assets/tiles/slope_tiles.bin"
; NOTE: the tiles are arranged in pairs, therefore the offsets are multiples
; of 16 (bytes).
;slope_l_TILE_OFFSETS    !byte   0,16,32,48,64
;slope_c_TILES_DEST = gfxs_c_BITMAP_BASE+(19*40*8)+(7*8) 
slope_c_INDICATOR_DEST_BASE = gfxs_c_BITMAP_BASE+(19*40*8)+(1*8)

; Two lookup tables for masking random byte and adding to it (to get a valid
; index for the given difficulty setting).
slope_l_RANDOM_MASKS    !byte   $01,$01,$03
slope_l_RANDOM_ADDENDS  !byte   $00,$01,$01
; Sin of 0, 2, 4, 6 and 8 degrees.
;slope_l_SIN_THETA       !byte   0,8,17,26,35
; Sin of 0, 1.5, 3.0, 4.5 and 6.0 degrees.
slope_l_SIN_THETA       !byte   0,6,13,20,26

; Apply slope (to ball's velocity) each time this counts down to 0.
slope_c_DELAY = 5

slope_l_INDICATOR_BYTES
    !byte    7, %11010101
    !byte   15, %01010101
    !byte   23, %01010101

    !byte   14, %00000101
    !byte   22, %01010101

    !byte   14, %01010101
    !byte   21, %00010101

    !byte    6, %11000001
    !byte   13, %00000101
    !byte   20, %00010101
    !byte   21, %01010101

    !byte    6, %11000101
    !byte   12, %00000001
    !byte   13, %00010101
    !byte   19, %00000101
    !byte   20, %01010101
slope_l_INDICATOR_BYTE_END_OFFSETS
    !byte   3*2,5*2,7*2,11*2,16*2


; *****************
; *** VARIABLES ***
; *****************
slope_v_vx_lo       !byte   0
slope_v_vx_hi       !byte   0
slope_v_vz_lo       !byte   0
slope_v_vz_hi       !byte   0
slope_v_quadrant    !byte   0

; How steep is the slope?  A number in the range [0,5).
slope_v_index   !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
slope_s_rotate
    lda #<slope_v_vx_lo
    sta P0
    lda #>slope_v_vx_lo
    sta P1
    jsr winslp_s_rotate

    ldx slope_v_index
    lda slope_l_SIN_THETA,x
    sta P0
    lda #slope_c_DELAY
    sta P1
    jsr winslp_s_calc_final_vector

    rts
; end sub slope_s_rotate
} ; !zone

; **************************************************

;!zone {
;slope_s_draw_tiles
;    ldx slope_v_index
;    lda slope_l_TILE_OFFSETS,x
;    tax
;    ldy #0
;-
;    lda slope_l_TILES,x
;    sta slope_c_TILES_DEST,y
;    inx
;    iny
;    cpy #16
;    bne -
;
;    rts
;; end sub slope_s_draw_tiles
;} ; !zone

; **************************************************

!zone {
.ANGLE  = CAMERA1

slope_s_init
    jsr rand_s_get
    and #$3f
    sta MATHS4  ; angle for call to 'maths_s_rotate'

    lda #winslp_c_ARROW_LEN 
    sta slope_v_vx_lo
    lda #0
    sta slope_v_vx_hi
    sta slope_v_vz_lo
    sta slope_v_vz_hi

    lda #<slope_v_vx_lo
    sta MATHS0
    lda #>slope_v_vx_lo
    sta MATHS1
    lda #<slope_v_vz_lo
    sta MATHS2
    lda #>slope_v_vz_lo
    sta MATHS3
    clc
    jsr maths_s_rotate
 
    ; Select a random slope based on difficulty setting for greens.
    jsr rand_s_get
    ldx shared_v_greens_difficulty      
    and slope_l_RANDOM_MASKS,x
    clc
    adc slope_l_RANDOM_ADDENDS,x
    sta slope_v_index

    rts
; end sub slope_s_init
} ; !zone

; **************************************************

!zone {
.ONE_PAST_END = MATHS0

slope_s_draw
    ldx slope_v_index
    lda slope_l_INDICATOR_BYTE_END_OFFSETS,x
    sta .ONE_PAST_END

    ldx #0
-
    ldy slope_l_INDICATOR_BYTES,x
    lda slope_l_INDICATOR_BYTES+1,x
    sta slope_c_INDICATOR_DEST_BASE,y
    inx
    inx
    cpx .ONE_PAST_END
    bne -

    rts
; end sub slope_s_draw
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

slope_c_SIZE = *-slope_c_BEGIN

