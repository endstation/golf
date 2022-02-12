; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


winslp_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
winslp_l_BOX_ICON
    !bin    "../../assets/pictures/slope_box.bin"
winslp_c_ARROW_LEN = 96
winslp_c_LINE_CENTER_X = 10
winslp_c_LINE_CENTER_Y = 145


; *****************
; *** VARIABLES ***
; *****************
winslp_v_rot_vx_lo  !byte   0
winslp_v_rot_vx_hi  !byte   0
winslp_v_rot_vz_lo  !byte   0
winslp_v_rot_vz_hi  !byte   0

winslp_v_final_vx_lo    !byte   0
winslp_v_final_vx_hi    !byte   0
winslp_v_final_vz_lo    !byte   0
winslp_v_final_vz_hi    !byte   0

; Set 'delay' variable as part of setup routine.
winslp_v_delay  !byte   0
winslp_v_count  !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; FIXME: this is used only for slopes, not wind!!!
!zone {
winslp_s_apply
    dec winslp_v_count
    bne .end

    lda winslp_v_delay
    sta winslp_v_count

    lda ball_vx_lo
    clc
    adc winslp_v_final_vx_lo
    sta ball_vx_lo
    lda ball_vx_hi
    adc winslp_v_final_vx_hi
    sta ball_vx_hi
    lda ball_vz_lo
    clc
    adc winslp_v_final_vz_lo
    sta ball_vz_lo
    lda ball_vz_hi
    adc winslp_v_final_vz_hi
    sta ball_vz_hi

.end
    rts
; end sub winslp_s_apply
} ; !zone

; **************************************************

!zone {
winslp_s_draw_box
    lda #<winslp_l_BOX_ICON
    sta P0
    lda #>winslp_l_BOX_ICON
    sta P1
    lda #$ff
    sta P2
    jsr icon_s_draw
    rts
; end sub winslp_s_draw_box
} ; !zone

; **************************************************

; INPUTS:   P0=P1 = address of source vectors (set of 4 bytes).
!zone {
winslp_s_rotate
    ldy #3
-
    lda (P0),y
    sta winslp_v_rot_vx_lo,y
    dey
    bpl -

    lda #<winslp_v_rot_vx_lo
    sta MATHS0
    lda #>winslp_v_rot_vx_lo
    sta MATHS1
    lda #<winslp_v_rot_vz_lo
    sta MATHS2
    lda #>winslp_v_rot_vz_lo
    sta MATHS3
    lda hole_current_rotation_angle     
    sta MATHS4
    clc
    jsr maths_s_rotate

    rts
; end sub winslp_s_rotate
} ; !zone

; **************************************************

!zone {
.TOP_LEFT       = gfxs_c_BITMAP_BASE+(17*40*8)+8+1
.BOTTOM_LEFT    = gfxs_c_BITMAP_BASE+(18*40*8)+8
.TOP_RIGHT      = gfxs_c_BITMAP_BASE+(17*40*8)+(3*8)+1
.BOTTOM_RIGHT   = gfxs_c_BITMAP_BASE+(18*40*8)+(3*8)
.CENTER_DEST    = gfxs_c_BITMAP_BASE+(18*40*8)+(2*8)+1
.CENTER_PATTERN = %00001100

winslp_s_draw_direction
    lda winslp_v_rot_vx_lo
    bpl +
    +nega
    lsr
    lsr
    lsr
    lsr
    +nega
    jmp ++
+
    lsr
    lsr
    lsr
    lsr
++
    ; TODO: adjust if > 3?
    sta MATHS0

    lda winslp_v_rot_vz_lo
    bpl +
    +nega
    lsr
    lsr
    lsr
    lsr
    +nega
    jmp ++
+
    lsr
    lsr
    lsr
    lsr
++
    sta MATHS1

;    lda #31
    lda #winslp_c_LINE_CENTER_X 
    sta LINE_X0_LO
    clc
    adc MATHS0

;    ; FIXME/TODO: hack! Sometimes the line overdraws the box!
;    cmp #35
;    bcc +
;    lda #34
;+

    sta LINE_X1_LO
;    lda #168
    lda #winslp_c_LINE_CENTER_Y 
    sta LINE_Y0_LO
    sec
    sbc MATHS1
    sta LINE_Y1_LO
    ; NOTE: even though we're not going to use them, set destination for
    ; 'edges', otherwise they'll overwrite some random area of memory.
    lda #<quads_edges_from
    sta EDGES_LO
    lda #>quads_edges_from
    sta EDGES_HI

    lda #1
    ldy #0
    jsr dp_s_draw_line
 
    +utils_m_kernal_out
    ; Do potential repair here (?!)...
    ; NOTE: we are only modifying 7 bytes for each cell - omit top and
    ; bottom rows (of the 16 available).
    ldx #6
-
    lda .TOP_LEFT,x
    ; Orange 'margin' around box:
    and #%11001111
    ; Black border on lhs:
    ora #%11000000
    sta .TOP_LEFT,x
    lda .BOTTOM_LEFT,x
    and #%11001111
    ora #%11000000
    sta .BOTTOM_LEFT,x

    lda .TOP_RIGHT,x
    and #%11111100
    sta .TOP_RIGHT,x
    lda .BOTTOM_RIGHT,x
    and #%11111100
    sta .BOTTOM_RIGHT,x

    dex
    bpl -

    ; And finally draw the center point in BLACK.
    ; NOTE: because color code for BLACK is 11 (in binary), we can just ORA
    ; it into the bitmap - no need to mask out the pixel first!
    lda .CENTER_DEST
    ora #.CENTER_PATTERN
    sta .CENTER_DEST
    lda .CENTER_DEST+1
    ora #.CENTER_PATTERN
    sta .CENTER_DEST+1
    
    +utils_m_kernal_in

    ; And finally draw the center point in BLACK.
;    ldx #10
;    ldy #145
;    lda #3
;    jsr dp_s_draw_pixel
;    ldx #10
;    ldy #146
;    lda #3
;    jsr dp_s_draw_pixel

    rts
; end sub winslp_s_draw_direction
} ; !zone

; **************************************************

; NOTE: this is the vector we'll actually apply to the ball's velocity.
; Use this same routine for both the wind and slope 'final' vectors (since
; they're never used at the same time)...
; INPUTS:   P0 = multiplier (for scaling up the unit vector)
;           P1 = initial delay
!zone {
.MULTIPLICAND   = CAMERA0
.NEG            = CAMERA1

winslp_s_calc_final_vector
    lda P0
    sta .MULTIPLICAND
    lda P1
    sta winslp_v_delay  

    ; First get a unit vector (of the rotated vector) and then multiply by
    ; whatever's in .MULTIPLICAND.
    ldx #3
-
    lda winslp_v_rot_vx_lo,x
    sta winslp_v_final_vx_lo,x
    dex
    bpl -

    ; Divisions.
    +clr .NEG
    lda winslp_v_final_vx_hi
    bpl +
    +neg16 winslp_v_final_vx_lo
    inc .NEG 
+
    lda #0
    sta P0
    lda winslp_v_final_vx_lo
    sta P1
    lda #<winslp_c_ARROW_LEN
    sta P2
    lda #>winslp_c_ARROW_LEN
    sta P3
    jsr maths_div16
    ; Multiplier now already in P0-P1.
    lda .MULTIPLICAND
    sta P2
    lda #0
    sta P3
    jsr maths_mul16

    ; Use msb as low byte - discard low byte of result.
    lda P5
    sta winslp_v_final_vx_lo
    lda #0
    sta winslp_v_final_vx_hi
    ; Does final result need to be negated?
    lda .NEG
    beq +
    +neg16 winslp_v_final_vx_lo
+

    ; Now for vz.
    +clr .NEG
    lda winslp_v_final_vz_hi
    bpl +
    +neg16 winslp_v_final_vz_lo
    inc .NEG 
+
    lda #0
    sta P0
    lda winslp_v_final_vz_lo
    sta P1
    lda #<winslp_c_ARROW_LEN
    sta P2
    lda #>winslp_c_ARROW_LEN
    sta P3
    jsr maths_div16
    ; Multiplier now already in P0-P1.
    lda .MULTIPLICAND
    sta P2
    lda #0
    sta P3
    jsr maths_mul16
    ; Use msb as low byte - discard low byte of result.
    lda P5
    sta winslp_v_final_vz_lo
    lda #0
    sta winslp_v_final_vz_hi
    ; Does final result need to be negated?
    lda .NEG
    beq +
    +neg16 winslp_v_final_vz_lo
+

    ; FIXME: is this right?!
    lda winslp_v_delay  
    sta winslp_v_count

    rts
; end sub winslp_s_calc_final_vector
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

winslp_c_SIZE = *-winslp_c_BEGIN


