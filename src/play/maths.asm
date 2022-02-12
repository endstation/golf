; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


maths_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; Trig tables: tangent, sine and cosine.  Angles: [0, 2*PI) in (4*16=)64 steps.
; All values are signed 16-bit.  Sine and cosine to be interpreted as fractions
; of 256; tangent as integer (high byte) and fractional (low byte) parts.
maths_l_SIN_LO
    !byte   0,<25,<50,<74,<98,<121,<142,<162,<181,<198,<213,<226,<237,<245,<251,<255
    !byte   <256,<255,<251,<245,<237,<226,<213,<198,<181,<162,<142,<121,<98,<74,<50,<25
    !byte   0,<(-25),<(-50),<(-74),<(-98),<(-121),<(-142),<(-162),<(-181),<(-198),<(-213),<(-226),<(-237),<(-245),<(-251),<(-255)
    !byte   <(-256),<(-255),<(-251),<(-245),<(-237),<(-226),<(-213),<(-198),<(-181),<(-162),<(-142),<(-121),<(-98),<(-74),<(-50),<(-25)
maths_l_SIN_HI
    !byte   0,>25,>50,>74,>98,>121,>142,>162,>181,>198,>213,>226,>237,>245,>251,>255
    !byte   >256,>255,>251,>245,>237,>226,>213,>198,>181,>162,>142,>121,>98,>74,>50,>25
    !byte   0,>(-25),>(-50),>(-74),>(-98),>(-121),>(-142),>(-162),>(-181),>(-198),>(-213),>(-226),>(-237),>(-245),>(-251),>(-255)
    !byte   >(-256),>(-255),>(-251),>(-245),>(-237),>(-226),>(-213),>(-198),>(-181),>(-162),>(-142),>(-121),>(-98),>(-74),>(-50),>(-25)
maths_l_COS_LO
    !byte   <256,<255,<251,<245,<237,<226,<213,<198,<181,<162,<142,<121,<98,<74,<50,<25
    !byte   0,<(-25),<(-50),<(-74),<(-98),<(-121),<(-142),<(-162),<(-181),<(-198),<(-213),<(-226),<(-237),<(-245),<(-251),<(-255)
    !byte   <(-256),<(-255),<(-251),<(-245),<(-237),<(-226),<(-213),<(-198),<(-181),<(-162),<(-142),<(-121),<(-98),<(-74),<(-50),<(-25)
    !byte   0,<25,<50,<74,<98,<121,<142,<162,<181,<198,<213,<226,<237,<245,<251,<255
maths_l_COS_HI
    !byte   >256,>255,>251,>245,>237,>226,>213,>198,>181,>162,>142,>121,>98,>74,>50,>25
    !byte   0,>(-25),>(-50),>(-74),>(-98),>(-121),>(-142),>(-162),>(-181),>(-198),>(-213),>(-226),>(-237),>(-245),>(-251),>(-255)
    !byte   >(-256),>(-255),>(-251),>(-245),>(-237),>(-226),>(-213),>(-198),>(-181),>(-162),>(-142),>(-121),>(-98),>(-74),>(-50),>(-25)
    !byte   0,>25,>50,>74,>98,>121,>142,>162,>181,>198,>213,>226,>237,>245,>251,>255
maths_l_TAN_LO
    !byte   0,<25,<51,<78,<106,<137,<171,<210,<256,<312,<383,<479,<618,<844,<1287,<2599
    !byte   0,<(-2599),<(-1286),<(-844),<(-618),<(-479),<(-383),<(-312),<(-256),<(-210),<(-171),<(-137),<(-106),<(-78),<(-51),<(-25)
    !byte   0,<25,<51,<78,<106,<137,<171,<210,<256,<312,<383,<479,<618,<844,<1287,<2599
    !byte   0,<(-2599),<(-1286),<(-844),<(-618),<(-479),<(-383),<(-312),<(-256),<(-210),<(-171),<(-137),<(-106),<(-78),<(-51),<(-25)
maths_l_TAN_HI
    !byte   0,>25,>51,>78,>106,>137,>171,>210,>256,>312,>383,>479,>618,>844,>1287,>2599
    !byte   0,>(-2599),>(-1286),>(-844),>(-618),>(-479),>(-383),>(-312),>(-256),>(-210),>(-171),>(-137),>(-106),>(-78),>(-51),>(-25)
    !byte   0,>25,>51,>78,>106,>137,>171,>210,>256,>312,>383,>479,>618,>844,>1287,>2599
    !byte   0,>(-2599),>(-1286),>(-844),>(-618),>(-479),>(-383),>(-312),>(-256),>(-210),>(-171),>(-137),>(-106),>(-78),>(-51),>(-25)

; More fine-grained trig tables (sine and cosine only) for calculating the
; direction of the golf shot initially.  These actually run CLOCKWISE from
; 4/6*PI to 2/6*PI.
maths_l_FINE_COS_LO
    !byte   <221,<225,<229,<233,<236,<239,<242,<244,<247,<249,<251,<252,<253,<254,<255,<255
    !byte   <256
    !byte   <255,<255,<254,<253,<252,<251,<249,<247,<244,<242,<239,<236,<233,<229,<225,<221
maths_l_FINE_COS_HI
    !byte   >221,>225,>229,>233,>236,>239,>242,>244,>247,>249,>251,>252,>253,>254,>255,>255
    !byte   >256
    !byte   >255,>255,>254,>253,>252,>251,>249,>247,>244,>242,>239,>236,>233,>229,>225,>221
maths_l_FINE_SIN_LO
    !byte   <127,<120,<113,<105,<97,<90,<82,<74,<66,<58,<49,<41,<33,<25,<16,<8
    !byte   0
    !byte   <(-8),<(-16),<(-25),<(-33),<(-41),<(-49),<(-58),<(-66),<(-74),<(-82),<(-90),<(-97),<(-105),<(-113),<(-120),<(-127)
maths_l_FINE_SIN_HI
    !byte   >127,>120,>113,>105,>97,>90,>82,>74,>66,>58,>49,>41,>33,>25,>16,>8
    !byte   0
    !byte   >(-8),>(-16),>(-25),>(-33),>(-41),>(-49),>(-58),>(-66),>(-74),>(-82),>(-90),>(-97),>(-105),>(-113),>(-120),>(-127)

; NOTE: used by maths_s_atan2.
maths_l_TRIG_BASE_I  !byte   8,56,24,40
maths_l_MUST_NEGATE  !byte   0,1,1,0


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; 16-bit division - result includes remainder.
; All values are in typical 6502 lo/hi byte format.
; INPUT:    P0-P1 (dividend), P2-P3 (divisor)
; OUTPUT:   P0-P1 (result), P4-P5 (remainder)
; NOTE: this routine comes from codebase64.org!
; For your convenience:
DIVIDEND    = P0
DIVISOR     = P2
REMAINDER   = P4
RESULT      = DIVIDEND

!zone {
maths_div16
    lda #0
    sta REMAINDER
    sta REMAINDER+1
    ldx #16 ; because 16 bits!
.loop
    asl DIVIDEND
    rol DIVIDEND+1
    rol REMAINDER
    rol REMAINDER+1
    lda REMAINDER
    sec
    sbc DIVISOR
    tay
    lda REMAINDER+1
    sbc DIVISOR+1
    bcc +
    sta REMAINDER+1
    sty REMAINDER
    inc RESULT
+
    dex
    bne .loop
    rts
; end sub maths_div16
} ; !zone

; **************************************************

; 16-bit unsigned (?!) multiplication with 32-bit product.
; INPUT:    P0-P1 (multiplier), P2-P3 (multiplicand)
; OUTPUT:   P4-P7
; NOTE: this code comes from codebase64.org!
MULTIPLIER      = P0
MULTIPLICAND    = P2
PRODUCT         = P4

!zone {
maths_mul16
    lda #0
    sta PRODUCT+2 ; clear upper bits of product
    sta PRODUCT+3 
    ldx #16 ; set binary count to 16 
.shift_r 
    lsr MULTIPLIER+1 ; divide multiplier by 2 
    ror MULTIPLIER
    bcc .rotate_r 
    lda PRODUCT+2 ; get upper half of product and add multiplicand
    clc
    adc MULTIPLICAND
    sta PRODUCT+2
    lda PRODUCT+3 
    adc MULTIPLICAND+1
.rotate_r
    ror ; rotate partial product 
    sta PRODUCT+3 
    ror PRODUCT+2
    ror PRODUCT+1 
    ror PRODUCT 
    dex
    bne .shift_r 
    rts
; end sub maths_mul16
} ; !zone

; **************************************************

; INPUTS:   MATHS0-MATHS1 = x address, MATHS2-MATHS3 = z address,
;           MATHS4 = angle/index
;           C flag clear = ccw rotation, C flag set = cw rotation
!zone {
xcos_lo    !byte   0
xcos_hi    !byte   0
xsin_lo    !byte   0
xsin_hi    !byte   0
zcos_lo    !byte   0
zcos_hi    !byte   0
zsin_lo    !byte   0
zsin_hi    !byte   0

maths_s_rotate
    ; NOTE: we use the same pair of equations for both ccw and cw rotation.
    ; If cw rotation is required, adjust the index here before continuing.
    ; For example, an index of 4 would become (64-4=)60.
    bcc +
    lda #64
    sec
    sbc MATHS4
    sta MATHS4

+
    ; x * cos:
    ldx MATHS4
    ldy #0
    lda (MATHS0),y
    sta P0
    iny
    lda (MATHS0),y
    sta P1
maths_mod0
    lda maths_l_COS_LO,x
    sta P2
maths_mod1
    lda maths_l_COS_HI,x
    sta P3
    jsr maths_mul16s
    ; Dividing by 256, so result is in P5-P6.
    lda SIGN_CHANGED
    +branch_if_false +
    +neg16 P5
+   lda P5
    sta xcos_lo
    lda P6
    sta xcos_hi
    
    ; x * sin:
    ldx MATHS4
    ldy #0
    lda (MATHS0),y
    sta P0
    iny
    lda (MATHS0),y
    sta P1
maths_mod2
    lda maths_l_SIN_LO,x
    sta P2
maths_mod3
    lda maths_l_SIN_HI,x
    sta P3
    jsr maths_mul16s
    lda SIGN_CHANGED
    +branch_if_false +
    +neg16 P5
+   lda P5
    sta xsin_lo
    lda P6
    sta xsin_hi

    ; z * cos:
    ldx MATHS4
    ldy #0
    lda (MATHS2),y
    sta P0
    iny
    lda (MATHS2),y
    sta P1
maths_mod4
    lda maths_l_COS_LO,x
    sta P2
maths_mod5
    lda maths_l_COS_HI,x
    sta P3
    jsr maths_mul16s
    lda SIGN_CHANGED
    +branch_if_false +
    +neg16 P5
+   lda P5
    sta zcos_lo
    lda P6
    sta zcos_hi

    ; z * sin:
    ldx MATHS4
    ldy #0
    lda (MATHS2),y
    sta P0
    iny
    lda (MATHS2),y
    sta P1
maths_mod6
    lda maths_l_SIN_LO,x
    sta P2
maths_mod7
    lda maths_l_SIN_HI,x
    sta P3
    jsr maths_mul16s
    lda SIGN_CHANGED
    +branch_if_false +
    +neg16 P5
+   lda P5
    sta zsin_lo
    lda P6
    sta zsin_hi

    ; x' = x*cos - z*sin
    ldy #0
    lda xcos_lo
    sec
    sbc zsin_lo
    sta (MATHS0),y
    iny
    lda xcos_hi
    sbc zsin_hi
    sta (MATHS0),y

    ; z' = x*sin + z*cos
    dey ; back to zero!
    lda xsin_lo
    clc
    adc zcos_lo
    sta (MATHS2),y
    iny
    lda xsin_hi
    adc zcos_hi
    sta (MATHS2),y

    rts
; end sub maths_s_rotate
} ; !zone

; **************************************************

; Change sign of a 16-bit value.
; INPUT:    P0-P1 (low byte first)
!zone {
maths_adjust_vec_signs
    ; EOR high byte first so low byte ready for addition.
    ldy #1
    lda (P0),y
    eor #$ff
    pha
    dey
    lda (P0),y
    eor #$ff
    clc
    adc #1
    sta (P0),y
    iny
    pla
    adc #0
    sta (P0),y
    rts
; end sub maths_adjust_vec_signs
} ; !zone

; **************************************************

; INPUTS:   MATHS0-MATHS1 = x, MATHS2-MATHS3 = z (16-bit signed integers).
; OUTPUT:   X = index into trig tables.
!zone {
.base_i !byte   0
.ITER           = MATHS0
.DELTA          = MATHS1
.BEST_DIFF_LO   = MATHS2
.BEST_DIFF_HI   = MATHS3
.DIFF_LO        = MATHS4
.DIFF_HI        = MATHS5
.BEST_I         = MATHS6

maths_s_atan2
    +bne16 MATHS0,.div_defined

    ; x-component of vector is zero so we need to avoid division by zero.
    bit MATHS3
    bpl .twelve_o_clock
    ; So six o' clock...
    ldx #(3*16)
    +skip_2_bytes 
.twelve_o_clock
    ldx #16
    rts ; EXIT POINT.

.div_defined
    ; STEP 1:
    ; Get absolute values of x and z (where necessary) and record quadrant.
    ; (- actually, index into maths_l_TRIG_BASE_I)...
    ldx #0
    bit MATHS1
    bpl +
    inx ; add 2 to 'index' if x is negative
    inx
    +neg16 MATHS0
+
    bit MATHS3
    bpl +
    inx ; add 1 if z negative
    +neg16 MATHS2
+
    stx .base_i

    ; STEP 2:
    ; Prepare vector components for division.  We now have two positive 16-bit
    ; integers.  Want to do: (z*256)/x... but may need to scale down x and z
    ; until z fits into two bytes.
    ; NOTE: use MATHS4 as low byte of z.
    lda #0
    sta MATHS4
-
    ; (Previous) high byte of z must be 0... and new high byte should be +ve.
    lda MATHS3
    bne .shift_right
    lda MATHS2
    bpl .ready_for_division
.shift_right
    lsr MATHS3
    ror MATHS2
    ror MATHS4
    lsr MATHS1
    ror MATHS0
    jmp -

.ready_for_division
    ; BUG: what if denominator has become 0?
    +bne16 MATHS0,+
    ; Put 'infinity' in P0-P1 and skip the division.
    lda #<3000
    sta P0
    lda #>3000
    sta P1
    bne .skip_division
+
    ; STEP 3:
    ; Do the division and negate result if necessary.
    lda MATHS4
    sta P0
    lda MATHS2
    sta P1
    lda MATHS0
    sta P2
    lda MATHS1
    sta P3
    jsr maths_div16
.skip_division
    ; Result in P0-P1.
    ldx .base_i
    lda maths_l_MUST_NEGATE,x
    +branch_if_false +
    +neg16 P0

+
    ; STEP 4:
    ; Lookup best match (index) in TAN table.
    ; Initialize variables.  X still holds 'base' index...
    lda maths_l_TRIG_BASE_I,x
    sta .ITER
    sta .BEST_I
    tax
    lda #8
    sta .DELTA
    ; Put highest possible absolute value into BEST_DIFF_LO/HI.
    lda #$ff
    sta .BEST_DIFF_LO
    sta .BEST_DIFF_HI

.loop_top
    lda P0
    sec
    sbc maths_l_TAN_LO,x
    sta .DIFF_LO
    lda P1
    sbc maths_l_TAN_HI,x
    sta .DIFF_HI
    php ; save results for later
    ; Negate difference so it's positive (if necessary).
    bpl +
    +neg16 .DIFF_LO
+
    ; Do unsigned comparison of current diff with best diff. 
    lda .BEST_DIFF_LO
    cmp .DIFF_LO
    lda .BEST_DIFF_HI
    sbc .DIFF_HI
    bcc .next
    ; Record new best.
    lda .DIFF_LO
    sta .BEST_DIFF_LO
    lda .DIFF_HI
    sta .BEST_DIFF_HI
    stx .BEST_I

.next
    lsr .DELTA
    beq .done
    ; Get result of subtraction off of stack - adjust .ITER up or down
    ; accordingly.  Remember the TAN table is always ascending!
    plp
    +blt_s +
    lda .ITER
    clc
    adc .DELTA
    ; NOTE: this will never be zero!
    bne ++
+
    lda .ITER
    sec
    sbc .DELTA
++
    sta .ITER

    tax
    jmp .loop_top

.done
    ; NOTE: remember to clean up!!!
    plp
    ldx .BEST_I
    rts
; end sub math_s_atan2
} ; !zone

; **************************************************

; 16-bit signed (!) multiplication with 32-bit product.
; INPUT:    P0-P1 (multiplier), P2-P3 (multiplicand),
;           SIGN_BIT_MASK (which bit should hold neg. situation?)
; OUTPUT:   P4-P7, SIGN_CHANGED (non-zero if result should be negative)
; NOTE: this code comes from codebase64.org!
MULTIPLIER      = P0
MULTIPLICAND    = P2
PRODUCT         = P4

!zone {
maths_mul16s
    jsr maths_s_check_operand_signs

    lda #0
    sta PRODUCT+2 ; clear upper bits of product
    sta PRODUCT+3 
    ldx #16 ; set binary count to 16 
.shift_r 
    lsr MULTIPLIER+1 ; divide multiplier by 2 
    ror MULTIPLIER
    bcc .rotate_r 
    lda PRODUCT+2 ; get upper half of product and add multiplicand
    clc
    adc MULTIPLICAND
    sta PRODUCT+2
    lda PRODUCT+3 
    adc MULTIPLICAND+1
.rotate_r
    ror ; rotate partial product 
    sta PRODUCT+3 
    ror PRODUCT+2
    ror PRODUCT+1 
    ror PRODUCT 
    dex
    bne .shift_r 
    rts
; end sub maths_mul16s
} ; !zone

; **************************************************

; 16-bit signed division - result includes remainder.
; All values are in typical 6502 lo/hi byte format.
; INPUT:    P0-P1 (dividend), P2-P3 (divisor)
; OUTPUT:   P0-P1 (result), P4-P5 (remainder), SIGN_CHANGED.
DIVIDEND    = P0
DIVISOR     = P2
REMAINDER   = P4
RESULT      = DIVIDEND

!zone {
maths_div16s
    jsr maths_s_check_operand_signs

    lda #0
    sta REMAINDER
    sta REMAINDER+1
    ldx #16 ; because 16 bits!
.loop
    asl DIVIDEND
    rol DIVIDEND+1
    rol REMAINDER
    rol REMAINDER+1
    lda REMAINDER
    sec
    sbc DIVISOR
    tay
    lda REMAINDER+1
    sbc DIVISOR+1
    bcc +
    sta REMAINDER+1
    sty REMAINDER
    inc RESULT
+
    dex
    bne .loop
    rts
; end sub maths_div16s
} ; !zone

; **************************************************

;; INPUTS:   P0-P1 = ax, P2-P3 = az, P4-P5 = bx, P6-P7 = bz
;; OUTPUTS:  MATHS0-MATHS1 = dot product.
;!zone {
;.TERM0_NEG = CAMERA0
;.TERM1_NEG = CAMERA1
;.ax_lo  !byte   0
;.ax_hi  !byte   0
;.az_lo  !byte   0
;.az_hi  !byte   0
;.bx_lo  !byte   0
;.bx_hi  !byte   0
;.bz_lo  !byte   0
;.bz_hi  !byte   0
;.ax_times_bx_lo !byte   0
;.ax_times_bx_hi !byte   0
;.az_times_bz_lo !byte   0
;.az_times_bz_hi !byte   0
;
;maths_s_vector_dot_product
;    ; Copy input values into local variables.
;    lda P0
;    sta .ax_lo
;    lda P1
;    sta .ax_hi
;    lda P2
;    sta .az_lo
;    lda P3
;    sta .az_hi
;    lda P4
;    sta .bx_lo
;    lda P5
;    sta .bx_hi
;    lda P6
;    sta .bz_lo
;    lda P7
;    sta .bz_hi
;
;    ; ax*bx
;    ; 16-bit multiply & store result.
;    lda .ax_lo
;    sta P0
;    lda .ax_hi
;    sta P1
;    lda .bx_lo
;    sta P2
;    lda .bx_hi
;    sta P3
;    jsr maths_mul16s
;    lda P5
;    sta .ax_times_bx_lo
;    lda P6
;    sta .ax_times_bx_hi
;    lda SIGN_CHANGED
;    sta .TERM0_NEG
;    
;    ; az*bz
;    ; 16-bit multiply & store result.
;    lda .az_lo
;    sta P0
;    lda .az_hi
;    sta P1
;    lda .bz_lo
;    sta P2
;    lda .bz_hi
;    sta P3
;    jsr maths_mul16s
;    lda P5
;    sta .az_times_bz_lo
;    lda P6
;    sta .az_times_bz_hi
;    lda SIGN_CHANGED
;    sta .TERM1_NEG
;
;    ; FIXME: use macro for this?
;    ; Does either term need to be negated?
;    lda .TERM0_NEG
;    beq +
;    lda #<.ax_times_bx_lo
;    sta P0
;    lda #>.ax_times_bx_lo
;    sta P1
;    jsr maths_adjust_vec_signs
;+
;    lda .TERM1_NEG
;    beq +
;    lda #<.az_times_bz_lo
;    sta P0
;    lda #>.az_times_bz_lo
;    sta P1
;    jsr maths_adjust_vec_signs
;+
;
;    ; Do the sum and put result in MATHS0-MATHS1.
;    lda .ax_times_bx_lo
;    clc
;    adc .az_times_bz_lo
;    sta MATHS0
;    lda .ax_times_bx_hi
;    adc .az_times_bz_hi
;    sta MATHS1
;
;    rts
;; end sub maths_s_vector_dot_product
;} ; !zone

; **************************************************

; INPUTS:   P0-P1 = lhs, P2-P3 = rhs
; OUTPUTS:  sets SIGN_CHANGED as appropriate.
!zone {
maths_s_check_operand_signs
    lda #0
    sta SIGN_CHANGED

    ; Check for negative operands and take 2's comp if necessary.
    ; Record if result should be negative.
    ; LHS:
    lda P1
    bpl +
    +neg16 P0
;    lda #SIGN_BIT_MASK
;    eor SIGN_CHANGED
;    sta SIGN_CHANGED
    lda SIGN_CHANGED
    eor #$01
    sta SIGN_CHANGED
+
    ; RHS:
    lda P3
    bpl +
    +neg16 P2
;    lda #SIGN_BIT_MASK
;    eor SIGN_CHANGED
;    sta SIGN_CHANGED
    lda SIGN_CHANGED
    eor #$01
    sta SIGN_CHANGED
+

    rts
; end sub maths_s_check_operand_signs
} ; !zone

; **************************************************

!zone {
maths_s_install_fine_trig
    lda #<maths_l_FINE_COS_LO
    sta maths_mod0+1
    sta maths_mod4+1
    lda #>maths_l_FINE_COS_LO
    sta maths_mod0+2
    sta maths_mod4+2
    lda #<maths_l_FINE_COS_HI
    sta maths_mod1+1
    sta maths_mod5+1
    lda #>maths_l_FINE_COS_HI
    sta maths_mod1+2
    sta maths_mod5+2
    lda #<maths_l_FINE_SIN_LO
    sta maths_mod2+1
    sta maths_mod6+1
    lda #>maths_l_FINE_SIN_LO
    sta maths_mod2+2
    sta maths_mod6+2
    lda #<maths_l_FINE_SIN_HI
    sta maths_mod3+1
    sta maths_mod7+1
    lda #>maths_l_FINE_SIN_HI
    sta maths_mod3+2
    sta maths_mod7+2
    rts
; end sub maths_s_install_fine_trig
} ; !zone

; **************************************************

!zone {
maths_s_restore_std_trig
    lda #<maths_l_COS_LO
    sta maths_mod0+1
    sta maths_mod4+1
    lda #>maths_l_COS_LO
    sta maths_mod0+2
    sta maths_mod4+2
    lda #<maths_l_COS_HI
    sta maths_mod1+1
    sta maths_mod5+1
    lda #>maths_l_COS_HI
    sta maths_mod1+2
    sta maths_mod5+2
    lda #<maths_l_SIN_LO
    sta maths_mod2+1
    sta maths_mod6+1
    lda #>maths_l_SIN_LO
    sta maths_mod2+2
    sta maths_mod6+2
    lda #<maths_l_SIN_HI
    sta maths_mod3+1
    sta maths_mod7+1
    lda #>maths_l_SIN_HI
    sta maths_mod3+2
    sta maths_mod7+2
    rts
; end sub maths_s_restore_std_trig
} ; !zone

; **************************************************

; NOTE: you must write the values of x-lo/hi and z-lo/hi; then the addresses
;       of x-lo and z-lo into the routine before calling it.
; INPUTS:   C flag set = do rotation; C flag clear = quit before rotation
; OUTPUT:   angle in accumulator if not doing rotation
!zone {
.DO_ROTATE = FADE_CR_LO

maths_s_rotate_vec_to_pos_z_axis
    ldx #0
    bcc +
    inx
+
    stx .DO_ROTATE

maths_mod08 = *+1
    lda #$ff    ; x-lo
    sta MATHS0
maths_mod09 = *+1
    lda #$ff    ; x-hi
    sta MATHS1
maths_mod10 = *+1
    lda #$ff    ; z-lo
    sta MATHS2
maths_mod11 = *+1
    lda #$ff    ; z-hi
    sta MATHS3
    jsr maths_s_atan2
    ; Result is in X.
    stx MATHS0
    ; NOTE: our target trig index is 16 (= 12 o' clock).  So we'll do 
    ; 16 - [current index].  If difference is negative, add 64 to get (in
    ; either case) a counter-clockwise rotation.
    lda #16
    sec
    sbc MATHS0
    bpl +
    clc
    adc #64
+   

    ; Are we going to rotate?
    ldx .DO_ROTATE
    +branch_if_true .rotate
    rts ; EXIT POINT.

.rotate
    sta MATHS4

maths_mod12 = *+1
    lda #$ff    ; <x-lo
    sta MATHS0
maths_mod13 = *+1
    lda #$ff    ; >x-lo
    sta MATHS1
maths_mod14 = *+1
    lda #$ff    ; <z-lo
    sta MATHS2
maths_mod15 = *+1
    lda #$ff    ; >z-lo
    sta MATHS3
    clc
    jsr maths_s_rotate

    rts
; end sub maths_s_rotate_vec_to_pos_z_axis
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

maths_c_SIZE = *-maths_c_BEGIN
