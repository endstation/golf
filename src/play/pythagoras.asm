; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


pythag_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
pythag_l_SQUARES_LO
!for i,64 {
    !byte   <((i-1)*(i-1))
} ; !for
pythag_l_SQUARES_HI
!for i,64 {
    !byte   >((i-1)*(i-1))
} ; !for

; NOTE: to be interpreted as a fraction of 2^16.
pythag_c_DRAG_COEFFICIENT = 64


; *****************
; *** VARIABLES ***
; *****************
pythag_v_speed_squared_lo   !byte   0
pythag_v_speed_squared_hi   !byte   0
pythag_v_speed              !byte   0
pythag_v_drag_force_lo      !byte   0
pythag_v_drag_force_hi      !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; NOTE: need a generic routine to calculate vector magnitude.
; NOTE: values of P0-P2 are not modified!
; INPUTS:   P0-P2=vector components (positive 8-bit values).
; OUTPUTS:  MATHS0-MATHS1=v^2, X=v
!zone {
.DELTA              = MATHS2
.CURRENT_DIFF_LO    = MATHS3
.CURRENT_DIFF_HI    = MATHS4
.LOWEST_DIFF_LO     = MATHS5
.LOWEST_DIFF_HI     = MATHS6
.BEST_INDEX         = MATHS7
.ITER   !byte   0

pythag_s_calc_magnitude
    lda #$0f
    sta .LOWEST_DIFF_LO
    sta .LOWEST_DIFF_HI

    ; Put sum of squares into MATHS0-MATHS1.
    lda P0
    bpl +
    +nega
+   
    tax
    lda pythag_l_SQUARES_LO,x
    sta MATHS0
    lda pythag_l_SQUARES_HI,x
    sta MATHS1

    lda P1
    bpl +
    +nega
+
    tax
    lda pythag_l_SQUARES_LO,x
    clc
    adc MATHS0
    sta MATHS0
    lda pythag_l_SQUARES_HI,x
    adc MATHS1
    sta MATHS1

    lda P2
    bpl +
    +nega
+
    tax
    lda pythag_l_SQUARES_LO,x
    clc
    adc MATHS0
    sta MATHS0
    lda pythag_l_SQUARES_HI,x
    adc MATHS1
    sta MATHS1

    ; Now we'll look up the root.
    ; X will be index into table.
    ; .DELTA is what we add to (or subtract from) index each time round.
    lda #16
    sta .DELTA
    ldx #32
    stx .BEST_INDEX
.loop_top
    lda MATHS0
    sec
    sbc pythag_l_SQUARES_LO,x
    sta .CURRENT_DIFF_LO
    lda MATHS1
    sbc pythag_l_SQUARES_HI,x
    sta .CURRENT_DIFF_HI

    ; Check if we have a new 'lowest diff'.
    php
    bpl .current_positive
    ; Current diff is negative so add lowest diff.  If sum is positive, 
    ; that's a new best.
    lda .CURRENT_DIFF_LO
    clc
    adc .LOWEST_DIFF_LO
    lda .CURRENT_DIFF_HI
    adc .LOWEST_DIFF_HI
    bpl .new_best
    jmp .finished_check
.current_positive
    ; Subtract lowest diff.  If difference is negative, new best.
    lda .CURRENT_DIFF_LO
    sec
    sbc .LOWEST_DIFF_LO
    lda .CURRENT_DIFF_HI
    sbc .LOWEST_DIFF_HI
    bmi .new_best
    jmp .finished_check
.new_best
    lda .CURRENT_DIFF_LO
    sta .LOWEST_DIFF_LO
    lda .CURRENT_DIFF_HI
    sta .LOWEST_DIFF_HI
    bpl +
    +neg16 .LOWEST_DIFF_LO
+
    stx .BEST_INDEX
.finished_check
    plp

    ; NOTE: a check for equality here won't work - only acts on high byte.
    bcs .greater_than
    ; So less than:
    ; Adjust index down (i.e. towards beginning of table).
    txa
    sec
    sbc .DELTA
    tax
    bne +
.greater_than
    txa
    clc
    adc .DELTA
    tax
+
    ; FIXME: should do this first!
    ; If .DELTA is 0, there's no more processing to do (- we just checked
    ; the last index).
    lda .DELTA
    beq .ok
    lsr .DELTA
    jmp .loop_top
.ok
    ; Put the closest integer match in X.
    ldx .BEST_INDEX

    rts
; end sub pythag_s_calc_magnitude
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
; **************************************************
; **************************************************
; **************************************************

pythag_c_SIZE = *-pythag_c_BEGIN
