; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
SHAKER_STEPS = 5
SHAKER_DY   !byte   2,5,3,4,3


; *****************
; *** VARIABLES ***
; *****************
shaker_active   !byte   0
shaker_iter     !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
shaker_init
    ; Initialize only if user selected 'camera shake ON'.
    lda round_camera_shake_enabled
    +branch_if_false .end

    lda golfer_v_shot_power
    beq .max
    cmp #128
    bcs .max
    cmp #64
    bcc .end    ; Shot too weak - no camera shake.

    ; Medium shake.
    ldx #2
    jmp +
.max
    ldx #0
+   stx shaker_iter

    lda SCROLY
    and #$f8
    ora SHAKER_DY,x
    sta SCROLY
    lda #1
    sta shaker_active

.end
    rts
; end sub shaker_init
} ; !zone

; **************************************************

!zone {
shaker_update
    lda shaker_active
    +branch_if_false .end

    ldx shaker_iter
    inx
    cpx #SHAKER_STEPS
    beq .deactivate
    stx shaker_iter

    lda SCROLY
    and #$f8
    ora SHAKER_DY,x
    sta SCROLY
    rts ; EXIT POINT.

.deactivate
    lda #0
    sta shaker_active

.end
    rts
; end sub shaker_update
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

