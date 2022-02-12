; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; NOTE: Inputs are two 3D vectors, representing ball velocity and spin axis.
; Spin axis is a unit vector, with each component stored as a fraction of
; 256.  Velocity vector has 16-bit components, an integer (high byte) and a
; fractional (low byte) part.
; Access to ball velocity is through ball_v_mutex_vx_lo, etc.  Spin axis never
; changes during flight so it's OK to use those variables directly.


fmag_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
fmag_c_X_MASK = %0001
fmag_c_Y_MASK = %0010
fmag_c_Z_MASK = %0100


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************
!macro fmag_record_neg .addr, .mask {
    lda .addr
    eor #.mask
    sta .addr
} ; fmag_record_neg


; *******************
; *** SUBROUTINES ***
; *******************
; OUTPUT:   unit vector on which magnus force should be applied.  Stored in
;           ball_v_mutex_spin_x_lo, etc.
;           X = 'sign change'
!zone {
; Record here, for each component, whether sign needs changing (back) after
; normalization.  (See below for masks.)
.sign_change    !byte   0
.ITER = MATHS3
.SPIN_X_LO = LINE_X0_LO
.SPIN_X_HI = LINE_Y0_LO
.SPIN_Z_LO = LINE_X1_LO
.SPIN_Z_HI = LINE_Y1_LO

fmag_s_cross_product
    ; Look up spin axis and store in .SPIN_X_LO, etc.
    lda ball_v_spin_axis_x_lo
    sta .SPIN_X_LO
    lda ball_v_spin_axis_x_hi
    sta .SPIN_X_HI
    lda ball_v_spin_axis_z_lo
    sta .SPIN_Z_LO
    lda ball_v_spin_axis_z_hi
    sta .SPIN_Z_HI

    ; Clear this out first!
    lda #0
    sta .sign_change

    ; norm_x = vy*sz
    lda ball_v_mutex_vy_lo
    sta P0
    lda ball_v_mutex_vy_hi
    sta P1
    bpl +
    +neg16 P0
    +fmag_record_neg .sign_change,fmag_c_X_MASK
+
    lda .SPIN_Z_LO ;spin_z_lo
    sta P2
    lda .SPIN_Z_HI ;spin_z_hi
    sta P3
    bpl +
    +neg16 P2
    +fmag_record_neg .sign_change,fmag_c_X_MASK
+
    jsr maths_mul16

    ; That was an 8-bit number multiplied by a 16-bit number, so result should
    ; fit into 3 bytes.  Divide result by 256 - this will give us a 16-bit 
    ; value that should be interpreted as having integer (hi) and fractional
    ; (lo) parts.
    lda P5
    sta ball_v_mutex_spin_x_lo  ;norm_x_lo
    lda P6
    sta ball_v_mutex_spin_x_hi  ;norm_x_hi
    
    ; norm_y = vz*sx - vx*sz
    lda ball_v_mutex_vz_lo
    sta P0
    lda ball_v_mutex_vz_hi
    sta P1
    bpl +
    +neg16 P0
    +fmag_record_neg .sign_change,fmag_c_Y_MASK
+
    lda .SPIN_X_LO ;spin_x_lo
    sta P2
    lda .SPIN_X_HI ;spin_x_hi
    sta P3
    bpl +
    +neg16 P2
    +fmag_record_neg .sign_change,fmag_c_Y_MASK
+
    jsr maths_mul16
    ; Put result in MATHS0-MATHS2 temporarily.  (We don't need the
    ; 4th byte.)
    lda P4
    sta MATHS0
    lda P5
    sta MATHS1
    lda P6
    sta MATHS2
    ; If sign needs changing (because one of vz and sx was negative) change
    ; it now before subtraction.  NOTE: 24-bit value!
    lda .sign_change
    and #fmag_c_Y_MASK
    beq +   ; OK.
    +neg24 MATHS0
    ; Reset 'fmag_c_Y_MASK' bit to 0.
    +fmag_record_neg .sign_change,fmag_c_Y_MASK
+
    ; Now vx*sz.
    lda ball_v_mutex_vx_lo
    sta P0
    lda ball_v_mutex_vx_hi
    sta P1
    bpl +
    +neg16 P0
    +fmag_record_neg .sign_change,fmag_c_Y_MASK
+
    lda .SPIN_Z_LO ;spin_z_lo
    sta P2
    lda .SPIN_Z_HI ;spin_z_hi
    sta P3
    bpl +
    +neg16 P2
    +fmag_record_neg .sign_change,fmag_c_Y_MASK
+
    jsr maths_mul16
    ; Again, change sign of product if necessary before subtraction.
    ; Product is in P4-P6 (- we don't need the high byte).
    lda .sign_change
    and #fmag_c_Y_MASK
    beq +
    +neg24 P4
    ; Clear out 'fmag_c_Y_MASK' bit - may need it later...
    +fmag_record_neg .sign_change,fmag_c_Y_MASK
+
    ; Now a 24-bit subtraction.  Don't bother storing the low byte because
    ; we're multiplying by 256.
    lda MATHS0
    sec
    sbc P4
    lda MATHS1
    sbc P5
    sta ball_v_mutex_spin_y_lo  ;norm_y_lo
    lda MATHS2
    sbc P6
    sta ball_v_mutex_spin_y_hi  ;norm_y_hi
    ; Need to do a division later (to find unit vector) which will require
    ; positive values.  So if difference was negative, make it positive here
    ; and record change made.
    bpl +
    +neg16 ball_v_mutex_spin_y_lo   ;norm_y_lo
    +fmag_record_neg .sign_change,fmag_c_Y_MASK
+
    
    ; 0 - vysx.
    ; This could be either positive or negative.
    ; NOTE: SET AS NEGATIVE TO BEGIN WITH!!!
    +fmag_record_neg .sign_change,fmag_c_Z_MASK
    lda ball_v_mutex_vy_lo
    sta P0
    lda ball_v_mutex_vy_hi
    sta P1
    bpl +
    +neg16 P0
    +fmag_record_neg .sign_change,fmag_c_Z_MASK
+
    lda .SPIN_X_LO ;spin_x_lo
    sta P2
    lda .SPIN_X_HI ;spin_x_hi
    sta P3
    bpl +
    +neg16 P2
    +fmag_record_neg .sign_change,fmag_c_Z_MASK
+
    jsr maths_mul16
    lda P5
    sta ball_v_mutex_spin_z_lo  ;norm_z_lo
    lda P6
    sta ball_v_mutex_spin_z_hi  ;norm_z_hi

    ; So now we have a normal vector, stored in ball_v_mutex_spin_x_lo, etc.
    ; All values should (?!) be positive, but we've recorded (in .sign_change)
    ; any components that need to be negated before use.
    ; But still need to make it a unit vector.  First find its approximate
    ; length.
    lda ball_v_mutex_spin_x_hi
    sta P0
    lda ball_v_mutex_spin_y_hi
    sta P1
    lda ball_v_mutex_spin_z_hi
    sta P2
    jsr pythag_s_calc_magnitude

    ; |v^2| is in MATHS0-MATHS1; |v| is in X.
    stx MATHS2
    ; Divide each component of normal by MATHS2 (which holds approximate length 
    ; of normal).  Interpret the result as a fraction of 256.  If result is
    ; >255, clamp it to 255.  Store results back into norm_x_lo, etc., 
    ; always setting high byte to zero.

    ldx #0
.loop
    stx .ITER
    lda ball_v_mutex_spin_x_lo,x    ;norm_x_lo,x
    sta P0
    lda ball_v_mutex_spin_x_hi,x    ;norm_x_lo+1,x
    sta P1
    lda MATHS2
    sta P2
    lda #0
    sta P3
    jsr maths_div16
    ; FIXME: what if it's >16-bit?  Is this possible?!
    lda P1
    beq +
    ; Result is >255 so clamp to 255.
    lda #$ff
    bne ++
+
    lda P0
++
    ldx .ITER
    sta ball_v_mutex_spin_x_lo,x    ;norm_x_lo,x
    lda #0
    sta ball_v_mutex_spin_x_hi,x

    ; Increment twice because vectors are stored lo/hi, lo/hi, etc.
    inx
    inx
    cpx #6
    bne .loop
;    beq .done
;    stx .ITER
;    bne .loop

.done
    ; If 'powarc_precision_offset' is set to hook (i.e. in range [9,15]), 
    ; must negate x-component of spin vector.  Record that here if necessary.
;    lda powarc_v_precision_offset
;    cmp #POWARC_PRECISION_BEGIN_STEP
;    bcc +
;    cmp #POWARC_PRECISION_MID_STEP
;    bcs +
;    ; So must negate x-component.
;    +fmag_record_neg .sign_change,fmag_c_X_MASK
;+
;    ldx .sign_change

    ; If the shot is a hook, each component of the spin vector must be negated.
    ; Obviously we will not change the actual vector here, but just the 
    ; '.sign_change' record.
    lda ball_v_spin_type
    cmp #ball_c_SPIN_TYPE_HOOK
    bne +
    lda .sign_change
    eor #%111
    sta .sign_change
+
    ldx .sign_change
    rts
; end sub fmag_s_cross_product
} ; !zone

; **************************************************

!zone {
fmag_calc_force
    rts
; end sub fmag_calc_force
} ; !zone

; **************************************************

;!zone {
;.counter    !byte   0
;
;fmag_run_test
;    lda #<fmag_test_data
;    sta BITMAP_LO
;    lda #>fmag_test_data
;    sta BITMAP_HI
;    lda #<fmag_test_data
;    sta TREES_LO
;    lda #>fmag_test_data
;    sta TREES_HI
;
;.loop_top
;    ; Load input data.
;    ldy #0
;-
;    lda (BITMAP_LO),y
;    sta ball_v_mutex_vx_lo,y
;    iny
;    cpy #6
;    bne -
;
;    lda (BITMAP_LO),y
;    sta spin_x_lo
;    iny
;    lda (BITMAP_LO),y
;    sta spin_x_hi
;    iny
;    iny
;    iny
;    lda (BITMAP_LO),y
;    sta spin_z_lo
;    iny 
;    lda (BITMAP_LO),y
;    sta spin_z_hi
;    
;    jsr fmag_cross_product
;    ; Store results.
;    ldy #0
;-
;    lda norm_x_lo,y
;    sta (TREES_LO),y
;    iny
;    cpy #6
;    bne -
;    lda sign_change
;    sta (TREES_LO),y
;
;    ldx .counter
;    inx
;    cpx #100
;    beq .end
;    stx .counter
;    ; Advance pointers.
;    lda BITMAP_LO
;    clc
;    adc #12
;    sta BITMAP_LO
;    lda BITMAP_HI
;    adc #0
;    sta BITMAP_HI
;    lda TREES_LO
;    clc
;    adc #7
;    sta TREES_LO
;    lda TREES_HI
;    adc #0
;    sta TREES_HI
;
;    jmp .loop_top
;
;.end
;    rts
;; end sub fmag_run_test
;} ; !zone

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

;fmag_test_data
;!byte $cf,$17,$f4,$1e,$30,$08
;!byte $1f,$ff,$00,$00,$78,$00
;!byte $f1,$0c,$7d,$12,$12,$04
;!byte $b7,$00,$00,$00,$b2,$00
;!byte $97,$06,$7c,$11,$73,$0a
;!byte $18,$00,$00,$00,$fe,$00
;!byte $bd,$1e,$d8,$05,$9b,$18
;!byte $b2,$00,$00,$00,$b7,$00
;!byte $d8,$06,$6d,$09,$c8,$1d
;!byte $bd,$00,$00,$00,$ac,$00
;!byte $88,$0d,$53,$0a,$2a,$0d
;!byte $21,$ff,$00,$00,$7c,$00
;!byte $a3,$02,$84,$0d,$c9,$1c
;!byte $ef,$00,$00,$00,$59,$00
;!byte $bc,$13,$1b,$02,$33,$0d
;!byte $aa,$00,$00,$00,$be,$00
;!byte $ce,$16,$e2,$19,$95,$0d
;!byte $f5,$00,$00,$00,$48,$00
;!byte $bb,$1e,$5b,$1c,$d3,$03
;!byte $06,$ff,$00,$00,$33,$00
;!byte $21,$12,$c0,$09,$ac,$07
;!byte $dc,$00,$00,$00,$81,$00
;!byte $69,$17,$2c,$0e,$ea,$18
;!byte $7a,$00,$00,$00,$e0,$00
;!byte $4a,$1b,$77,$11,$eb,$18
;!byte $ab,$00,$00,$00,$bd,$00
;!byte $80,$07,$d5,$12,$f4,$17
;!byte $d3,$ff,$00,$00,$fb,$00
;!byte $1e,$09,$fc,$15,$a5,$0a
;!byte $09,$ff,$00,$00,$43,$00
;!byte $19,$01,$66,$10,$22,$09
;!byte $1c,$00,$00,$00,$fe,$00
;!byte $f9,$07,$6c,$05,$bf,$1d
;!byte $b6,$00,$00,$00,$b3,$00
;!byte $ff,$0e,$b6,$10,$73,$15
;!byte $ac,$00,$00,$00,$bd,$00
;!byte $57,$09,$d1,$05,$3e,$18
;!byte $d1,$ff,$00,$00,$fb,$00
;!byte $3a,$11,$9f,$0e,$46,$0f
;!byte $f5,$00,$00,$00,$48,$00
;!byte $92,$1a,$fb,$19,$1a,$01
;!byte $5b,$00,$00,$00,$ef,$00
;!byte $34,$0b,$74,$0d,$f5,$19
;!byte $5d,$ff,$00,$00,$c5,$00
;!byte $26,$0c,$7f,$05,$71,$1e
;!byte $11,$ff,$00,$00,$5a,$00
;!byte $91,$11,$ad,$1b,$4a,$15
;!byte $da,$00,$00,$00,$85,$00
;!byte $4d,$10,$b2,$15,$6b,$14
;!byte $f5,$00,$00,$00,$48,$00
;!byte $b9,$0f,$61,$17,$fa,$0c
;!byte $fe,$00,$00,$00,$1f,$00
;!byte $39,$0f,$32,$11,$45,$18
;!byte $36,$ff,$00,$00,$9c,$00
;!byte $1a,$0e,$88,$13,$03,$1c
;!byte $c2,$00,$00,$00,$a6,$00
;!byte $60,$13,$aa,$0e,$16,$1e
;!byte $6d,$00,$00,$00,$e7,$00
;!byte $18,$08,$e0,$15,$eb,$0b
;!byte $ce,$ff,$00,$00,$fb,$00
;!byte $04,$0a,$38,$0a,$b6,$09
;!byte $91,$00,$00,$00,$d2,$00
;!byte $10,$10,$85,$0b,$8f,$04
;!byte $17,$00,$00,$00,$fe,$00
;!byte $98,$02,$21,$15,$8d,$02
;!byte $cc,$ff,$00,$00,$fa,$00
;!byte $12,$13,$59,$11,$07,$02
;!byte $e0,$ff,$00,$00,$fd,$00
;!byte $c7,$18,$9b,$13,$30,$16
;!byte $ff,$00,$00,$00,$09,$00
;!byte $fe,$17,$35,$19,$e2,$0c
;!byte $2d,$ff,$00,$00,$90,$00
;!byte $9c,$18,$38,$0a,$db,$15
;!byte $48,$00,$00,$00,$f5,$00
;!byte $35,$0a,$15,$08,$1d,$0b
;!byte $cd,$ff,$00,$00,$fa,$00
;!byte $2c,$11,$23,$0f,$15,$1a
;!byte $1a,$00,$00,$00,$fe,$00
;!byte $62,$03,$e0,$19,$24,$1b
;!byte $01,$ff,$00,$00,$10,$00
;!byte $d3,$1a,$58,$13,$69,$06
;!byte $f7,$00,$00,$00,$42,$00
;!byte $31,$08,$19,$0b,$58,$16
;!byte $87,$ff,$00,$00,$e1,$00
;!byte $24,$12,$42,$13,$b6,$15
;!byte $f1,$00,$00,$00,$54,$00
;!byte $d6,$0f,$96,$19,$29,$0c
;!byte $27,$00,$00,$00,$fc,$00
;!byte $af,$03,$72,$16,$71,$11
;!byte $0d,$00,$00,$00,$ff,$00
;!byte $f4,$0c,$21,$0c,$42,$13
;!byte $bd,$ff,$00,$00,$f7,$00
;!byte $67,$01,$d5,$18,$3d,$16
;!byte $2b,$00,$00,$00,$fc,$00
;!byte $79,$18,$52,$1d,$fa,$09
;!byte $74,$00,$00,$00,$e4,$00
;!byte $cd,$1a,$90,$08,$6b,$08
;!byte $82,$ff,$00,$00,$de,$00
;!byte $8b,$08,$42,$1a,$23,$0d
;!byte $6b,$ff,$00,$00,$d0,$00
;!byte $84,$10,$87,$0b,$5b,$12
;!byte $a5,$00,$00,$00,$c3,$00
;!byte $af,$18,$24,$0f,$0f,$01
;!byte $34,$ff,$00,$00,$9a,$00
;!byte $7e,$04,$eb,$08,$92,$06
;!byte $9e,$00,$00,$00,$c9,$00
;!byte $12,$1d,$ac,$05,$df,$11
;!byte $46,$ff,$00,$00,$af,$00
;!byte $bf,$05,$98,$13,$1c,$05
;!byte $85,$00,$00,$00,$da,$00
;!byte $ff,$18,$9a,$17,$25,$1a
;!byte $c4,$00,$00,$00,$a3,$00
;!byte $44,$05,$ad,$02,$df,$11
;!byte $ad,$00,$00,$00,$bc,$00
;!byte $ee,$0b,$ff,$02,$5c,$15
;!byte $00,$00,$00,$00,$ff,$00
;!byte $7b,$1c,$bd,$11,$e6,$0a
;!byte $27,$ff,$00,$00,$87,$00
;!byte $86,$0e,$ff,$05,$94,$1b
;!byte $0a,$ff,$00,$00,$44,$00
;!byte $0d,$01,$93,$1c,$1e,$13
;!byte $05,$ff,$00,$00,$30,$00
;!byte $67,$03,$a8,$0d,$ba,$0c
;!byte $75,$00,$00,$00,$e3,$00
;!byte $3d,$0c,$28,$16,$bc,$04
;!byte $3b,$00,$00,$00,$f9,$00
;!byte $be,$0a,$f3,$0f,$8c,$11
;!byte $8d,$ff,$00,$00,$e4,$00
;!byte $48,$02,$64,$05,$10,$1a
;!byte $67,$ff,$00,$00,$cd,$00
;!byte $81,$03,$2e,$03,$5d,$04
;!byte $fd,$00,$00,$00,$25,$00
;!byte $4f,$02,$d5,$16,$0c,$10
;!byte $e4,$00,$00,$00,$73,$00
;!byte $e8,$1e,$c3,$1d,$0b,$01
;!byte $ce,$ff,$00,$00,$fa,$00
;!byte $59,$17,$8a,$1d,$65,$0e
;!byte $0b,$ff,$00,$00,$49,$00
;!byte $de,$18,$8d,$02,$f3,$04
;!byte $fb,$ff,$00,$00,$ff,$00
;!byte $e8,$0f,$59,$0c,$17,$1b
;!byte $4b,$00,$00,$00,$f4,$00
;!byte $54,$03,$6f,$18,$7a,$01
;!byte $ce,$00,$00,$00,$96,$00
;!byte $06,$15,$f3,$0e,$4d,$06
;!byte $41,$ff,$00,$00,$a9,$00
;!byte $d4,$0a,$8f,$0c,$1a,$1c
;!byte $31,$ff,$00,$00,$95,$00
;!byte $e9,$0d,$80,$10,$a5,$0f
;!byte $f5,$00,$00,$00,$48,$00
;!byte $ac,$1a,$09,$04,$c5,$1a
;!byte $64,$ff,$00,$00,$ca,$00
;!byte $76,$19,$85,$1d,$cb,$15
;!byte $04,$ff,$00,$00,$2b,$00
;!byte $43,$1a,$b2,$1d,$63,$18
;!byte $ff,$00,$00,$00,$13,$00
;!byte $bd,$0b,$3f,$11,$cf,$02
;!byte $ba,$00,$00,$00,$af,$00
;!byte $15,$13,$f5,$13,$00,$11
;!byte $75,$ff,$00,$00,$d6,$00
;!byte $89,$1d,$e8,$13,$67,$0f
;!byte $8e,$00,$00,$00,$d4,$00
;!byte $c6,$17,$7b,$0d,$99,$17
;!byte $51,$ff,$00,$00,$ba,$00
;!byte $cd,$12,$3a,$1b,$7e,$1b
;!byte $f0,$00,$00,$00,$56,$00
;!byte $e5,$18,$70,$12,$85,$0f
;!byte $a9,$00,$00,$00,$bf,$00
;!byte $cd,$0b,$47,$15,$fb,$14
;!byte $e0,$00,$00,$00,$7a,$00
;!byte $a7,$19,$e0,$08,$52,$03
;!byte $96,$00,$00,$00,$ce,$00
;!byte $40,$0c,$fb,$1d,$30,$1a
;!byte $c0,$00,$00,$00,$a8,$00
;!byte $4e,$0a,$29,$11,$9a,$1c
;!byte $ff,$00,$00,$00,$07,$00
;!byte $28,$0e,$6a,$0e,$5a,$12
;!byte $33,$ff,$00,$00,$98,$00
;!byte $f4,$1d,$4d,$1b,$98,$15
;!byte $c8,$ff,$00,$00,$f9,$00
;!byte $91,$0e,$f6,$1d,$62,$15
;!byte $a5,$ff,$00,$00,$ef,$00
;!byte $18,$0e,$52,$17,$b2,$0e
;!byte $29,$ff,$00,$00,$8a,$00
;!byte $a9,$0d,$79,$1c,$e5,$12
;!byte $ec,$ff,$00,$00,$ff,$00
;!byte $05,$1a,$36,$0d,$bf,$1a
;!byte $fa,$ff,$00,$00,$ff,$00
;!byte $36,$19,$fa,$09,$75,$05
;!byte $25,$ff,$00,$00,$83,$00
;!byte $9e,$18,$0d,$03,$aa,$02
;!byte $9a,$00,$00,$00,$cc,$00
;!byte $dc,$0a,$71,$02,$60,$18
;!byte $06,$ff,$00,$00,$35,$00
;!byte $a3,$05,$37,$0a,$29,$1d
;!byte $09,$ff,$00,$00,$40,$00
;!byte $7c,$02,$4e,$16,$f1,$1a
;!byte $08,$00,$00,$00,$ff,$00
;!byte $33,$12,$6f,$05,$42,$17
;!byte $9c,$00,$00,$00,$ca,$00
fmag_c_SIZE = *-fmag_c_BEGIN
