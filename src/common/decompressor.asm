; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   P0-P1 = source address.

!zone {
.SRC_LO     = P0
.SRC_HI     = P1
.DEST_LO    = P2
.DEST_HI    = P3
.ESC_CODE   = P4
; Use this Y-index for .DEST_LO/.DEST_HI.
.DEST_ITER  = P5
.PATTERN    = P6

decmp_s_display_mc_bitmap
    ; Two-byte header consists of b/g color and the escape code.
    ldy #0
    sty .DEST_ITER

    lda (.SRC_LO),y
    sta BGCOL0
    iny
    lda (.SRC_LO),y
    sta .ESC_CODE

    ; Add 2 (header size in bytes) to .SRC_LO/HI - keeps things tidy(-ish).
    +adc16_8bit_imm .SRC_LO,2

.loop_top
    ldy #0
    lda (.SRC_LO),y
    cmp .ESC_CODE
    beq .handle_run

    ; A single byte to deal with.
    ldy .DEST_ITER
    sta (.DEST_LO),y
    iny
    sty .DEST_ITER
    bne +
    inc .DEST_HI
+
    ; FIXME: wasteful!  Shouldn't need to do this each time round for
    ; single byte writes!!!
    +adc16_8bit_imm .SRC_LO,1
    jmp .loop_top

.handle_run
    iny
    lda (.SRC_LO),y
    sta .PATTERN
    iny
    lda (.SRC_LO),y
    ; Number of repeats into X (- though won't need it if we're at end of data).
    tax 
    ; Advance 'source' pointer.
    +adc16_8bit_imm .SRC_LO,3
    txa
    ; Escape code followed by 2 0's?!
    ora .PATTERN
    beq .colors

    ; Handle 'run' here.  Byte to be written is in .PATTERN; X holds number
    ; of repeats required.
    ldy .DEST_ITER
    lda .PATTERN
.run_loop
    sta (.DEST_LO),y
    iny
    bne +
    inc .DEST_HI
+
    dex
    bne .run_loop
   
    ; Run has finished.  Record destination iterator for next use.
    sty .DEST_ITER
    ; NOTE: 'sty' doesn't affect any flags, so Z flag will still be set.
    beq .loop_top

.colors

    rts
; end sub decmp_s_display_mc_bitmap
} ; !zone
end_sub_decmp_s_display_mc_bitmap
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
; **************************************************

