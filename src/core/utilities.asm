; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; Various stuff which doesn't fit in anywhere else.


; *****************
; *** CONSTANTS ***
; *****************
utils_l_HEX2DEC_LOOKUP
    !byte   1,0,0,0,0
    !byte   2,0,0,0,0
    !byte   4,0,0,0,0
    !byte   8,0,0,0,0
    !byte   6,1,0,0,0
    !byte   2,3,0,0,0
    !byte   4,6,0,0,0
    !byte   8,2,1,0,0
    !byte   6,5,2,0,0
    !byte   2,1,5,0,0
    !byte   4,2,0,1,0
    !byte   8,4,0,2,0
    !byte   6,9,0,4,0
    !byte   2,9,1,8,0
    !byte   4,8,3,6,1
    !byte   8,6,7,2,3
utils_c_HEX2DEC_RESULT_SIZE_BYTES = 5

utils_l_BIT_LOOKUP        !byte   $01,$02,$04,$08,$10,$20,$40,$80
utils_l_EOR_BIT_LOOKUP    !byte   $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f


; *****************
; *** VARIABLES ***
; *****************
; NOTE: stored lsb first.
utils_v_dec_digits    !fill   5


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUT:    copy value into P0-P1 (low byte first).
; OUTPUT:   digits in reverse order in utils_v_dec_digits; number of digits in .X
!zone {
.bit_counter    !byte   0

utils_s_16bit_hex_to_dec
    ; Clear out the results area first.
    ldx #4
    lda #0
-   sta utils_v_dec_digits,x
    dex
    bpl -

    ; '.bit_counter' goes from 15-0.  Once it's negative, we're done.
    lda #15
    sta .bit_counter
    ; X tracks position in lookup table.
    ldx #0

.loop_top
    lsr P1
    ror P0
    bcc .skip

    ; Use Y to count to 5.
    ldy #0
    clc
-   lda utils_v_dec_digits,y
    adc utils_l_HEX2DEC_LOOKUP,x
    ; Force carry if result >= 10.
    cmp #10
    bcc .ok
    ; NOTE: C flag will be set.
    sbc #10
    ; NOTE: C flag will still be set.
.ok
    sta utils_v_dec_digits,y
    inx
    iny
    ; Preserve status of 'C' flag during following comparison.
    php
    cpy #5
    beq +
    plp
    jmp -
+
    plp
    jmp .next_bit
    
.skip
    ; Advance lookup table iterator, since it wasn't used this time round.
    txa
    clc
    adc #utils_c_HEX2DEC_RESULT_SIZE_BYTES
    tax

.next_bit
    dec .bit_counter
    bpl .loop_top

    ; Find length.
    ; Look at each value starting at the end of the array and stop as soon as
    ; it's not zero.  Length is then .X+1.
    ldx #4
-   lda utils_v_dec_digits,x
    bne .found
    dex
    bpl -
.found
    inx
    ; Make sure it's at least 1!
    bne +
    inx
+

    rts
; end sub utils_s_16bit_hex_to_dec
} ; !zone

; **************************************************

; INPUTS:   P0-P1 = address of indices, P2 = number of elements
; NOTE: write address of 'compare' subroutine into location utils_comp+1.
!zone {
.did_swap   !byte   0
.iter       !byte   0

utils_s_bubble_sort

.outer_loop
    lda #0
    sta .iter
    sta .did_swap

.inner_loop
    ; If .iter >= n, inner loop is complete.
    ldx .iter
    inx
    cpx P2
    bcc +

    ; Loop is complete.  If no swaps occurred, we're done.
    lda .did_swap
    beq .end
    ; So not done yet.  Back round for another go.
    jmp .outer_loop

+
    ; Load the next two indices into P3 and P4, then call the 'compare'
    ; routine.
    ldy .iter
    lda (P0),y
    sta P3
    iny
    lda (P0),y
    sta P4
utils_s_comp
    ; NOTE: $ffcc is a dummy placeholder.  Actual address of compare
    ; routine should be written here.
    jsr $ffcc

    bcc +
    ; C flag set, so we must swap these two indices.
    ldy .iter
    lda (P0),y
    tax ; First element into X.
    iny
    lda (P0),y
    pha ; Second element onto stack.
    ; Y is pointing to the second slot.
    txa
    sta (P0),y
    dey
    ; Now Y is pointing to the first slot.
    pla
    sta (P0),y
    ; Record that a swap has taken place.
    lda #1
    sta .did_swap

+
    inc .iter
    jmp .inner_loop

.end
    rts
; end sub utils_s_bubble_sort
} ; !zone

; **************************************************

; Rotate bits in accumulator right twice.
!zone {
utils_s_ror_two_bits
    pha
    lsr
    pla
    ror
    pha
    lsr
    pla
    ror
    rts
; end sub utils_s_ror_two_bits
} ; !zone

; **************************************************

; INPUTS:   P0-P1 = buffer address
;           X = number of bytes to write
; OUTPUTS:  Y = number of bytes written.
!zone {
utils_s_write_digits_to_buffer
    ldy #0
    dex

-
    lda utils_v_dec_digits,x
    clc
    adc #font_c_ASCII_0 
    sta (P0),y
    iny
    dex
    bpl -

    rts
; end sub utils_s_write_digits_to_buffer
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

