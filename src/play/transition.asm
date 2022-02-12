; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


transtn_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************


; *****************
; *** VARIABLES ***
; *****************
transtn_v_current_row       !byte   0
transtn_v_is_active         !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
transtn_s_init
    +clr transtn_v_current_row
    lda #1
    sta transtn_v_is_active

    lda #<COLOR_RAM
    sta LINE_X0_LO 
    lda #>COLOR_RAM
    sta LINE_Y0_LO 
    lda #<gfxs_c_DISPLAY_BASE 
    sta LINE_X1_LO 
    lda #>gfxs_c_DISPLAY_BASE 
    sta LINE_Y1_LO 

    lda #<gfxs_c_BITMAP_BASE 
    sta EDGES_LO 
    lda #>gfxs_c_BITMAP_BASE 
    sta EDGES_HI 
    lda #<gfxs_c_BITMAP_BASE+160
    sta WS_X_LO  
    lda #>gfxs_c_BITMAP_BASE+160
    sta WS_X_HI 

    rts
; end sub transtn_s_init
} ; !zone

; **************************************************

; OUTPUT:   Z flag set if whole screen now blanked; else Z flag clear.
!zone {
.CURRENT_COLOR = MATHS0

transtn_s_blank_one_row
    ldy #0
    lda #0
-
    sta (LINE_X0_LO),y
    sta (LINE_X1_LO),y
    iny
    cpy #40
    bne -

    ldy #0
    lda #$ff
-
    sta (EDGES_LO),y
    sta (WS_X_LO),y
    iny
    cpy #160
    bne -

    ldx transtn_v_current_row
    inx
    ; Don't change the final row - keep colors intact for messaging!
    cpx #24
    beq .end
    stx transtn_v_current_row

    lda EDGES_LO
    clc
    adc #<320
    sta EDGES_LO
    lda EDGES_HI
    adc #>320
    sta EDGES_HI
    lda WS_X_LO
    clc
    adc #<320
    sta WS_X_LO
    lda WS_X_HI
    adc #>320
    sta WS_X_HI

    lda LINE_X0_LO
    clc
    adc #40
    sta LINE_X0_LO
    lda LINE_Y0_LO
    adc #0
    sta LINE_Y0_LO
    lda LINE_X1_LO
    clc
    adc #40
    sta LINE_X1_LO
    ; NOTE: this will always be non-zero, so routine will exit with 
    ; Z flag clear.
    lda LINE_Y1_LO
    adc #0
    sta LINE_Y1_LO

.end
    rts
; end sub transtn_s_blank_one_row
} ; !zone

; **************************************************

!zone {
transtn_s_update
    lda transtn_v_is_active
    +branch_if_false .end

    jsr transtn_s_blank_one_row
    bne .end
    lda #0
    sta transtn_v_is_active
    sta round_v_current_sky_color     
    sta round_v_current_ground_color    

.end
    rts
; end sub transtn_s_update
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

transtn_c_SIZE = *-transtn_c_BEGIN

