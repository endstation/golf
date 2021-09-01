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
; INPUTS:   P0-P1 = bitmap data.
!zone {
.BMAP_SRC_LO = P0   ; bitmap
.BMAP_SRC_HI = P1
.VRAM_SRC_LO = P2   ; video RAM
.VRAM_SRC_HI = P3
.CRAM_SRC_LO = P4   ; color RAM
.CRAM_SRC_HI = P5

.BMAP_DST_LO = MATHS0
.BMAP_DST_HI = MATHS1
.VRAM_DST_LO = MATHS2
.VRAM_DST_HI = MATHS3
.CRAM_DST_LO = MATHS4
.CRAM_DST_HI = MATHS5

bmap_s_draw_multicolor
    ; NOTE: assume multicolor bitmap mode has already been set!

    ; Initialize pointers for video & color RAM.
    lda .BMAP_SRC_LO
    clc
    adc #<8000
    sta .VRAM_SRC_LO
    lda .BMAP_SRC_HI
    adc #>8000
    sta .VRAM_SRC_HI
    lda .VRAM_SRC_LO
    clc
    adc #<1000
    sta .CRAM_SRC_LO
    lda .VRAM_SRC_HI
    adc #>1000
    sta .CRAM_SRC_HI
    ; Destination pointers.
    lda #<gfxs_c_BITMAP_BASE
    sta .BMAP_DST_LO
    lda #>gfxs_c_BITMAP_BASE
    sta .BMAP_DST_HI
    lda #<gfxs_c_DISPLAY_BASE
    sta .VRAM_DST_LO
    lda #>gfxs_c_DISPLAY_BASE
    sta .VRAM_DST_HI
    lda #<COLOR_RAM
    sta .CRAM_DST_LO
    lda #>COLOR_RAM
    sta .CRAM_DST_HI

    ; Copy bitmap data.
    ; 32 * 250 bytes.
    ldx #32
.loop1
    ldy #0
-
    lda (.BMAP_SRC_LO),y
    sta (.BMAP_DST_LO),y
    iny
    cpy #250
    bne -
    dex
    beq .colors
    ; Advance pointers.
    lda .BMAP_SRC_LO
    clc
    adc #250
    sta .BMAP_SRC_LO
    bcc +
    inc .BMAP_SRC_HI
+    
    lda .BMAP_DST_LO
    clc
    adc #250
    sta .BMAP_DST_LO
    bcc .loop1
    inc .BMAP_DST_HI
    bne .loop1 

.colors
    ; Copy 4 * 250 bytes.
    ldx #4
.loop2
    ldy #0
-
    lda (.VRAM_SRC_LO),y
    sta (.VRAM_DST_LO),y
    lda (.CRAM_SRC_LO),y
    sta (.CRAM_DST_LO),y
    iny
    cpy #250
    bne -
    dex
    beq .bg
    ; Advance pointers.
    lda .VRAM_SRC_LO
    clc
    adc #250
    sta .VRAM_SRC_LO
    bcc +
    inc .VRAM_SRC_HI
+
    lda .CRAM_SRC_LO
    clc
    adc #250
    sta .CRAM_SRC_LO
    bcc +
    inc .CRAM_SRC_HI
+
    lda .VRAM_DST_LO
    clc
    adc #250
    sta .VRAM_DST_LO
    bcc +
    inc .VRAM_DST_HI
+
    lda .CRAM_DST_LO
    clc
    adc #250
    sta .CRAM_DST_LO
    bcc .loop2
    inc .CRAM_DST_HI
    bne .loop2

.bg
    lda (.CRAM_SRC_LO),y
    sta BGCOL0

    rts
; end sub bmap_s_draw_multicolor
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
; **************************************************

