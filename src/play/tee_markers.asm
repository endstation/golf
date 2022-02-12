; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


tmarkers_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; NOTE: colors = 00 (brown), 01 (cyan), 10 (green), 11 (blue)
tmarkers_c_PATTERN  !byte   $aa,$96,$55,$d5,$d5,$f5,$ff,$3e
tmarkers_c_BITMAP_DEST0 = gfxs_c_BITMAP_BASE+(22*40*8)+(13*8)
tmarkers_c_BITMAP_DEST1 = gfxs_c_BITMAP_BASE+(22*40*8)+(26*8)
tmarkers_c_VRAM_DEST0 = gfxs_c_DISPLAY_BASE+(22*40)+13 
tmarkers_c_VRAM_DEST1 = gfxs_c_DISPLAY_BASE+(22*40)+26 
tmarkers_c_CRAM_DEST0 = COLOR_RAM+(22*40)+13
tmarkers_c_CRAM_DEST1 = COLOR_RAM+(22*40)+26

tmarkers_c_VRAM_VALUE = (CYAN<<4)|GREEN
tmarkers_c_CRAM_VALUE = BLUE

tmarkers_c_SHADOW_BASE_DEST0 = gfxs_c_BITMAP_BASE+(22*40*8)+(12*8)+6
tmarkers_c_SHADOW_BASE_DEST1 = gfxs_c_BITMAP_BASE+(23*40*8)+(12*8) 
tmarkers_c_SHADOW_OFFSET = 13*8
tmarkers_c_SHADOW_PATTERNS1 !byte $fc,$cf,$3f
tmarkers_c_SHADOW_OFFSETS1  !byte 0,8,9


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
tmarkers_s_draw
    ; Draw the pattern first.
    ldx #7
-
    lda tmarkers_c_PATTERN,x
    sta tmarkers_c_BITMAP_DEST0,x
    sta tmarkers_c_BITMAP_DEST1,x
    dex
    bpl -

    ; And now the colours.
    lda #tmarkers_c_VRAM_VALUE
    sta tmarkers_c_VRAM_DEST0
    sta tmarkers_c_VRAM_DEST1
    lda #tmarkers_c_CRAM_VALUE
    sta tmarkers_c_CRAM_DEST0
    sta tmarkers_c_CRAM_DEST1

    ; Finally, shadows.
    ; Switch out the kernal because we're reading from the bitmap.
    +utils_m_kernal_out 

    lda tmarkers_c_SHADOW_BASE_DEST0
    and #$fc
    sta tmarkers_c_SHADOW_BASE_DEST0
    lda tmarkers_c_SHADOW_BASE_DEST0+tmarkers_c_SHADOW_OFFSET
    and #$fc
    sta tmarkers_c_SHADOW_BASE_DEST0+tmarkers_c_SHADOW_OFFSET

    ldx #2
-
    lda tmarkers_c_SHADOW_OFFSETS1,x
    tay
    lda tmarkers_c_SHADOW_BASE_DEST1,y
    and tmarkers_c_SHADOW_PATTERNS1,x
    sta tmarkers_c_SHADOW_BASE_DEST1,y
    lda tmarkers_c_SHADOW_BASE_DEST1+tmarkers_c_SHADOW_OFFSET,y
    and tmarkers_c_SHADOW_PATTERNS1,x
    sta tmarkers_c_SHADOW_BASE_DEST1+tmarkers_c_SHADOW_OFFSET,y
    dex
    bpl -

    +utils_m_kernal_in 

    rts
; end sub tmarkers_s_draw
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

tmarkers_c_SIZE = *-tmarkers_c_BEGIN

