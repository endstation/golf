; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
; Adjust these as required.
; Bank for VIC-II memory.
; Bits 0-1:
; 11 = bank #0  ($0000-$3fff)
; 10 = bank #1  ($4000-$7fff)
; 01 = bank #2  ($8000-$bfff)
; 00 = bank #3  ($c000-$ffff)
gfxs_c_BANK0 = %11
gfxs_c_BANK1 = %10
gfxs_c_BANK2 = %01
gfxs_c_BANK3 = %00

; Display memory.
; Bits 4-7 can represent values 0-15.  This is 1K offset from start of 
; video memory.
gfxs_c_DISPLAY_OFFSET = 2<<4 
; Character data.
; Bits 1-3 can represent even values from 0-14 for 2K offset from start of
; video memory.
gfxs_c_CHARS_OFFSET = 0<<4
; Bit #3 of VMCSB holds bitmap screen 8K offset (when in bitmap mode).
gfxs_c_BITMAP_OFFSET = 1<<3

; For copying chars.  Destination address and how many bytes to copy 
; (multiples of 256).  8 pages will copy the whole set of 256 chars.
gfxs_c_CHARS_TO   = $c000
gfxs_c_PAGES      = 2

gfxs_c_DISPLAY_BASE = $c800
gfxs_c_SPR_PTR_BASE = gfxs_c_DISPLAY_BASE+1024-8

gfxs_c_BITMAP_BASE = $e000
; This is the number of bytes separating color RAM from display RAM.
gfxs_c_COLOR_RAM_OFFSET = COLOR_RAM - gfxs_c_DISPLAY_BASE


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; General graphics setup routine.  Modify constants above as required.
!zone {
gfxs_s_init
    ; Set bits 0 and 1 on Data Direction Register A.  This sets them as
    ; outputs.  You need to do this to be able to change banks.
    lda C2DDRA
    ora #$03
    sta C2DDRA

    ; Select bank for VIC-II memory.
    lda CI2PRA
    and #$fc
    ora #gfxs_c_BANK3
    sta CI2PRA

    lda #gfxs_c_DISPLAY_OFFSET
    ora #gfxs_c_BITMAP_OFFSET
    sta VMCSB

    jsr gfxs_s_clear_bitmap

    rts
; end sub gfxs_s_init 
} ; !zone

; **************************************************

; 8000 bytes to clear: 31*256 + 64.
; Skip the first 3 rows = 3*40*8 = 960.
; Then we have 27*256 + 128!
!zone {
gfxs_s_clear_bitmap
    lda #<gfxs_c_BITMAP_BASE
    sta BITMAP_LO
    lda #>gfxs_c_BITMAP_BASE
    sta BITMAP_HI
    lda #0
    ldx #31 ; 31 pages and then an extra 64 bytes.
    ldy #0

-
    sta (BITMAP_LO),y
    iny
    bne -

    inc BITMAP_HI
    dex
    beq .coup_de_grace
    bne -

.coup_de_grace
    ; Final 64 bytes.
    ldy #63
-
    sta (BITMAP_LO),y
    dey
    bpl -

    rts
; end sub gfxs_s_clear_bitmap
} ; !zone

; **************************************************

; Background colors are set as part of the interrupt routine.
!zone {
gfxs_s_init_colors
    ; Foreground colors: eight rows from 16 to 23.  (24-16)*40=320.
    ; YELLOW=%01, GREEN=%10, LIGHT_BLUE=%11.
    ldx #0
-
    lda #(YELLOW<<4)|GREEN
    sta gfxs_c_DISPLAY_BASE+(16*40),x
    sta gfxs_c_DISPLAY_BASE+(20*40),x
    lda #LIGHT_BLUE
    sta COLOR_RAM+(16*40),x
    sta COLOR_RAM+(20*40),x
    inx
    cpx #160
    bne -

    ldx #0
-
    lda #(GREY2<<4)|GREEN
    sta gfxs_c_DISPLAY_BASE+(4*40),x
    sta gfxs_c_DISPLAY_BASE+(10*40),x
    lda #GREY2
    sta COLOR_RAM+(4*40),x
    sta COLOR_RAM+(10*40),x
    inx
    cpx #240
    bne -

    rts
; end sub gfxs_s_init_colors
} ; !zone

; **************************************************

!zone {
gfxs_s_clear_msg_area
    lda #<(gfxs_c_BITMAP_BASE+24*40*8)
    sta P0
    lda #>(gfxs_c_BITMAP_BASE+24*40*8)
    sta P1

    ldy #0
    lda #$ff
-
    sta (P0),y
    iny
    bne -

    inc P1
    ldy #0
-
    sta (P0),y
    iny
    cpy #64
    bne -

    ldx #39
-
    lda #GREY3|(BLACK<<4)
    sta gfxs_c_DISPLAY_BASE+24*40,x
    lda #BLACK
    sta COLOR_RAM+24*40,x
    dex
    bpl -

    rts
; end sub gfxs_s_clear_msg_area
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

