; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


terrain_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
;terrain_c_BUFFER_LEN = 4
;terrain_l_FACTOR_STRS           !raw    "  x1","x2/3","x1/2","x1/3"
;terrain_c_MSG_COL = 36*4
;terrain_l_STR_OFFSETS           !byte   4,0,0
;terrain_l_BUNKER_STR_OFFSETS    !byte   4,8,12
;terrain_l_STR_COLORS            !byte   ORANGE,YELLOW,GREEN

terrain_c_VIDEO_RAM_BASE = gfxs_c_DISPLAY_BASE+(22*40)+1
terrain_l_COLORS
    !byte   (ORANGE<<4)|ORANGE,0,(GREEN<<4)|GREEN,(LIGHT_BLUE<<4)|LIGHT_BLUE
terrain_l_SAND_COLORS
    !byte   (YELLOW<<4)|YELLOW,(GREY1<<4)|YELLOW,YELLOW
terrain_l_NUM_CHARS_TO_PAINT    !byte   2,0,3,2
terrain_l_SAND_NUM_CHARS_TO_PAINT   !byte   2,2,1


; *****************
; *** VARIABLES ***
; *****************
;terrain_v_buffer    !fill   terrain_c_BUFFER_LEN


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
;!zone {
;.COLOR_I = CAMERA0
;
;terrain_s_draw_indicator
;    ldx round_v_current_player
;    lda players_v_terrain,x
;    sta .COLOR_I
;    cmp #ball_c_TERRAIN_BUNKER
;    bne +
;
;    ; In bunker, so 'power factor' depends on difficulty setting.
;    ; We also use a different table for the offset lookup.
;    ldx shared_v_sand_traps_difficulty
;    lda terrain_l_BUNKER_STR_OFFSETS,x
;    bpl ++
;
;+
;    tax
;    lda terrain_l_STR_OFFSETS,x
;
;++
;    tax
;    
;    ; Load string into buffer.
;    ; X = source, Y = destination.
;    ldy #0
;-
;    lda terrain_l_FACTOR_STRS,x
;    sta terrain_v_buffer,y
;    inx
;    iny
;    cpy #terrain_c_BUFFER_LEN
;    bne -
;
;    sty P4
;    lda #<terrain_v_buffer
;    sta P0
;    lda #>terrain_v_buffer
;    sta P1
;    lda #terrain_c_MSG_COL
;    sta P3
;    jsr msg_s_display_at_column
;
;    ; Also need to set color!
;    ldx .COLOR_I
;    lda terrain_l_STR_COLORS,x
;    ldx #3
;-
;    sta gfxs_c_DISPLAY_BASE+(24*40)+36,x
;    dex
;    bpl -
;
;    rts
;; end sub terrain_s_draw_indicator
;} ; !zone

; **************************************************

;!zone {
;terrain_s_clear_indicator
;    lda #BLACK
;    ldx #3
;
;-
;    sta gfxs_c_DISPLAY_BASE+(24*40)+36,x
;    dex
;    bpl -
;
;    rts
;; end sub terrain_s_clear_indicator
;} ; !zone

; **************************************************

!zone {
.NUM_CHARS = MATHS0

terrain_s_draw
    ldx round_v_current_player
    ldy players_v_terrain,x
    cpy #ball_c_TERRAIN_BUNKER
    beq .sand
    lda terrain_l_NUM_CHARS_TO_PAINT,y
    sta .NUM_CHARS
    lda terrain_l_COLORS,y
    bne +

.sand
    ldx shared_v_sand_traps_difficulty
    lda terrain_l_SAND_NUM_CHARS_TO_PAINT,x
    sta .NUM_CHARS
    lda terrain_l_SAND_COLORS,x

+
    ; Accumulator holds color.
    ldx #0
-
    sta terrain_c_VIDEO_RAM_BASE,x
    inx
    cpx .NUM_CHARS
    bne -
    
    rts
; end sub terrain_s_draw
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

terrain_c_SIZE = *-terrain_c_BEGIN 

