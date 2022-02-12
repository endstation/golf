; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


clubs_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; NOTE: this excludes the putter:
CLUBS_N = 12
CLUBS_PUTTER_I = CLUBS_N
clubs_c_OFFSET_TO_PUNCH_VELOCITIES = 4*13

; Power levels for each club.
;clubs_l_VZ_LO   !byte <8686, <7887 ,<7079, <5925, <5553, <4922, <4572, <4096, <3904
;                !byte <3629, <2933, <2271, <1480    ;<1504
;clubs_l_VZ_HI   !byte >8686, >7887 ,>7079, >5925, >5553, >4922, >4572, >4096, >3904
;                !byte >3629, >2933, >2271, >1480    ;>1504
;clubs_l_VY_LO   !byte <1375, <1391 ,<1376, <1477, <1487, <1411, <1398, <1331, <1343
;                !byte <1307, <1368, <1419, 0
;clubs_l_VY_HI   !byte >1375, >1391 ,>1376, >1477, >1487, >1411, >1398, >1331, >1343
;                !byte >1307, >1368, >1419, 0
!source "trajectories.asm"
!source "../../tools/clubs/punch_club_velocities.asm"

CLUBS_STR !scr "DR3W5W3I4I5I6I7I8I9IPWSWPT"
clubs_l_DISTANCE_STR    !scr "240230215205195175165150145135120100"   
                        !scr "2  6  14 24 37 52 "
clubs_l_DISTANCE_STR_OFFSETS    !byte 0,3,6,9,12,15,18,21,24,27,30,33
                                !byte 36,39,42,45,48,51
clubs_l_DISTANCE_PIXELS_LO
    !byte   <(240*hole_c_PIXELS_PER_YARD )
    !byte   <(230*hole_c_PIXELS_PER_YARD )
    !byte   <(215*hole_c_PIXELS_PER_YARD )
    !byte   <(205*hole_c_PIXELS_PER_YARD )
    !byte   <(195*hole_c_PIXELS_PER_YARD )
    !byte   <(175*hole_c_PIXELS_PER_YARD )
    !byte   <(165*hole_c_PIXELS_PER_YARD )
    !byte   <(150*hole_c_PIXELS_PER_YARD )
    !byte   <(145*hole_c_PIXELS_PER_YARD )
    !byte   <(135*hole_c_PIXELS_PER_YARD )
    !byte   <(120*hole_c_PIXELS_PER_YARD )
    !byte   <(100*hole_c_PIXELS_PER_YARD )
clubs_l_DISTANCE_PIXELS_HI
    !byte   >(240*hole_c_PIXELS_PER_YARD )
    !byte   >(230*hole_c_PIXELS_PER_YARD )
    !byte   >(215*hole_c_PIXELS_PER_YARD )
    !byte   >(205*hole_c_PIXELS_PER_YARD )
    !byte   >(195*hole_c_PIXELS_PER_YARD )
    !byte   >(175*hole_c_PIXELS_PER_YARD )
    !byte   >(165*hole_c_PIXELS_PER_YARD )
    !byte   >(150*hole_c_PIXELS_PER_YARD )
    !byte   >(145*hole_c_PIXELS_PER_YARD )
    !byte   >(135*hole_c_PIXELS_PER_YARD )
    !byte   >(120*hole_c_PIXELS_PER_YARD )
    !byte   >(100*hole_c_PIXELS_PER_YARD )

; NOTE: these three chars are placeholders for actual distance.
clubs_l_DISTANCE_BUFF !raw "000"
clubs_c_BUFF_LEN = 3

clubs_c_DR = 0
clubs_c_3W = 1
clubs_c_5I = 5
clubs_c_8I = 8
clubs_c_PW = 10
clubs_c_SW = 11
; NOTE: except when on the green!
clubs_c_MAX_ALLOWED = clubs_c_SW
; Index this with the current player's terrain.
clubs_l_MIN_CLUBS   !byte   clubs_c_3W,clubs_c_8I,clubs_c_3W,clubs_c_5I   


; *****************
; *** VARIABLES ***
; *****************
clubs_v_current_selection !byte   0
clubs_v_min_allowed     !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
clubs_reset
    lda #0
    sta clubs_v_current_selection
    rts
; end sub clubs_reset
} ; !zone

; **************************************************

; NOTE: implement wraparound.
!zone {
clubs_s_next
    ldx clubs_v_current_selection
    inx
    cpx #clubs_c_MAX_ALLOWED+1
    bcc .ok
    ; Wraparound.
    ldx clubs_v_min_allowed
.ok
    stx clubs_v_current_selection

    ldy #sfx_c_BROWSE
    jsr snd_s_init_sfx

    rts
; end sub clubs_s_next
} ; !zone

; **************************************************

; NOTE: implement wraparound.
!zone {
clubs_s_prev
    ldx clubs_v_current_selection
    dex
    bmi .wraparound
    cpx clubs_v_min_allowed
    bcc .wraparound
    +skip_2_bytes
.wraparound
    ldx #clubs_c_MAX_ALLOWED
    stx clubs_v_current_selection

    ldy #sfx_c_BROWSE
    jsr snd_s_init_sfx

    rts
; end sub clubs_s_prev
} ; !zone

; **************************************************

; Puts address of club 'strings' into P0-P1 and sets Y to the correct
; offset for the current club selection.
;!zone {
;clubs_get_str
;    lda #<CLUBS_STR
;    sta P0
;    lda #>CLUBS_STR
;    sta P1
;    lda clubs_current_selection
;    asl
;    tay
;    rts
;; end sub clubs_get_str
;} ; !zone

; **************************************************

;!zone {
;clubs_s_draw
;    ldx clubs_current_selection
;    lda CLUBS_STR_OFFSETS,x
;    sta P0  ; from
;    lda CLUBS_STR_OFFSETS+1,x
;    sta P1  ; to
;
;    ; X indexes source, Y the destination.
;    ldx P0
;    ldy #0
;
;-
;    lda CLUBS_STR2,x
;    sta utils_str_buf,y
;    inx
;    iny
;    cpx P1
;    bne -
;
;    ldx #0
;-
;    lda CLUBS_STR_SUFFIX,x
;    beq +
;    sta utils_str_buf,y
;    inx
;    iny
;    jmp -
;
;+
;    ; Y now holds string length.
;    sty P6  
;    lda #<utils_str_buf
;    sta P0
;    lda #>utils_str_buf
;    sta P1
;    lda #24*8
;    sta P2
;    lda #4
;    sta P3
;    jsr dp_draw_string
;
;    lda clubs_current_selection
;    ; Multiply by 2 to get index into CLUBS_STR2.
;    asl
;    tax
;    ; Second char 'code' first and push onto stack.
;    lda CLUBS_STR2+1,x
;    pha
;    lda CLUBS_STR2,x
;    ; Multiply code by 8 to get index into char data (i.e. CLUBS_CHARS).
;    asl
;    asl
;    asl
;    tax
;
;    ; Use Y to count writes.
;    ldy #0
;
;-
;    lda CLUBS_CHARS,x
;    sta CLUBS_STR_DEST,y
;    sta CLUBS_STR_DEST2,y
;    iny
;    inx
;    cpy #8
;    bne -
;
;    ; Now second char - pull code off stack.
;    pla
;    asl
;    asl
;    asl
;    tax
;-
;    lda CLUBS_CHARS,x
;    sta CLUBS_STR_DEST,y
;    sta CLUBS_STR_DEST2,y
;    iny
;    inx
;    cpy #16
;    bne -
;
;    rts
;; end sub clubs_s_draw
;} ; !zone

; **************************************************

!zone {
clubs_s_draw2
    ; Address of text into P0-P1.
    lda clubs_v_current_selection
    asl
    clc
    adc #<CLUBS_STR
    sta P0
    lda #>CLUBS_STR
    adc #0
    sta P1

    ; (Off-screen) destination into BITMAP_LO/HI.
;    lda #<CLUBS_STR_DEST2
;    sta BITMAP_LO
;    lda #>CLUBS_STR_DEST2
;    sta BITMAP_HI
    lda #2
    sta P4
;    jsr font_s_draw_text_direct

    lda #20*8
    sta P2
    lda #1*4
    sta P3

    jsr font_s_shift_text_right
    jsr font_s_draw_text
    jsr clubs_s_draw_distance
    jsr font_s_reset_shift

    rts
; end sub clubs_s_draw2
} ; !zone

; **************************************************

; Draw average maximum distance for this club in the lhs panel.
!zone {
.DEST_ROW = 21*8
.DEST_COL = 1*4

clubs_s_draw_distance
    ldx clubs_v_current_selection
    cpx #CLUBS_PUTTER_I 
    bne .swing

    lda powarc_v_putt_assist_index
    ; Skip over the first 12 bytes to get to the putting distances!
    clc
    adc #CLUBS_N
    tax

.swing
    lda clubs_l_DISTANCE_STR_OFFSETS,x
    tax
    ; Now we have offset for the distance string. Copy 3 bytes to the buffer.
    ldy #0
-
    lda clubs_l_DISTANCE_STR,x
    sta clubs_l_DISTANCE_BUFF,y
    inx
    iny
    cpy #clubs_c_BUFF_LEN
    bne -

    ; Prepare call to msg_s_display.
    lda #<clubs_l_DISTANCE_BUFF
    sta P0
    lda #>clubs_l_DISTANCE_BUFF
    sta P1
    lda #clubs_c_BUFF_LEN
    sta P4
    lda #.DEST_ROW
    sta P2
    lda #.DEST_COL
    sta P3
    jsr font_s_draw_text

    rts
; end sub clubs_s_draw_distance
} ; !zone

; **************************************************

!zone {
.LAST_VALID_INDEX = MATHS0

clubs_s_select_best_club
    ldy round_v_current_player
    ldx #clubs_c_MAX_ALLOWED 
-
    lda clubs_l_DISTANCE_PIXELS_LO,x
    cmp players_v_distance_lo,y
    lda clubs_l_DISTANCE_PIXELS_HI,x
    sbc players_v_distance_hi,y
    bcs .found
    dex
    cpx clubs_v_min_allowed
    bne -

.found
    stx clubs_v_current_selection

    rts
; end sub clubs_s_select_best_club
} ; !zone

; **************************************************

; NOTE: minimum in terms of its index - actually it's the biggest club you're
; allowed to play!
; INPUTS:   MATHS0 = terrain
!zone {
clubs_s_set_min_allowed
    lda round_v_teeing_off
    +branch_if_false + 
    lda #clubs_c_DR
    beq ++
+
    ldy MATHS0
    lda clubs_l_MIN_CLUBS,y
++
    sta clubs_v_min_allowed
    rts
; end sub clubs_s_set_min_allowed
} ; !zone

; **************************************************

; INPUTS:   X = index into ghosting color table.
!zone {
.COLORS !byte   GREY2,GREY1
.DEST = gfxs_c_DISPLAY_BASE+21*40+1 

clubs_s_ghost_carry
    lda .COLORS,x
    ; Three chars will be affected, starting at the rightmost edge.  Stop
    ; when index becomes negative.
    ldx #2
-
    sta .DEST,x
    dex
    bpl -
    rts
; end sub clubs_s_ghost_carry
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

clubs_c_SIZE = *-clubs_c_BEGIN
