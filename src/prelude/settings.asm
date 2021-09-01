; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
SETTINGS_HOLES_18           = 0
SETTINGS_HOLES_FRONT_NINE   = 1
SETTINGS_HOLES_BACK_NINE    = 2

; For weather, bunkers and greens.
SETTINGS_EASY   = 0
SETTINGS_SO_SO  = 1
SETTINGS_HARD   = 2

SETTINGS_DATA_BASE_ADDR = $a000
SETTINGS_DATA_RECORD_SIZE = 365
settings_l_ICON_ADDR_LO   
    !byte   <(SETTINGS_DATA_BASE_ADDR)   
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*1)
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*2)
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*3)
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*4)
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*5)
    ; Next 3 refer to alternative globe icons.
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*6)
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*7)
    !byte   <(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*8)
settings_l_ICON_ADDR_HI   
    !byte   >(SETTINGS_DATA_BASE_ADDR)   
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*1)
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*2)
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*3)
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*4)
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*5)
    ; And alternative globe icons.
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*6)
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*7)
    !byte   >(SETTINGS_DATA_BASE_ADDR+SETTINGS_DATA_RECORD_SIZE*8)

; Sprite numbers for 'settings' sprites.
SETTINGS_SPR_BASE = 64
settings_c_SPR_OOB_DISALLOWED = SETTINGS_SPR_BASE
SETTINGS_SPR_MATCH      = SETTINGS_SPR_BASE+1
SETTINGS_SPR_STROKE     = SETTINGS_SPR_BASE+2
SETTINGS_SPR_ONE        = SETTINGS_SPR_BASE+3
SETTINGS_SPR_NINE       = SETTINGS_SPR_BASE+4
SETTINGS_SPR_TEN        = SETTINGS_SPR_BASE+5
SETTINGS_SPR_EIGHTEEN   = SETTINGS_SPR_BASE+6
SETTINGS_SPR_EASY       = SETTINGS_SPR_BASE+7
SETTINGS_SPR_SOSO       = SETTINGS_SPR_BASE+8
SETTINGS_SPR_HARD       = SETTINGS_SPR_BASE+9
SETTINGS_SPR_TEARS      = SETTINGS_SPR_BASE+10

; Index this table with the value of settings_scoring to get the correct
; sprite pointer.
SETTINGS_SCORING_SPRITES    !byte   SETTINGS_SPR_STROKE,SETTINGS_SPR_MATCH
SETTINGS_SMILEY_SPRITES     !byte   SETTINGS_SPR_EASY,SETTINGS_SPR_SOSO,SETTINGS_SPR_HARD
SETTINGS_TEARS_POS_Y    !byte   0,0,SETTINGS_SMILIES_POS_Y 

; Encode where the cursor can move from each icon.  Lower nybble tells you
; whether can move up (bit #0), down, left and right.  Upper nybble shows 
; how far you move up (bits #4/5) or down (bits #6/7) - add or subtract
; these values from current index to get the new index.
SETTINGS_CURSOR_MOVES   
    !byte   $42,$9b,$86,$29,$2d,$04
SETTINGS_UP_MASK    = %00110000
SETTINGS_DOWN_MASK  = %11000000
; Whether you can go from each icon to the 'hamburger':
settings_l_TO_HAMBURGER !byte   1,1,0,1,0,0

; Two lookup tables to determine which sprites to show for 'holes' setting
; (i.e. 'from' ... 'to').
SETTINGS_HOLE_FROM  !byte   SETTINGS_SPR_ONE,SETTINGS_SPR_ONE,SETTINGS_SPR_TEN
SETTINGS_HOLE_TO    !byte   SETTINGS_SPR_EIGHTEEN,SETTINGS_SPR_NINE,SETTINGS_SPR_EIGHTEEN
settings_c_HOLE_FROM_X_POS = 100+8
settings_c_HOLE_FROM_Y_POS = 122+8
settings_c_HOLE_TO_X_POS = 110+8
settings_c_HOLE_TO_Y_POS = 146+8
settings_c_SCORING_SPR_X_POS = $28+8
settings_c_SCORING_SPR_Y_POS = $73+8

; NOTE: must add 'tears' sprite for 'hard'.
SETTINGS_DIFFICULTY !byte   SETTINGS_SPR_EASY,SETTINGS_SPR_SOSO,SETTINGS_SPR_HARD

SETTINGS_SMILIES_POS_X  !byte   60+8,116+8,172+8   
SETTINGS_SMILIES_POS_Y = 200+8 

; Highlighting the current icon.
SETTINGS_HIGHLIGHT_NUM_OFFSETS = 20
SETTINGS_HIGHLIGHT_OFFSETS
    !byte   0,1,2,3,4,5,40,45,80,85,120,125,160,165,200,201,202,203,204,205
SETTINGS_HIGHLIGHT_BASE_ADDR_LO
    !byte   <(COLOR_RAM+(2*40)+2)
    !byte   <(COLOR_RAM+(9*40)+2)
    !byte   <(COLOR_RAM+(9*40)+9)
    !byte   <(COLOR_RAM+(16*40)+2)
    !byte   <(COLOR_RAM+(16*40)+9)
    !byte   <(COLOR_RAM+(16*40)+16)
SETTINGS_HIGHLIGHT_BASE_ADDR_HI
    !byte   >(COLOR_RAM+(2*40)+2)
    !byte   >(COLOR_RAM+(9*40)+2)
    !byte   >(COLOR_RAM+(9*40)+9)
    !byte   >(COLOR_RAM+(16*40)+2)
    !byte   >(COLOR_RAM+(16*40)+9)
    !byte   >(COLOR_RAM+(16*40)+16)
SETTINGS_PULSE_DELAY = 8
SETTINGS_PULSE_COLORS   !byte   CYAN,LIGHT_BLUE,BLUE,LIGHT_BLUE,CYAN,WHITE
SETTINGS_PULSE_N = 6

settings_c_MYTEXT
!byte 34,13,19,20,14,18,127,19,0,1,11,4,19,18,255

settings_c_NUM_ICONS = 6    ;7

settings_c_MODE_ICONS       = 0
settings_c_MODE_HAMBURGER   = 1
settings_c_MODE_EXIT        = 2

settings_c_GLOBE_POS_SPR_BASE_PTR = 94
settings_c_GLOBE_POS_SPR_END_PTR = 94+4
settings_c_GLOBE_POS_SPR_OFFSET_X = (-3)
settings_c_GLOBE_POS_SPR_OFFSET_Y = (-3)
settings_c_GLOBE_BASE_X = spr_c_VISIBLE_ALL_L+settings_c_GLOBE_POS_SPR_OFFSET_X+16
settings_c_GLOBE_BASE_Y = spr_c_VISIBLE_ALL_T+settings_c_GLOBE_POS_SPR_OFFSET_Y+16
settings_l_GLOBE_ICONS  !byte   0,0,6,7,8,8
settings_l_GLOBE_POSX
    !byte   settings_c_GLOBE_BASE_X+19
    !byte   settings_c_GLOBE_BASE_X+18
    !byte   settings_c_GLOBE_BASE_X+16
    !byte   settings_c_GLOBE_BASE_X+25
    !byte   settings_c_GLOBE_BASE_X+16
    !byte   settings_c_GLOBE_BASE_X+23
settings_l_GLOBE_POSY
    !byte   settings_c_GLOBE_BASE_Y+12
    !byte   settings_c_GLOBE_BASE_Y+18
    !byte   settings_c_GLOBE_BASE_Y+21
    !byte   settings_c_GLOBE_BASE_Y+24
    !byte   settings_c_GLOBE_BASE_Y+13
    !byte   settings_c_GLOBE_BASE_Y+12

settings_l_BADGES_ADDR_LO
    !byte   <(prelude_c_BADGES_BASE)
    !byte   <(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*1)
    !byte   <(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*2)
    !byte   <(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*3)
    !byte   <(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*4)
    !byte   <(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*5)
settings_l_BADGES_ADDR_HI
    !byte   >(prelude_c_BADGES_BASE)
    !byte   >(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*1)
    !byte   >(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*2)
    !byte   >(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*3)
    !byte   >(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*4)
    !byte   >(prelude_c_BADGES_BASE+SETTINGS_DATA_RECORD_SIZE*5)
settings_c_CLUB_NAMES_0
    !raw    "Beechings Golf & Country Club, England"
settings_c_CLUB_NAMES_1
    !raw    "Sierre des Alpes, Switzerland"
settings_c_CLUB_NAMES_2
    !raw    "Al Ab Golf Club, Egypt"
settings_c_CLUB_NAMES_3
    !raw    "Hokusai Falls, Japan"
settings_c_CLUB_NAMES_4
    !raw    "Carver Invitational '87, USA"
settings_c_CLUB_NAMES_5
    !raw    "New York Golf Club, USA"
settings_l_CLUB_NAMES_ADDR_LO
    !byte   <settings_c_CLUB_NAMES_0
    !byte   <settings_c_CLUB_NAMES_1
    !byte   <settings_c_CLUB_NAMES_2
    !byte   <settings_c_CLUB_NAMES_3
    !byte   <settings_c_CLUB_NAMES_4
    !byte   <settings_c_CLUB_NAMES_5
settings_l_CLUB_NAMES_ADDR_HI
    !byte   >settings_c_CLUB_NAMES_0
    !byte   >settings_c_CLUB_NAMES_1
    !byte   >settings_c_CLUB_NAMES_2
    !byte   >settings_c_CLUB_NAMES_3
    !byte   >settings_c_CLUB_NAMES_4
    !byte   >settings_c_CLUB_NAMES_5
settings_l_CLUB_NAME_LENS   !byte   38,29,22,20,28,23


; *****************
; *** VARIABLES ***
; *****************
settings_cursor_pos !byte   0
settings_pulse_i    !byte   0
settings_pulse_count    !byte   0
settings_v_current_mode !byte 0
settings_v_must_redraw_club_badge   !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
settings_init
    ; Go to multicolor bitmap mode & clear screen!
    +utils_m_enable_bitmap_mode 
    +utils_m_enable_multicolor_mode 

    ; FIXME: parameterize this for different app modes?!
    ; Global colors.
    lda #BLACK
    sta SPMC0
    lda #GREY1
    sta SPMC1

    jsr settings_draw_all_icons
    jsr settings_s_init_sprites

    ; Highlighting.
    ; Base address of current cursor/icon into CAMERA0-CAMERA1.
    ldx #0 ;settings_cursor_pos
    stx settings_cursor_pos
    stx settings_v_must_redraw_club_badge
    lda SETTINGS_HIGHLIGHT_BASE_ADDR_LO,x
    sta CAMERA0
    lda SETTINGS_HIGHLIGHT_BASE_ADDR_HI,x
    sta CAMERA1
    lda #SETTINGS_PULSE_DELAY 
    sta settings_pulse_count    
    lda #0
    sta settings_pulse_i

    jsr prelude_s_draw_hamburger

    lda #settings_c_MODE_ICONS
    sta settings_v_current_mode

    jsr settings_s_set_message_color

    rts
; end sub settings_init
} ; !zone

; **************************************************

!zone {
.ICON_COUNT = MATHS2

settings_draw_all_icons
    lda #settings_c_NUM_ICONS-1
    sta .ICON_COUNT

-
    ldx .ICON_COUNT
    lda settings_l_ICON_ADDR_LO,x   
    sta P0
    lda settings_l_ICON_ADDR_HI,x   
    sta P1
    lda #$ff
    sta P2
    jsr icon_s_draw
    dec .ICON_COUNT
    bpl -

    rts
; end sub settings_draw_all_icons
} ; !zone

; **************************************************

!zone {
settings_s_loop
    lda settings_v_current_mode
    cmp #settings_c_MODE_EXIT
    beq .end
    lda settings_v_must_redraw_club_badge
    +branch_if_false settings_s_loop
    jsr settings_s_draw_club_badge
    jmp settings_s_loop

.end
    rts

; end sub settings_s_loop
} ; !zone

; **************************************************

!zone {
settings_refresh_cursor
    ; Restore cyan to the last icon's border.
    ldx #SETTINGS_HIGHLIGHT_NUM_OFFSETS-1
-
    lda SETTINGS_HIGHLIGHT_OFFSETS,x
    tay
    lda #GREY1
    sta (CAMERA0),y
    dex
    bpl -

    ; NOTE: load new cursor pos into Y!!!
    ldy settings_cursor_pos
    lda SETTINGS_HIGHLIGHT_BASE_ADDR_LO,y
    sta CAMERA0
    lda SETTINGS_HIGHLIGHT_BASE_ADDR_HI,y
    sta CAMERA1

    +prelude_m_browse_sfx

    rts
; end sub settings_refresh_cursor
} ; !zone

; **************************************************

; INPUTS:   Y = cursor pos   
!zone {
settings_move_cursor_down
    lda SETTINGS_CURSOR_MOVES,y
    and #SETTINGS_DOWN_MASK
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    clc
    adc settings_cursor_pos
    sta settings_cursor_pos
    rts
; end sub settings_move_cursor_down
} ; !zone

; **************************************************

; INPUTS:   Y = cursor pos   
!zone {
settings_move_cursor_up
    lda SETTINGS_CURSOR_MOVES,y
    and #SETTINGS_UP_MASK
    lsr
    lsr
    lsr
    lsr
    sta P0
    lda settings_cursor_pos
    sec
    sbc P0
    sta settings_cursor_pos
    rts
; end sub settings_move_cursor_up
} ; !zone

; **************************************************

!zone {
settings_s_init_sprites
    lda #YELLOW
    sta spr_v_color+1
    lda #1
    sta spr_v_xxpand+1
    sta spr_v_hires+1
    lda #settings_c_SCORING_SPR_X_POS 
    sta spr_v_x_lo+1
    lda #settings_c_SCORING_SPR_Y_POS 
    sta spr_v_y+1
    ldx shared_v_scoring
    lda SETTINGS_SCORING_SPRITES,x    
    sta spr_v_current_ptr+1

    ; Holes (i.e. 9 or 18; front or back 9?).
    lda #GREY1
    sta spr_v_color+2
    sta spr_v_color+3
    lda #settings_c_HOLE_FROM_X_POS 
    sta spr_v_x_lo+2
    lda #settings_c_HOLE_FROM_Y_POS 
    sta spr_v_y+2
    lda #settings_c_HOLE_TO_X_POS 
    sta spr_v_x_lo+3
    lda #settings_c_HOLE_TO_Y_POS 
    sta spr_v_y+3
    ldx shared_v_holes
    lda SETTINGS_HOLE_FROM,x  
    sta spr_v_current_ptr+2
    lda SETTINGS_HOLE_TO,x  
    sta spr_v_current_ptr+3
    lda #0
    sta spr_v_hires+2
    sta spr_v_hires+3

    ; Sand traps, greens and wind.
    ; Be sure to set the correct pointer.
    ldx #2
-
    lda shared_v_sand_traps_difficulty,x
    tay
    lda SETTINGS_SMILEY_SPRITES,y
    sta spr_v_current_ptr+8,x
    lda SETTINGS_TEARS_POS_Y,y
    sta spr_v_y+11,x
    lda SETTINGS_SMILIES_POS_X,x
    sta spr_v_x_lo+8,x
    sta spr_v_x_lo+11,x
    dex
    bpl -
    
    lda #YELLOW
    sta spr_v_color+8
    sta spr_v_color+9
    sta spr_v_color+10
    lda #SETTINGS_SMILIES_POS_Y
    sta spr_v_y+8
    sta spr_v_y+9
    sta spr_v_y+10
    ; Tears...
    lda #SETTINGS_SPR_TEARS
    sta spr_v_current_ptr+11
    sta spr_v_current_ptr+12
    sta spr_v_current_ptr+13
    lda #LIGHT_BLUE
    sta spr_v_color+11
    sta spr_v_color+12
    sta spr_v_color+13

    ; All 'smiley' sprites [8,13] are multicolor.
    ldx #8
    lda #0
-
    sta spr_v_hires,x
    inx
    cpx #14
    bne -

    ; Use sprite #0 for the global position indicator.
    ldx shared_v_course_index
    lda #BLACK
    sta spr_v_color
    lda settings_l_GLOBE_POSX,x
    sta spr_v_x_lo
    lda settings_l_GLOBE_POSY,x
    sta spr_v_y
    lda #settings_c_GLOBE_POS_SPR_BASE_PTR
    sta spr_v_current_ptr
    lda #1
    sta spr_v_hires
    lda #0
    sta spr_v_xxpand
    sta spr_v_yxpand
    sta spr_v_x_hi

    jsr settings_s_draw_globe
    jsr settings_s_draw_club_badge

    rts
; end sub settings_s_init_sprites
} ; !zone

; **************************************************

!zone {
settings_draw_upper_sprites
    lda #$0f
    sta SPENA

    ldx #0
    ldy #0
    jsr spr_s_write_to_vic_ii
    ldx #1
    ldy #1
    jsr spr_s_write_to_vic_ii
    ldx #2
    ldy #2
    jsr spr_s_write_to_vic_ii
    ldx #3
    ldy #3
    jsr spr_s_write_to_vic_ii

    ; We will always need to draw the sprites, but we'll pulse the icon only
    ; when we're in 'ICONS' mode.
    lda settings_v_current_mode
    cmp #settings_c_MODE_ICONS
    beq .icons_pulse
    cmp #settings_c_MODE_HAMBURGER
    beq .hamburger_pulse
    ; Must be MODE_EXIT, so nothing to do here.
    rts ; EXIT POINT.

.icons_pulse
    jsr settings_s_pulse_highlight
    rts ; EXIT POINT.

.hamburger_pulse
    jsr prelude_s_update_hamburger_pulse
    bcc .end
    jsr settings_s_pulse_globe_pos

.end
    rts
; end sub settings_draw_upper_sprites
} ; !zone

; **************************************************

!zone {
settings_draw_lower_sprites
    lda #$fc
    sta SPENA

    ldy #8
    ldx #5
    jsr spr_s_write_to_vic_ii
    ldy #9
    ldx #6
    jsr spr_s_write_to_vic_ii
    ldy #10
    ldx #7
    jsr spr_s_write_to_vic_ii

    ldy #11
    ldx #2
    jsr spr_s_write_to_vic_ii
    ldy #12
    ldx #3
    jsr spr_s_write_to_vic_ii
    ldy #13
    ldx #4
    jsr spr_s_write_to_vic_ii

    rts
; end sub settings_draw_lower_sprites
} ; !zone

; **************************************************

!zone {
settings_s_handle_fire
    +prelude_m_select_sfx

    ; What happens depends on our current cursor position.
    lda settings_cursor_pos
    beq .course
    cmp #1
    beq .scoring
    cmp #2
    beq .holes
    
    ; Smilies!
    jsr settings_handle_smilies
    rts ; EXIT POINT.

.course
    jsr settings_s_next_course
    rts ; EXIT POINT.

.scoring
    lda shared_v_scoring
    eor #$01
    sta shared_v_scoring
    tax
    lda SETTINGS_SCORING_SPRITES,x    
    sta spr_v_current_ptr+1
    rts ; EXIT POINT.

.holes
    ldy shared_v_holes
    iny
    cpy #3
    bne +
    ldy #0
+
    sty shared_v_holes
    lda SETTINGS_HOLE_FROM,y
    sta spr_v_current_ptr+2
    lda SETTINGS_HOLE_TO,y
    sta spr_v_current_ptr+3
    rts

; end sub settings_s_handle_fire
} ; !zone

; **************************************************

; INPUTS:   A = cursor pos.
!zone {
settings_handle_smilies
    ; Subtract 5 from cursor pos and store in Y.  Offset for smilie (0-2).
    sec
    sbc #3
    tay

    lda shared_v_sand_traps_difficulty,y
    clc
    adc #1
    cmp #3
    bne +
    lda #0
+
    sta shared_v_sand_traps_difficulty,y
    pha
    ; Consider settings value (easy, soso or hard) to be a delta value that we
    ; add to SPR_EASY to get the correct pointer.
    clc
    adc #SETTINGS_SPR_EASY
    sta spr_v_current_ptr+8,y

    pla
    cmp #2
    bne .no_tears
    lda #SETTINGS_SMILIES_POS_Y
    bne +
.no_tears
    lda #0
+
    sta spr_v_y+11,y

    +prelude_m_select_sfx

    rts
; end sub settings_handle_smilies
} ; !zone

; **************************************************

!zone {
settings_s_pulse_highlight
    dec settings_pulse_count
    bne .end
    lda #SETTINGS_PULSE_DELAY 
    sta settings_pulse_count

    ; What's the next color in the cycle?  Put it into MATHS7.
    ldx settings_pulse_i
    inx
    cpx #6
    bne +
    ldx #0
+   stx settings_pulse_i
    lda SETTINGS_PULSE_COLORS,x
    sta MATHS7

    ; Use X as offset into 'offsets' table.
    ldx #SETTINGS_HIGHLIGHT_NUM_OFFSETS-1

.loop
    lda SETTINGS_HIGHLIGHT_OFFSETS,x
    tay
    lda MATHS7
    sta (CAMERA0),y
    dex
    bpl .loop

    jsr settings_s_pulse_globe_pos

.end
    rts
; end sub settings_s_pulse_highlight
} ; !zone

; **************************************************

; NOTE: LEFT has already been locked when this routine is called.
!zone {
settings_s_handle_left
    ldy settings_cursor_pos
    lda SETTINGS_CURSOR_MOVES,y
    and #joy_c_LEFT
    beq +
    dec settings_cursor_pos
    jsr settings_refresh_cursor
    rts ; EXIT POINT.

+
    ; Maybe can move to 'hamburger' from here?
    lda settings_l_TO_HAMBURGER,y
    beq .end
    ; To hamburger!
    +prelude_m_browse_sfx
    lda #settings_c_MODE_HAMBURGER
    sta settings_v_current_mode
    jsr settings_s_disable_cursor
    sec
    jsr prelude_s_highlight_hamburger
   
.end
    rts
; end sub settings_s_handle_left
} ; !zone

; **************************************************

!zone {
settings_s_handle_controls_hamburger
    ldx #joy_c_PORT2 

    +joy_m_is_fire
    bne .check_right
    +joy_m_is_locked_fire
    +branch_if_true .end
    +joy_m_lock_fire
    +prelude_m_back_sfx
    lda #settings_c_MODE_EXIT
    sta settings_v_current_mode
    lda #prelude_c_MODE_TITLES
    sta prelude_v_mode_after_fade
    rts ; EXIT POINT.

.check_right
    +joy_m_is_right
    bne .end
    +joy_m_is_locked_right
    +branch_if_true .end
    +joy_m_lock_right
    lda #settings_c_MODE_ICONS
    sta settings_v_current_mode
    jsr settings_refresh_cursor
    clc
    jsr prelude_s_highlight_hamburger

.end
    rts
; end sub settings_s_handle_controls_hamburger
} ; !zone

; **************************************************

; NOTE: this simply sets the current icon's border to GREY1.
!zone {
settings_s_disable_cursor
    ldx #SETTINGS_HIGHLIGHT_NUM_OFFSETS-1
-
    lda SETTINGS_HIGHLIGHT_OFFSETS,x
    tay
    lda #GREY1
    sta (CAMERA0),y
    dex
    bpl -

    rts
; end sub settings_s_disable_cursor
} ; !zone

; **************************************************

!zone {
settings_s_update
    jsr settings_draw_upper_sprites

    lda settings_v_current_mode
    cmp #settings_c_MODE_ICONS
    beq .icons
    cmp #settings_c_MODE_HAMBURGER
    beq .hamburger
    ; So must be settings_c_MODE_EXIT...
    rts ; EXIT POINT.

.hamburger
    jsr settings_s_handle_controls_hamburger
    rts ; EXIT POINT.

.icons
    ; Listen out for joystick (#2) moves.
    ldx #joy_c_PORT2
    +joy_m_is_up
    beq .handle_up
    +joy_m_is_down
    beq .handle_down
    +joy_m_is_left
    beq .handle_left
    +joy_m_is_right
    beq .handle_right
    +joy_m_is_fire
    beq .handle_fire

    ; No movement.
    jsr joy_s_release_all_locks
    rts ; EXIT POINT.

.handle_up
    +joy_m_is_locked_up
    bne +
    ; Can move up?
    ldy settings_cursor_pos
    lda SETTINGS_CURSOR_MOVES,y
    and #joy_c_UP
    beq +   ; No need to lock?
    jsr settings_move_cursor_up
    +joy_m_lock_up 
    jsr settings_refresh_cursor
+
    rts ; EXIT POINT.

.handle_down
    +joy_m_is_locked_down
    bne +
    ; Can move down?
    ldy settings_cursor_pos
    lda SETTINGS_CURSOR_MOVES,y
    and #joy_c_DOWN
    beq +
    jsr settings_move_cursor_down
    +joy_m_lock_down
    jsr settings_refresh_cursor
+
    rts ; EXIT POINT.

.handle_left
    +joy_m_is_locked_left
    +branch_if_true +
    ; Can move left?
    +joy_m_lock_left
    jsr settings_s_handle_left
    rts ; EXIT POINT.

.handle_right
    +joy_m_is_locked_right
    bne +
    ; Can move right?
    ldy settings_cursor_pos
    lda SETTINGS_CURSOR_MOVES,y
    and #joy_c_RIGHT
    beq +
    inc settings_cursor_pos
    +joy_m_lock_right
    jsr settings_refresh_cursor
+
    rts ; EXIT POINT.

.handle_fire
    ; What happens depends on cursor position...
    +joy_m_is_locked_fire
    bne +
    +joy_m_lock_fire
    jsr settings_s_handle_fire
+
    rts

; end sub settings_s_update
} ; !zone

; **************************************************

!zone {
settings_s_next_course
    ldx shared_v_course_index
    inx
    cpx #shared_c_NUM_COURSES
    bne +
    ldx #0
+
    stx shared_v_course_index

    ; Update position of globe position sprite!
    lda settings_l_GLOBE_POSX,x 
    sta spr_v_x_lo
    lda settings_l_GLOBE_POSY,x
    sta spr_v_y
    ; NOTE: X still holds course index.
    jsr settings_s_draw_globe

    inc settings_v_must_redraw_club_badge
    
    rts

; end sub settings_s_next_course
} ; !zone

; **************************************************

!zone {
settings_s_pulse_globe_pos
    ; NOTE: this operation affects sprite #0.
    ldx spr_v_current_ptr
    inx
    cpx #settings_c_GLOBE_POS_SPR_END_PTR 
    bne +
    ldx #settings_c_GLOBE_POS_SPR_BASE_PTR
+
    stx spr_v_current_ptr

    rts
; end sub settings_s_pulse_globe_pos
} ; !zone

; **************************************************

; INPUTS:   X = course index.
!zone {
settings_s_draw_globe
    ldy settings_l_GLOBE_ICONS,x
    lda settings_l_ICON_ADDR_LO,y   
    sta P0
    lda settings_l_ICON_ADDR_HI,y   
    sta P1
    lda #$ff
    sta P2
    jsr icon_s_draw
    rts
; end sub settings_s_draw_globe
} ; !zone

; **************************************************

!zone {
settings_s_draw_club_badge
    ldx shared_v_course_index
    lda settings_l_BADGES_ADDR_LO,x
    sta P0
    lda settings_l_BADGES_ADDR_HI,x
    sta P1
    lda #$ff
    sta P2
    jsr icon_s_draw

    jsr msg_s_clear
    ldx shared_v_course_index
    lda settings_l_CLUB_NAMES_ADDR_LO,x
    sta P0
    lda settings_l_CLUB_NAMES_ADDR_HI,x
    sta P1
    lda settings_l_CLUB_NAME_LENS,x
    sta P4
    jsr msg_s_display

    ; And clear this flag now that we've finished drawing:
    lda #0
    sta settings_v_must_redraw_club_badge

    rts
; end sub settings_s_draw_club_badge
} ; !zone

; **************************************************

!zone {
.BASE = gfxs_c_DISPLAY_BASE+(24*40)
.BASE2 = COLOR_RAM+(24*40)

settings_s_set_message_color
    ldx #39
-
    lda .BASE,x
    and #$f0
    ora #GREY3
    sta .BASE,x
    lda #BLACK
    sta .BASE2,x
    dex
    bpl -

    rts
; end sub settings_s_set_message_color
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

