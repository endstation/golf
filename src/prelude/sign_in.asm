; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
sign_l_WINDOWS2
    !bin "../../assets/pictures/window2.bin"
sign_l_END_OF_WINDOW_ICON
    !bin "../../assets/pictures/end_of_window.bin"

sign_c_MODE_PLAYER_SELECTION    = 0
sign_c_MODE_EDITING_NAME        = 1
sign_c_MODE_EDITING_GENDER      = 2
sign_c_MODE_EDITING_CONTROL     = 3
sign_c_MODE_HAMBURGER           = 4
sign_c_MODE_EXIT                = 5

sign_l_TEXT_ADDR_LO
    !byte   <gfxs_c_BITMAP_BASE+(3*40*8)+(5*8)
    !byte   <gfxs_c_BITMAP_BASE+(8*40*8)+(5*8)
    !byte   <gfxs_c_BITMAP_BASE+(15*40*8)+(5*8)
    !byte   <gfxs_c_BITMAP_BASE+(20*40*8)+(5*8)
sign_l_TEXT_ADDR_HI
    !byte   >gfxs_c_BITMAP_BASE+(3*40*8)+(5*8)
    !byte   >gfxs_c_BITMAP_BASE+(8*40*8)+(5*8)
    !byte   >gfxs_c_BITMAP_BASE+(15*40*8)+(5*8)
    !byte   >gfxs_c_BITMAP_BASE+(20*40*8)+(5*8)
; Addresses of the name texts in screen RAM.
sign_l_TEXT_ADDR_SR_LO
    !byte   <gfxs_c_DISPLAY_BASE+(3*40)+5 
    !byte   <gfxs_c_DISPLAY_BASE+(8*40)+5 
    !byte   <gfxs_c_DISPLAY_BASE+(15*40)+5 
    !byte   <gfxs_c_DISPLAY_BASE+(20*40)+5 
sign_l_TEXT_ADDR_SR_HI
    !byte   >gfxs_c_DISPLAY_BASE+(3*40)+5 
    !byte   >gfxs_c_DISPLAY_BASE+(8*40)+5 
    !byte   >gfxs_c_DISPLAY_BASE+(15*40)+5 
    !byte   >gfxs_c_DISPLAY_BASE+(20*40)+5 

sign_l_DOTS_CR_ADDR_LO
    !byte   <gfxs_c_DISPLAY_BASE+(3*40)+3
    !byte   <gfxs_c_DISPLAY_BASE+(8*40)+3
    !byte   <gfxs_c_DISPLAY_BASE+(15*40)+3
    !byte   <gfxs_c_DISPLAY_BASE+(20*40)+3
sign_l_DOTS_CR_ADDR_HI
    !byte   >gfxs_c_DISPLAY_BASE+(3*40)+3
    !byte   >gfxs_c_DISPLAY_BASE+(8*40)+3
    !byte   >gfxs_c_DISPLAY_BASE+(15*40)+3
    !byte   >gfxs_c_DISPLAY_BASE+(20*40)+3

sign_c_DOTS_CR_LO = EDGES_LO
sign_c_DOTS_CR_HI = EDGES_HI

; i.e. write this byte eight times:
sign_c_CURSOR_CHAR = $ab

; NOTE: delay = how long before repeating begins; freq = frames between
; each 'repeat'.
sign_c_KEY_REPEAT_DELAY = 27
sign_c_KEY_REPEAT_FREQ  = 4

sign_c_SPR_BASE = 86
sign_c_SPR_MALE_SHIRT   = sign_c_SPR_BASE 
sign_c_SPR_FEMALE_SHIRT = sign_c_SPR_BASE+1
sign_c_SPR_MALE_SKIN_SHADOW = sign_c_SPR_BASE+2
sign_c_SPR_FEMALE_SKIN_SHADOW = sign_c_SPR_BASE+3
sign_c_SPR_JOYSTICK_PORT2 = sign_c_SPR_BASE+4
sign_c_SPR_JOYSTICK_PORT1 = sign_c_SPR_BASE+5
sign_c_SPR_BORDER_A = sign_c_SPR_BASE+6
sign_c_SPR_BORDER_B = sign_c_SPR_BASE+7

sign_c_MALE_PORTRAIT_ICON = 0
sign_c_FEMALE_PORTRAIT_ICON = 1
sign_c_JOYSTICK_ICON = 2

sign_l_PORTRAIT_ICONS
!bin "../../assets/pictures/portrait_icons.bin"
sign_l_PORTRAIT_SIZE_BYTES = 125
sign_l_PORTRAITS_ADDR_LO
    !byte   <sign_l_PORTRAIT_ICONS
    !byte   <(sign_l_PORTRAIT_ICONS+(1*sign_l_PORTRAIT_SIZE_BYTES))
    !byte   <(sign_l_PORTRAIT_ICONS+(2*sign_l_PORTRAIT_SIZE_BYTES))
sign_l_PORTRAITS_ADDR_HI
    !byte   >sign_l_PORTRAIT_ICONS
    !byte   >(sign_l_PORTRAIT_ICONS+(1*sign_l_PORTRAIT_SIZE_BYTES))
    !byte   >(sign_l_PORTRAIT_ICONS+(2*sign_l_PORTRAIT_SIZE_BYTES))

sign_c_NUM_WINDOWS = 4
sign_l_WINDOW_COLORS    !byte   CYAN,LIGHT_BLUE,YELLOW,LIGHT_GREEN
sign_l_WINDOW_ROWS !byte   2*8,7*8,14*8,19*8
sign_c_PORTRAIT_COL_PIXELS = 16*4
sign_c_JOYSTICK_COL_PIXELS = 19*4

; 48 bytes.  12 bytes per configuration: male light, female light, male dark,
; female dark.  Write these values to video RAM.
sign_l_SKIN_COLORS
!bin "../../assets/pictures/skin_colors.bin"
sign_l_PORTRAITS_VRAM_BASE_LO
    !byte   <(gfxs_c_DISPLAY_BASE+(2*40)+16) 
    !byte   <(gfxs_c_DISPLAY_BASE+(7*40)+16) 
    !byte   <(gfxs_c_DISPLAY_BASE+(14*40)+16) 
    !byte   <(gfxs_c_DISPLAY_BASE+(19*40)+16) 
sign_l_PORTRAITS_VRAM_BASE_HI
    !byte   >(gfxs_c_DISPLAY_BASE+(2*40)+16) 
    !byte   >(gfxs_c_DISPLAY_BASE+(7*40)+16) 
    !byte   >(gfxs_c_DISPLAY_BASE+(14*40)+16) 
    !byte   >(gfxs_c_DISPLAY_BASE+(19*40)+16) 
sign_l_SKIN_COLOR_OFFSETS   !byte   0,1,2,40,41,42,80,81,82,120,121,122
sign_l_SKIN_COLOR_SRC_LO
    !byte   <sign_l_SKIN_COLORS
    !byte   <(sign_l_SKIN_COLORS+(1*12))
    !byte   <(sign_l_SKIN_COLORS+(2*12))
    !byte   <(sign_l_SKIN_COLORS+(3*12))
sign_l_SKIN_COLOR_SRC_HI
    !byte   >sign_l_SKIN_COLORS
    !byte   >(sign_l_SKIN_COLORS+(1*12))
    !byte   >(sign_l_SKIN_COLORS+(2*12))
    !byte   >(sign_l_SKIN_COLORS+(3*12))
sign_l_SKIN_SHADOWS !byte   RED,BROWN

; NOTE: listed in groups of 4 (for each slot).
sign_l_SW_SPR
    !byte   0,4,12
    !byte   1,5,13
    !byte   2,6,14
    !byte   3,7,15
sign_l_HW_SPR
    !byte   0,1,3
    !byte   4,5,7
    !byte   0,1,3
    !byte   4,5,7
; Table of offsets into above two tables for each slot.
; NOTE: in each case, the FOLLOWING byte holds the end condition.
sign_l_SW_HW_SPR_OFFSETS    !byte   0,3,6,9,12

sign_l_SLOT_NUMBERS_DATA
    !byte   $ef,$af,$ef,$ef,$ef,$ef,$ef,$ab
    !byte   $ef,$bb,$bb,$fb,$ef,$bf,$bf,$ab
    !byte   $ef,$bb,$fb,$ef,$fb,$bb,$bb,$ef
    !byte   $bf,$bf,$bf,$bf,$bb,$ab,$fb,$fb

; Which sprites to enable if slot is occupied.
sign_l_HW_SPR_ENABLE    !byte   $0b,$b0,$0b,$b0

sign_l_CURSOR_PULSE_COLORS
    !byte   BLACK,BLUE,VIOLET,LIGHT_BLUE,CYAN,LIGHT_BLUE,VIOLET,BLUE,$ff
    !byte   BLACK,BLUE,VIOLET,GREY2,LIGHT_BLUE,GREY2,VIOLET,BLUE,$ff
    !byte   BLACK,BROWN,ORANGE,GREY2,YELLOW,GREY2,ORANGE,BROWN,$ff
    !byte   BLACK,BROWN,ORANGE,GREEN,LIGHT_GREEN,GREEN,ORANGE,BROWN,$ff
; I.e. offsets into the above table.
sign_l_CURSOR_PULSE_OFFSETS !byte   0,9,18,27
sign_c_CURSOR_PULSE_DELAY = 7

sign_l_BORDER_A_SPR_Y   !byte   $46,$6e,$a6,$ce
sign_l_BORDER_B_SPR_Y   !byte   $5b,$83,$bb,$e3
sign_c_BORDER_SPR_X_PLAYER      = $98
sign_c_BORDER_SPR_X_JOYSTICK    = $b0
; NOTE: index into this table is 'sign_v_current_mode'.
sign_l_BORDER_SPR_X = * - sign_c_MODE_EDITING_GENDER      
    !byte   $98,$b0

sign_l_TEXT_ROWS    !byte   3*8,8*8,15*8,20*8
sign_c_TEXT_COL = 5*4


; *****************
; *** VARIABLES ***
; *****************
sign_v_current_player   !byte   0
; This is valid only after call to 'goto_next/prev_player', where it is set:
sign_v_previous_current_player  !byte   0
sign_v_current_mode     !byte   0
sign_v_keyboard_locked  !byte   0
sign_v_last_keypress    !byte   0
sign_v_key_repeat_on    !byte   0
sign_v_key_repeat_count !byte   0
; Pulsing cursor.
sign_v_cursor_pulse_i       !byte   0
sign_v_cursor_pulse_count   !byte   0
sign_v_cursor_is_pulsing    !byte   0
; Border sprites.
sign_v_border_is_active     !byte   0

sign_v_shirts_taken     !fill   shared_c_NUM_SHIRT_COLORS,0


; *******************
; ****** MACROS *****
; *******************
!macro sign_m_set_key_repeat_count .delay {
    lda #.delay
    sta sign_v_key_repeat_count
} ; sign_m_set_key_repeat_count 


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
.iter   !byte   0

sign_s_init
    ; NOTE: as a precaution, move all h/w sprites offscreen lhs (to prevent
    ; any flickering).
    ldx #16
    lda #0
-
    sta SP0X,x
    dex
    bpl -
    
    sta sign_v_current_player
    sta sign_v_current_mode
    sta sign_v_key_repeat_on
    lda #$ff
    sta sign_v_last_keypress

    sec
    jsr sign_s_set_dots_ptr
    jsr sign_s_draw_windows
    jsr sign_s_draw_numbers
    jsr sign_s_init_sprites
    jsr sign_s_populate_all_slots
    jsr sign_s_reset_pulse

    jsr prelude_s_draw_hamburger

    rts
; end sub sign_s_init
} ; !zone

; **************************************************

!zone {
sign_s_loop
    ; Return if we're in EXIT mode.
    lda sign_v_current_mode
    cmp #sign_c_MODE_EXIT
    bne sign_s_loop
    lda #prelude_c_MODE_TITLES
    sta prelude_v_mode_after_fade
    rts
; end sub sign_s_loop
} ; !zone

; **************************************************

!zone {
sign_s_handle_player_selection
    ldx #joy_c_PORT2 

    +joy_m_is_up
    bne .check_down
    +joy_m_is_locked_up
    +branch_if_true .exit1
    +joy_m_lock_up
    jsr sign_s_goto_prev_player 
.exit1
    rts ; EXIT POINT.

.check_down
    +joy_m_is_down
    bne .check_left
    +joy_m_is_locked_down
    +branch_if_true .exit2
    +joy_m_lock_down
    jsr sign_s_goto_next_player
.exit2
    rts ; EXIT POINT.

.check_left
    +joy_m_is_left
    bne .check_fire
    +joy_m_is_locked_left
    +branch_if_true .exit3
    +joy_m_lock_left
    +prelude_m_browse_sfx
    lda #sign_c_MODE_HAMBURGER
    sta sign_v_current_mode
    jsr sign_s_reset_number
    sec
    jsr prelude_s_highlight_hamburger
.exit3
    rts ; EXIT POINT.

.check_fire
    ; Check fire button.
    +joy_m_is_fire
    bne .unlock_fire
    +joy_m_is_locked_fire
    +branch_if_true .exit4
    +joy_m_lock_fire
    +prelude_m_select_sfx
    jsr sign_s_start_editing_name
.exit4
    rts ; EXIT POINT.

.unlock_fire
    jsr joy_s_release_all_locks

    ; No joystick activity so look at keyboard.
    jsr SCNKEY
    ldx SFDX
    lda KB_MATRIX_DECODE_TBL,x
    bmi .unlock_keyboard
    ; Acknowledge only if keyboard unlocked.
    ldx sign_v_keyboard_locked
    bne .end
    ; PETSCII code in accumulator.
    ; We're listening out for: 1-4 (swap) and DEL (with or without C= key).
    jsr sign_s_handle_keyboard_player_select
    inc sign_v_keyboard_locked
    rts ; EXIT POINT.

.unlock_keyboard
    +clr sign_v_keyboard_locked

.end
    rts
; end sub sign_s_handle_player_selection
} ; !zone

; **************************************************

!zone {
sign_s_goto_next_player
    ; Nothing to do if zero players.
    lda shared_v_num_players
    bne +
    rts ; EXIT POINT.

+
    ldx sign_v_current_player
    stx sign_v_previous_current_player  
    cpx shared_v_num_players
    beq .to_zero
    inx
    cpx #shared_c_MAX_PLAYERS 
    bne +
.to_zero
    ldx #0
+
    stx sign_v_current_player
    clc
    jsr sign_s_set_dots_ptr
    jsr sign_s_reset_pulse

    +prelude_m_browse_sfx
    rts
; end sub sign_s_goto_next_player
} ; !zone

; **************************************************

!zone {
sign_s_goto_prev_player
    ; Nothing to do if <= 1 player.
    lda shared_v_num_players
    bne +
    rts ; EXIT POINT.

+
    ldx sign_v_current_player
    stx sign_v_previous_current_player  
    dex
    bpl +

    ; Wraparound - so go to first unoccupied slot or MAX_PLAYERS-1, whichever
    ; is smaller.
    ldx shared_v_num_players
    cpx #shared_c_MAX_PLAYERS 
    bcc +
    dex

+
    stx sign_v_current_player
    clc
    jsr sign_s_set_dots_ptr
    jsr sign_s_reset_pulse

    +prelude_m_browse_sfx
    rts
; end sub sign_s_goto_prev_player
} ; !zone

; **************************************************

; INPUTS:   C flag set - not yet initialized so don't reset currently
;               flashing dots; C flag clear - do reset them.
!zone {
sign_s_set_dots_ptr
    bcs +

    ldy #0
    ldx sign_v_previous_current_player  
    lda #BLACK ;sign_l_SLOT_NUMBER_COLORS,x   
    sta (sign_c_DOTS_CR_LO),y

+
    ldx sign_v_current_player
    lda sign_l_DOTS_CR_ADDR_LO,x
    sta sign_c_DOTS_CR_LO 
    lda sign_l_DOTS_CR_ADDR_HI,x
    sta sign_c_DOTS_CR_HI 

    rts
; end sub sign_s_set_dots_ptr
} ; !zone

; **************************************************

!zone {
sign_s_update
    jsr sign_s_draw_upper_sprites

    ; FIXME: this used to be in sign_s_loop!
    lda sign_v_current_mode
    cmp #sign_c_MODE_PLAYER_SELECTION
    bne +
    jsr sign_s_handle_player_selection
    jmp .key_repeat
+
    cmp #sign_c_MODE_EDITING_NAME
    bne +
    jsr sign_s_handle_edit_name
    jmp .key_repeat
+
    cmp #sign_c_MODE_EDITING_GENDER
    bne +
    jsr sign_s_handle_edit_gender
    jmp .pulse
+
    cmp #sign_c_MODE_EDITING_CONTROL
    bne +
    jsr sign_s_handle_edit_control
    jmp .pulse
+
    cmp #sign_c_MODE_HAMBURGER
    bne +
    jsr sign_s_handle_hamburger
    jmp .pulse
+
    ; Exit mode?  Nothing to do in that case.
    rts ; EXIT POINT.

.key_repeat
    lda sign_v_key_repeat_on
    +branch_if_false .pulse
    dec sign_v_key_repeat_count
    bne .pulse
    ; Reset repeat-count to the lower value and re-process the last keypress.
    ; The PETSCII value is stored in last_keypress variable.
sign_short_delay
    +sign_m_set_key_repeat_count sign_c_KEY_REPEAT_FREQ  
    ldx sign_v_last_keypress
    jsr sign_s_handle_keypress

.pulse
    jsr sign_s_update_cursor_pulse

.end
    rts
; end sub sign_s_update
} ; !zone

; **************************************************

!zone {
.iter   !byte   0

sign_s_populate_all_slots
    ldx #shared_c_MAX_PLAYERS-1 
.loop
    stx .iter
    jsr sign_s_populate_slot
    ldx .iter
    dex
    bpl .loop

    rts
; end sub sign_s_populate_all_slots
} ; !zone

; **************************************************

; INPUTS:   X = slot number.
!zone {
.slot_num   !byte   0

sign_s_populate_slot
    ; If slot number is >= number of players, we are erasing rather than
    ; populating!
    cpx shared_v_num_players
    bcc .populate
    jsr sign_s_erase_name
    jsr sign_s_erase_icons
    rts ; EXIT POINT.

.populate
    stx .slot_num
    jsr sign_s_erase_name_from_bitmap

    ; Name first.
    lda shared_v_player_name_lens,x
    sta P4
    lda sign_l_TEXT_ROWS,x
    sta P2
    lda #sign_c_TEXT_COL
    sta P3
    lda shared_v_player_name_indices,x
    tax
    lda shared_l_NAME_ADDR_LO,x 
    sta P0
    lda shared_l_NAME_ADDR_HI,x 
    sta P1
    jsr font_s_draw_text

    ; Then the icons...
    ; Portrait.  Male or female?
    ldx .slot_num
    jsr sign_s_draw_portrait
    ldx .slot_num
    jsr sign_s_draw_shirt
    ldx .slot_num
    jsr sign_s_draw_joystick
    ldx .slot_num
    jsr sign_s_set_name_text_color

    rts
; end sub sign_s_populate_slot
} ; !zone

; **************************************************

; Set initial position of cursor before start editing name.
!zone {
sign_s_set_cursor_pos
    ; NOTE: remember each char of the player's name occupies 8 bytes of bitmap.
    ldx sign_v_current_player
    lda shared_v_player_name_lens,x
    asl
    asl
    asl
    clc
    adc sign_l_TEXT_ADDR_LO,x
    sta CURSOR_POS_LO
    lda sign_l_TEXT_ADDR_HI,x
    adc #0
    sta CURSOR_POS_HI

    lda sign_l_TEXT_ADDR_SR_LO,x
    sta CURSOR_POS_SR_LO 
    lda sign_l_TEXT_ADDR_SR_HI,x
    sta CURSOR_POS_SR_HI 

    rts
; end sub sign_s_set_cursor_pos
} ; !zone

; **************************************************

!zone {
sign_s_draw_cursor
    ldy #7
    lda #sign_c_CURSOR_CHAR 
-
    sta (CURSOR_POS_LO),y
    dey
    bpl -
    rts
; end sub sign_s_draw_cursor
} ; !zone

; **************************************************

; INPUTS:   C flag set = move forward; C flag clear = move back
!zone {
sign_s_move_cursor_one_place
    bcs .forward

    ; When moving back, we will first delete the cursor in its current
    ; position.
    jsr sign_s_erase_cursor

    lda CURSOR_POS_LO
    sec
    sbc #8
    sta CURSOR_POS_LO
    lda CURSOR_POS_HI
    sbc #0
    sta CURSOR_POS_HI
    jmp .redraw

.forward
    lda CURSOR_POS_LO
    clc
    adc #8
    sta CURSOR_POS_LO
    lda CURSOR_POS_HI
    adc #0
    sta CURSOR_POS_HI

    ; Put the current display RAM char back to black.
    ldx sign_v_current_player
    lda shared_v_player_name_lens,x
    tay
    dey
    lda #BLACK
    sta (CURSOR_POS_SR_LO),y

.redraw
    jsr sign_s_draw_cursor
    
    rts
; end sub sign_s_move_cursor_one_place
} ; !zone

; **************************************************

!zone {
sign_s_handle_edit_name
    ; Scan the keyboard using Kernal routine.
    jsr SCNKEY
    ldx SFDX
    lda KB_MATRIX_DECODE_TBL,x
    bpl +

    ; No keypress detected.
    ldx #$ff
    stx sign_v_last_keypress
    inx
    stx sign_v_key_repeat_on 
    rts ; EXIT POINT.

+
    ; PETSCII code is in accumulator. 
    cmp sign_v_last_keypress
    beq .end
    sta sign_v_last_keypress
    tax
    ; New key so initialize key repeat.
    lda #1
    sta sign_v_key_repeat_on
sign_long_delay
    +sign_m_set_key_repeat_count sign_c_KEY_REPEAT_DELAY
    jsr sign_s_handle_keypress

.end
    rts
; end sub sign_s_handle_edit_name
} ; !zone

; **************************************************

; INPUTS:   X = PETSCII code.
!zone {
.name_index !byte 0

sign_s_handle_keypress
    ; Two special cases: DEL ($14) and RETURN ($0d).
    cpx #$14
    beq .delete
    cpx #$0d
    beq .return
    bne .check_alphabetic

.delete
    jsr sign_s_handle_delete
    rts ; EXIT POINT.

.return
    jsr sign_s_handle_return
    rts ; EXIT POINT.

.check_alphabetic
    ; FIXME: for the moment, accept only alphabetic characters (a-z).  These
    ; have PETSCII codes in range [65,91).
    cpx #92
    bcs .end
    cpx #65
    bcc .end
    stx MATHS0

    ; No further characters allowed if the name is already at MAX_LEN.
    ldx sign_v_current_player
    lda shared_v_player_name_lens,x
    ; FIXME: why hard-coded?!
    cmp #10 ;shared_c_MAX_NAME_LEN 
    bne +
    ; Name already at maximum length.
    +prelude_m_invalid_sfx
    rts ; EXIT POINT.

+
    ; If SHIFT isn't pressed, add 32 to the ASCII code to get lower case.
    lda SHFLAG
    and #$01
    bne +
    lda MATHS0
    clc
    adc #32
    sta MATHS0

+
    ; X already holds current player index.
    lda shared_v_player_name_indices,x
    tay
    ; Source of text into P0-P1.
    lda shared_l_NAME_ADDR_LO,y
    sta P0
    lda shared_l_NAME_ADDR_HI,y
    sta P1
    lda shared_v_player_name_lens,x
    tay
    lda MATHS0
    sta (P0),y
    inc shared_v_player_name_lens,x
    iny
    sty P4
    ; TODO: redraw name & cursor!
    ; P2 and P3 should hold destination of text.  Use 'bitmap' coordinates for
    ; row and then column.
    lda sign_l_TEXT_ROWS,x
    sta P2
    lda #sign_c_TEXT_COL
    sta P3
    jsr font_s_draw_text
    sec
    jsr sign_s_move_cursor_one_place
    ; Text was entered, so make 'typing' sound!
    +prelude_m_type_sfx

.end
    rts
; end sub sign_s_handle_keypress
} ; !zone

; **************************************************

!zone {
sign_s_handle_delete
    ldx sign_v_current_player
    lda shared_v_player_name_lens,x
    ; Nothing to do if name length is 0...
    bne +
    +prelude_m_invalid_sfx
    rts ; EXIT POINT.
    
+
    lda SHFLAG
    and #$02
    beq .one_char
    ; When C= key and INST/DEL pressed at the same time, delete the whole of
    ; the name in one go.
    jsr sign_s_delete_name
    rts ; EXIT POINT.

.one_char
    dec shared_v_player_name_lens,x
    ; Just need to move cursor back one space (to delete end character).
    clc
    jsr sign_s_move_cursor_one_place
    +prelude_m_delete_sfx

.end
    rts
; end sub sign_s_handle_delete
} ; !zone

; **************************************************

!zone {
sign_s_handle_return
    jsr sign_s_erase_cursor
    lda #0
    sta sign_v_cursor_is_pulsing
    sta sign_v_key_repeat_on    

    ; If the user has pressed [RETURN] on an empty string, this has the effect
    ; of deleting the current player.
    ldx sign_v_current_player
    lda shared_v_player_name_lens,x
    beq .delete_player

    lda #sign_c_MODE_EDITING_GENDER
    sta sign_v_current_mode
    jsr sign_s_init_icon_border
    rts ; EXIT POINT.

.delete_player
    jsr sign_s_delete_player
    lda #sign_c_MODE_PLAYER_SELECTION
    sta sign_v_current_mode

    rts
; end sub sign_s_handle_return
} ; !zone

; **************************************************

!zone {
sign_s_erase_cursor
    ldy #7
    lda #$ff
-
    sta (CURSOR_POS_LO),y
    dey
    bpl -

    rts
; end sub sign_s_erase_cursor
} ; !zone

; **************************************************

!zone {
sign_s_delete_name
    ; Make sure to erase cursor first!
    jsr sign_s_erase_cursor

    ldx sign_v_current_player

    lda #0
    sta shared_v_player_name_lens,x

    lda sign_l_TEXT_ADDR_LO,x
    sta MATHS0
    sta CURSOR_POS_LO
    lda sign_l_TEXT_ADDR_HI,x
    sta MATHS1
    sta CURSOR_POS_HI
    lda #$ff    ; 'clear' byte
    ; We can begin erasing at the second character, because the cursor will
    ; overwrite the first one!
    ldy #8
-
    sta (MATHS0),y
    iny
    cpy #(10*8)
    bne -

    jsr sign_s_draw_cursor

    rts
; end sub sign_s_delete_name
} ; !zone

; **************************************************

!zone {
.X !byte $9a,$9a,$9a,$9a,$9a,$9a,$9a,$9a,0,0,0,0,$b0,$b0,$b0,$b0
.Y !byte $48,$70,$a8,$d0,$48,$70,$a8,$d0,0,0,0,0,$46,$6e,$a6,$ce

sign_s_init_sprites
    lda #GREY1
    sta SPMC0   

    ; Initializing sprites in range: [0,16)
    ; First the positions.
    ldx #0
-
    lda .X,x
    sta spr_v_x_lo,x
    lda #0
    sta spr_v_x_hi,x
    ; All these sprites are multicolor.
    sta spr_v_hires,x
    lda .Y,x
    sta spr_v_y,x
    inx
    cpx #16
    bne -

    ; All joystick sprites (12-15) should be WHITE.
    lda #WHITE
    sta spr_v_color+12
    sta spr_v_color+13
    sta spr_v_color+14
    sta spr_v_color+15

    ; Set the data pointers for 'border' sprites A and B.
    lda #sign_c_SPR_BORDER_A
    sta spr_v_current_ptr+8
    lda #sign_c_SPR_BORDER_B
    sta spr_v_current_ptr+9

    rts
; end sub sign_s_init_sprites
} ; !zone

; **************************************************

!zone {
sign_s_draw_upper_sprites
    lda #0
    sta SPENA

    ldx #0
    jsr sign_s_draw_sprites_for_slot
    ldx #1
    jsr sign_s_draw_sprites_for_slot

    lda sign_v_border_is_active
    +branch_if_false .end
    lda SPENA
    ora #$44
    sta SPENA
    ldy #8
    ldx #2
    jsr spr_s_write_to_vic_ii
    ldy #9
    ldx #6
    jsr spr_s_write_to_vic_ii

.end
    rts
; end sub sign_s_draw_upper_sprites
} ; !zone

; **************************************************

!zone {
sign_s_handle_edit_gender
    ldx #joy_c_PORT2 

    +joy_m_is_up
    bne .check_down
    +joy_m_is_locked_up
    +branch_if_true .end
    +joy_m_lock_up
    jsr sign_s_cycle_gender
    rts ; EXIT POINT.

.check_down
    +joy_m_is_down
    bne .check_left
    +joy_m_is_locked_down
    +branch_if_true .end
    +joy_m_lock_down
    jsr sign_s_cycle_skin_tone
    rts ; EXIT POINT.

.check_left
    +joy_m_is_left
    bne .check_right
    +joy_m_is_locked_left
    +branch_if_true .end
    +joy_m_lock_left
    jsr sign_s_previous_shirt
    rts ; EXIT POINT.

.check_right
    +joy_m_is_right
    bne .check_fire
    +joy_m_is_locked_right
    +branch_if_true .end
    +joy_m_lock_right
    jsr sign_s_next_shirt
    rts ; EXIT POINT.

.check_fire
    +joy_m_is_fire
    bne .unlock
    +joy_m_is_locked_fire
    +branch_if_true .end
    +joy_m_lock_fire
    lda #sign_c_MODE_EDITING_CONTROL
    sta sign_v_current_mode     
    jsr sign_s_init_icon_border
    rts ; EXIT POINT.

.unlock
    ; No joystick events so unlock everything.
    jsr joy_s_release_all_locks

.end
    rts
; end sub sign_s_handle_edit_gender
} ; !zone

; **************************************************

!zone {
sign_s_cycle_skin_tone
    +prelude_m_browse_sfx

    ldx sign_v_current_player
    ; Change skin tone.
    lda shared_v_player_skin_tones,x
    eor #$01
    sta shared_v_player_skin_tones,x
    jsr sign_s_draw_skin_tone

    rts
; end sub sign_s_cycle_skin_tone
} ; !zone

; **************************************************

!zone {
sign_s_handle_edit_control
    ldx #joy_c_PORT2 

    +joy_m_is_right
    bne .check_fire
    +joy_m_is_locked_right
    +branch_if_true .end
    +joy_m_lock_right
    jsr sign_s_cycle_joystick
    rts ; EXIT POINT.

.check_fire
    +joy_m_is_fire
    bne .unlock
    +joy_m_is_locked_fire
    +branch_if_true .end
    +joy_m_lock_fire
    
    +prelude_m_select_sfx

    ; Finished editing player - back to selection...
    lda #sign_c_MODE_PLAYER_SELECTION
    sta sign_v_current_mode
    ; Automatically advance to the next player (unless this is slot #3).
    ldx sign_v_current_player
    jsr sign_s_take_shirt
    inx
    cpx #shared_c_MAX_PLAYERS 
    beq .no_wraparound
    stx sign_v_current_player
    sec
    jsr sign_s_set_dots_ptr
    jsr sign_s_reset_pulse
.no_wraparound
    +clr sign_v_border_is_active
    rts ; EXIT POINT.

.unlock
    jsr joy_s_release_all_locks
    
.end
    rts
; end sub sign_s_handle_edit_control
} ; !zone

; **************************************************

!zone {
sign_s_cycle_joystick
    +prelude_m_browse_sfx

    ldx sign_v_current_player
    lda shared_v_player_joysticks,x
    eor #$01
    sta shared_v_player_joysticks,x
    clc
    adc #sign_c_SPR_JOYSTICK_PORT2
    sta spr_v_current_ptr+12,x
    rts
; end sub sign_s_cycle_joystick
} ; !zone

; **************************************************

!zone {
sign_s_cycle_gender
    +prelude_m_browse_sfx

    ldx sign_v_current_player
    lda shared_v_player_genders,x
    eor #$01
    sta shared_v_player_genders,x
    jsr sign_s_draw_portrait
    rts
; end sub sign_s_cycle_gender
} ; !zone

; **************************************************

!zone {
.win_iter !byte 0

sign_s_draw_windows
    ldx #3
.loop_top
    stx .win_iter

    lda sign_l_WINDOW_ROWS,x
    sta sign_l_WINDOWS2
    lda #(2*4)
    sta sign_l_WINDOWS2+1
    lda #<sign_l_WINDOWS2
    sta P0
    lda #>sign_l_WINDOWS2
    sta P1
    lda sign_l_WINDOW_COLORS,x
    sta P2
    jsr icon_s_draw

    ldx .win_iter
    dex
    bpl .loop_top

    rts
; end sub sign_s_draw_windows
} ; !zone

; **************************************************

; INPUTS:   X = current player.
!zone {
.PLAYER_NUM = WS_X_LO

sign_s_draw_portrait
    stx .PLAYER_NUM

    ; Gender code will be either 0 (male) or 1 (female).  Push onto stack for
    ; later use.
    lda shared_v_player_genders,x
    pha

    ; Transfer this to Y and use as index into table of base addresses for 
    ; portrait icons.  We must write the destination address (row,col) directly
    ; into that memory block.
    tay
    lda sign_l_PORTRAITS_ADDR_LO,y
    sta P0
    lda sign_l_PORTRAITS_ADDR_HI,y
    sta P1
    ldy #0
    lda sign_l_WINDOW_ROWS,x
    sta (P0),y
    iny
    lda #sign_c_PORTRAIT_COL_PIXELS
    sta (P0),y
    lda sign_l_WINDOW_COLORS,x
    sta P2
    jsr icon_s_draw

    ; Safe to assume that gender may have changed, so make sure the shirt 
    ; and 'skin shadow' sprites are pointing to the correct data blocks.
    ; Gender is on top of stack.
    ldx .PLAYER_NUM
    pla
    pha
    clc
    adc #sign_c_SPR_MALE_SHIRT
    sta spr_v_current_ptr,x
    pla
    clc
    adc #sign_c_SPR_MALE_SKIN_SHADOW
    sta spr_v_current_ptr+4,x

    jsr sign_s_draw_skin_tone

    rts
; end sub sign_s_draw_portrait
} ; !zone

; **************************************************

; INPUTS:   X = current player.
!zone {
sign_s_draw_shirt
    ; Actually, we're just setting the shirt sprite's color and data ptr.
    ; Shirt sprites are in range [0,4).
    ldy shared_v_player_shirt_color_indices,x
    lda shared_l_PLAYER_SHIRT_COLORS,y    
    sta spr_v_color,x
    lda shared_v_player_genders,x
    clc
    adc #sign_c_SPR_MALE_SHIRT
    sta spr_v_current_ptr,x
    rts
; end sub sign_s_draw_shirt
} ; !zone

; **************************************************

; INPUTS:   X = current player
!zone {
.PLAYER_NUM = WS_X_LO

sign_s_draw_joystick
    stx .PLAYER_NUM

    ; Set location for joystick icon - write this directly to the 
    ; memory block.
    lda #<(sign_l_PORTRAIT_ICONS+(2*sign_l_PORTRAIT_SIZE_BYTES))
    sta P0
    lda #>(sign_l_PORTRAIT_ICONS+(2*sign_l_PORTRAIT_SIZE_BYTES))
    sta P1
    ldy #0
    lda sign_l_WINDOW_ROWS,x
    sta (P0),y
    iny
    lda #sign_c_JOYSTICK_COL_PIXELS 
    sta (P0),y
    lda sign_l_WINDOW_COLORS,x
    sta P2
    jsr icon_s_draw
    
    ; Overlay 'number' sprite.
    ldx .PLAYER_NUM
    lda shared_v_player_joysticks,x           
    ; Accumulator will now hold either 0 (port 2) or 1 (port 1).
    clc
    adc #sign_c_SPR_JOYSTICK_PORT2 
    ; Base spr num for joysticks is 12 (FIXME: use symbolic constant!)
    sta spr_v_current_ptr+12,x

    rts
; end sub sign_s_draw_joystick
} ; !zone

; **************************************************

; INPUTS:   X = current player
!zone {
.DEST_LO = P0
.SRC_LO = P2
.ITER = MATHS0
.PLAYER_NUM = MATHS2

sign_s_draw_skin_tone
    stx .PLAYER_NUM

    ; Destination into P0-P1.
    lda sign_l_PORTRAITS_VRAM_BASE_LO,x
    sta .DEST_LO
    lda sign_l_PORTRAITS_VRAM_BASE_HI,x
    sta .DEST_LO+1

    ; Temp store in MATHS1 for later use...
    lda shared_v_player_skin_tones,x
    sta MATHS1
    ; To get an index into the source table, multiply skin tone by 2 and add
    ; gender.  This should give a value in the range [0,4).
    asl
    clc
    adc shared_v_player_genders,x
    tay
    lda sign_l_SKIN_COLOR_SRC_LO,y
    sta .SRC_LO
    lda sign_l_SKIN_COLOR_SRC_HI,y
    sta .SRC_LO+1

    ldy #0
.loop
    sty .ITER
    lda (.SRC_LO),y
    pha
    lda sign_l_SKIN_COLOR_OFFSETS,y
    tay
    pla
    sta (.DEST_LO),y
    ; Again?
    ldy .ITER
    iny
    cpy #12
    bne .loop

    ; Still need to set skin shadow color - either RED or BROWN.
    ; MATHS1 holds skin tone index.  Shadow sprite numbers are 4-7.
    ldx MATHS1
    lda sign_l_SKIN_SHADOWS,x
    ldx .PLAYER_NUM 
    sta spr_v_color+4,x

    rts
; end sub sign_s_draw_skin_tone
} ; !zone

; **************************************************

; INPUTS:   X = slot.
!zone {
.count  !byte   0
.END = MATHS0

sign_s_draw_sprites_for_slot
    ; Unless slot is occupied, nothing to do!
    cpx shared_v_num_players    
    bcc +
    rts ; EXIT POINT.
+
    
    ; Enable the appropriate h/w sprites.
    lda SPENA
    ora sign_l_HW_SPR_ENABLE,x
    sta SPENA

    ; First get the index for the end of the loop and store.
    lda sign_l_SW_HW_SPR_OFFSETS+1,x
    sta .END
    ; And now the first index - goes into X.
    lda sign_l_SW_HW_SPR_OFFSETS,x
    tax
.loop
    stx .count

    ; NOTE: Y=from, X=to.
    lda sign_l_SW_SPR,x
    tay
    lda sign_l_HW_SPR,x
    tax
    jsr spr_s_write_to_vic_ii
    
    ldx .count
    inx
    cpx .END
    bne .loop

    rts
; end sub sign_s_draw_sprites_for_slot
} ; !zone

; **************************************************

!zone {
sign_s_draw_lower_sprites
    ; Disable all sprites by default.
    ; NOTE: preserve sprites #2 and #6 if they have been previously enabled.
    lda SPENA
    and #$44
    sta SPENA

    ldx #2
    jsr sign_s_draw_sprites_for_slot
    ldx #3
    jsr sign_s_draw_sprites_for_slot

    lda sign_v_border_is_active
    +branch_if_false .end
    ; Nothing to do except enable the relevant sprite numbers (#2 and #6).
    lda SPENA
    ora #$44
    sta SPENA
.end
    rts
; end sub sign_s_draw_lower_sprites
} ; !zone

; **************************************************

!zone {
sign_s_draw_numbers
    ; Bitmap data.
    ldx #0
.loop
    lda sign_l_SLOT_NUMBERS_DATA,x
    sta gfxs_c_BITMAP_BASE+(3*40*8)+(3*8),x 
    lda sign_l_SLOT_NUMBERS_DATA+8,x
    sta gfxs_c_BITMAP_BASE+(8*40*8)+(3*8),x 
    lda sign_l_SLOT_NUMBERS_DATA+16,x
    sta gfxs_c_BITMAP_BASE+(15*40*8)+(3*8),x 
    lda sign_l_SLOT_NUMBERS_DATA+24,x
    sta gfxs_c_BITMAP_BASE+(20*40*8)+(3*8),x 
    inx
    cpx #8
    bne .loop

    ; Colors.
    lda #BLACK
    sta gfxs_c_DISPLAY_BASE+(3*40)+3 
    sta gfxs_c_DISPLAY_BASE+(8*40)+3 
    sta gfxs_c_DISPLAY_BASE+(15*40)+3 
    sta gfxs_c_DISPLAY_BASE+(20*40)+3 
    rts
; end sub sign_s_draw_numbers
} ; !zone

; **************************************************

; INPUTS:   A = PETSCII code
!zone {
.DEL_KEY = $14
.ASCII_1 = 49
.ASCII_4 = 52

sign_s_handle_keyboard_player_select
    cmp #.DEL_KEY
    beq .delete
    cmp #.ASCII_1
    bcc .end
    cmp #.ASCII_4+1
    bcs .end

    ; It's a number between 1 and 4, so attempt a player swap.
    ; NOTE: subtracting 49 (= ASCII '1') from code gives us an index in
    ; the range [0,4).
    sec
    sbc #.ASCII_1
    sta P0
    jsr sign_s_swap_players
    rts ; EXIT POINT.

.delete
    jsr sign_s_delete_player
    rts ; EXIT POINT.

.end
    rts
; end sub sign_s_handle_keyboard_player_select
} ; !zone

; **************************************************

; INPUTS:   P0 = swap target (in range [0,4)).
!zone {
.ATTR_COUNT = MATHS0
.target !byte 0

sign_s_swap_players
    ; Swap is defined iff:
    ; - target != current
    ; - both target and current are < num players.
    lda P0
    cmp sign_v_current_player
    beq .invalid
    lda shared_v_num_players
    cmp P0
    +ble .invalid
    cmp sign_v_current_player
    +ble .invalid
    
    ; Swap is valid!
    ldx P0
    stx .target
    ldy sign_v_current_player
    lda #0
    sta .ATTR_COUNT

.loop
    lda shared_v_player_name_lens,x
    pha
    lda shared_v_player_name_lens,y
    sta shared_v_player_name_lens,x
    pla
    sta shared_v_player_name_lens,y

    inc .ATTR_COUNT
    lda .ATTR_COUNT
    cmp #7
    beq .done
    ; Increment both indices by 4 (=shared_c_MAX_PLAYERS) so they're ready
    ; for the next attribute.
    txa
    clc
    adc #shared_c_MAX_PLAYERS
    tax
    tya
    clc ; Probably don't need this again...  Save one byte?!
    adc #shared_c_MAX_PLAYERS
    tay
    jmp .loop

.done
    ldx .target
    jsr sign_s_erase_name_from_bitmap
    jsr sign_s_populate_slot
    ldx sign_v_current_player
    jsr sign_s_erase_name_from_bitmap
    jsr sign_s_populate_slot
    +prelude_m_select_sfx

.invalid
    rts
; end sub sign_s_swap_players
} ; !zone

; **************************************************

!zone {
sign_s_delete_player
    ; Is the deletion defined?  Current player should be < number of players.
    ldx sign_v_current_player
    cpx shared_v_num_players    
    bcc .deletion_defined
    +prelude_m_invalid_sfx
    rts ; EXIT POINT.

.deletion_defined
    ; If we're in 'MODE_PLAYER_SELECTION', this player's shirt must be
    ; returned.
    lda sign_v_current_mode
    cmp #sign_c_MODE_PLAYER_SELECTION
    bne +
    jsr sign_s_return_shirt 
+
    dec shared_v_num_players
    ; We don't need to do any 'shuffling up' if player we're deleting was in
    ; the last filled slot.  In this case, the player index will be equal to
    ; the decremented number of players.
    ldx sign_v_current_player
    cpx shared_v_num_players
    beq .repopulate

    ldy sign_v_current_player
    iny
    ; Save name index of player we're about to delete on stack.  We will later
    ; write it into slot #3.
    lda shared_v_player_name_indices,x
    pha
-
    lda shared_v_player_name_indices,y
    sta shared_v_player_name_indices,x
    lda shared_v_player_name_lens,y           
    sta shared_v_player_name_lens,x           
    lda shared_v_player_genders,y             
    sta shared_v_player_genders,x             
    lda shared_v_player_joysticks,y           
    sta shared_v_player_joysticks,x           
    lda shared_v_player_skin_tones,y          
    sta shared_v_player_skin_tones,x          
    lda shared_v_player_shirt_color_indices,y        
    sta shared_v_player_shirt_color_indices,x        
    ; Next and check if at the end.
    inx
    iny
    cpy #shared_c_MAX_PLAYERS 
    bne -

    ; Re-use deleted player's name index in last slot.
    pla
    sta shared_v_player_name_indices+shared_c_MAX_PLAYERS-1 

.repopulate
    ; TODO: won't need to repopulate all slots unless slot #0 was deleted -
    ; and even then depends on how many players there were before...
    jsr sign_s_populate_all_slots

    +prelude_m_delete_player_sfx

    rts
; end sub sign_s_delete_player
} ; !zone

; **************************************************

; INPUTS:   X = slot.
; NOTE: preserves value of X.
!zone {

sign_s_erase_name
    lda #0
    sta shared_v_player_name_lens,x
; NOTE: allow a second entry point into the routine that preserves 
; name length!
sign_s_erase_name_from_bitmap
    lda sign_l_TEXT_ADDR_LO,x
    sta MATHS0
    lda sign_l_TEXT_ADDR_HI,x
    sta MATHS1
    ldy #(shared_c_MAX_NAME_LEN*8)-1 
    lda #$ff
-
    sta (MATHS0),y
    dey
    bpl -

    rts
; end sub sign_s_erase_name
} ; !zone

; **************************************************

; INPUTS:   X = slot.
!zone {
sign_s_erase_icons
    ; First set the row where the icon will be drawn.
    lda sign_l_WINDOW_ROWS,x
    sta sign_l_END_OF_WINDOW_ICON
    lda sign_l_WINDOW_COLORS,x
    sta P2
    lda #<sign_l_END_OF_WINDOW_ICON
    sta P0
    lda #>sign_l_END_OF_WINDOW_ICON
    sta P1
    jsr icon_s_draw

    rts
; end sub sign_s_erase_icons
} ; !zone

; **************************************************

; NOTE: this routine is called when user presses fire button while in
; 'PLAYER_SELECTION' mode.  So the first thing to do here is check whether
; we need to add a new player at all, or simply edit an existing one.
!zone {
sign_s_add_player
    ldx sign_v_current_player
    cpx shared_v_num_players
    bne .existing_player

    inc shared_v_num_players
    jsr sign_s_try_on_first_available_shirt
    jsr sign_s_populate_slot

.existing_player
    ; Returned so we can reselect it while browsing...
    jsr sign_s_return_shirt
    rts
; end sub sign_s_add_player
} ; !zone

; **************************************************

!zone {
sign_s_update_cursor_pulse
    ; Do nothing if hamburger is highlighted...
    lda sign_v_current_mode
    cmp #sign_c_MODE_HAMBURGER
    bne +
    jsr prelude_s_update_hamburger_pulse
    rts ; EXIT POINT.

+
    dec sign_v_cursor_pulse_count
    bne .end
    lda #sign_c_CURSOR_PULSE_DELAY
    sta sign_v_cursor_pulse_count   

    ; Go to the next color.
    ldy sign_v_current_player
    ldx sign_v_cursor_pulse_i
    inx
.try_again
    lda sign_l_CURSOR_PULSE_COLORS,x
    bpl +
    ldx sign_l_CURSOR_PULSE_OFFSETS,y 
    jmp .try_again
+
    stx sign_v_cursor_pulse_i

    ; Color code in accumulator - push onto stack while we get correct
    ; offset into Y.
    pha

    ; What we do now depends on whether we're editing the name or
    ; gender/control.
    lda sign_v_current_mode
    cmp #sign_c_MODE_PLAYER_SELECTION
    beq .player_select
    cmp #sign_c_MODE_EDITING_NAME
    beq .name

    ; So editing gender/control - this means it's the icon border that's
    ; pulsing (two sprites).
    pla
    sta spr_v_color+8
    sta spr_v_color+9
    rts ; EXIT POINT.

.player_select
    pla
    ldy #0
    sta (sign_c_DOTS_CR_LO),y
    rts ; EXIT POINT.
    
.name
    lda shared_v_player_name_lens,y
    tay 
    ; Color code comes off top of stack.
    pla
    sta (CURSOR_POS_SR_LO),y
    
.end
    rts
; end sub sign_s_update_cursor_pulse
} ; !zone

; **************************************************

; INPUTS:   X = slot #.
!zone {
sign_s_set_name_text_color
    lda sign_l_TEXT_ADDR_SR_LO,x
    sta MATHS0
    lda sign_l_TEXT_ADDR_SR_HI,x
    sta MATHS1
    ldy #shared_c_MAX_NAME_LEN 
    lda #BLACK
-
    sta (MATHS0),y
    dey
    bpl -
    rts
; end sub sign_s_set_name_text_color
} ; !zone

; **************************************************

!zone {
sign_s_init_icon_border
    ldx sign_v_current_player
    lda sign_l_BORDER_A_SPR_Y,x
    sta spr_v_y+8
    lda sign_l_BORDER_B_SPR_Y,x
    sta spr_v_y+9
    ; Choose the correct x-position based on current mode.
    ldy sign_v_current_mode
    lda sign_l_BORDER_SPR_X,y
    sta spr_v_x_lo+8
    sta spr_v_x_lo+9

    ; Initialize pulsing.
    lda #sign_c_CURSOR_PULSE_DELAY
    sta sign_v_cursor_pulse_count
    sta sign_v_border_is_active
    ; X still holds current player.
    lda sign_l_CURSOR_PULSE_OFFSETS,x
    sta sign_v_cursor_pulse_i
    tax
    lda sign_l_CURSOR_PULSE_COLORS,x
    sta spr_v_color+8
    sta spr_v_color+9

    +prelude_m_select_sfx

    rts
; end sub sign_s_init_icon_border
} ; !zone

; **************************************************

!zone {
sign_s_reset_pulse
    lda #sign_c_CURSOR_PULSE_DELAY
    sta sign_v_cursor_pulse_count
    ldx sign_v_current_player
    lda sign_l_CURSOR_PULSE_OFFSETS,x
    sta sign_v_cursor_pulse_i
    tax
    lda sign_l_CURSOR_PULSE_COLORS,x
    pha

    ; Color code is in A.  Where we write this depends on current mode.
    lda sign_v_current_mode
    cmp #sign_c_MODE_PLAYER_SELECTION
    beq .player_select
    cmp #sign_c_MODE_EDITING_NAME
    beq .name

    ; So must be editing gender/control...
    pla
    sta spr_v_color+8
    sta spr_v_color+9
    rts ; EXIT POINT.

.player_select
    pla
    ldy #0
    sta (sign_c_DOTS_CR_LO),y
    rts ; EXIT POINT.
    
.name
    lda shared_v_player_name_lens,y
    tay 
    ; Color code comes off top of stack.
    pla
    sta (CURSOR_POS_SR_LO),y

    rts
; end sub sign_s_reset_pulse
} ; !zone

; **************************************************

!zone {
sign_s_handle_hamburger
    ; Fire to go back to main menu.  Right to go back to player selection.
    ldx #joy_c_PORT2 

    +joy_m_is_right
    bne .check_fire
    +joy_m_is_locked_right
    +branch_if_true .end
    +joy_m_lock_right
    +prelude_m_browse_sfx
    lda #sign_c_MODE_PLAYER_SELECTION
    sta sign_v_current_mode
    jsr sign_s_reset_pulse
    clc
    jsr prelude_s_highlight_hamburger
    rts ; EXIT POINT.

.check_fire
    +joy_m_is_fire
    bne .unlock
    +joy_m_is_locked_fire
    +branch_if_true .end
    +joy_m_lock_fire
    +prelude_m_back_sfx
    lda #sign_c_MODE_EXIT
    sta sign_v_current_mode
    rts ; EXIT POINT.

.unlock
    jsr joy_s_release_all_locks

.end
    rts
; end sub sign_s_handle_hamburger
} ; !zone

; **************************************************

!zone {
sign_s_reset_number
    lda #BLACK
    ldy #0
    sta (sign_c_DOTS_CR_LO),y
    rts
; end sub sign_s_reset_number
} ; !zone

; **************************************************

!zone {
sign_s_start_editing_name
    lda #sign_c_MODE_EDITING_NAME
    sta sign_v_current_mode
    lda #1
    sta sign_v_keyboard_locked
    jsr sign_s_add_player
    jsr sign_s_set_cursor_pos
    jsr sign_s_draw_cursor
    jsr sign_s_reset_number
    jsr sign_s_reset_pulse
    rts
; end sub sign_s_start_editing_name
} ; !zone

; **************************************************

; Clear out any 'dead' characters at the end of each name entry.
!zone {
.END = TREES_HI

sign_s_tidy_up_names
    ; X keeps track of slots.
    ldx #0

.loop_top
    lda shared_v_player_name_indices,x
    tay
    lda shared_l_NAME_OFFSETS+1,y
    sta .END
    lda shared_l_NAME_OFFSETS,y
    clc
    adc shared_v_player_name_lens,x
    cmp .END
    beq .next
    tay

    lda #SCR_CODE_SPACE
-
    sta shared_v_player_names,y
    iny
    cpy .END
    bne -

.next
    ; NOTE: X still holds slot #.
    inx
    cpx #4
    bne .loop_top

    rts
; end sub sign_s_tidy_up_names
} ; !zone

; **************************************************

; INPUTS:   X = current player.
!zone {
sign_s_take_shirt
    ldy shared_v_player_shirt_color_indices,x
    lda #1
    sta sign_v_shirts_taken,y
    rts
; end sub sign_s_take_shirt
} ; !zone

; **************************************************

; INPUTS:   X = current player.
!zone {
sign_s_return_shirt
    ldy shared_v_player_shirt_color_indices,x
    lda #0
    sta sign_v_shirts_taken,y
    rts
; end sub sign_s_return_shirt
} ; !zone

; **************************************************

!zone {
sign_s_next_shirt
    ldx sign_v_current_player
    ldy shared_v_player_shirt_color_indices,x

.again
    iny
    ; Either Y will be out of range, or in range and taken/not taken.
    cpy #shared_c_NUM_SHIRT_COLORS
    beq .out_of_range
    lda sign_v_shirts_taken,y
    beq .found
    bne .again

.out_of_range
    ldy #$ff
    bne .again

.found
    jsr sign_s_wear_shirt

    +prelude_m_browse_sfx

    rts
; end sub sign_s_next_shirt
} ; !zone

; **************************************************

!zone {
sign_s_previous_shirt
    ldx sign_v_current_player
    ldy shared_v_player_shirt_color_indices,x

.again
    dey
    ; Either Y will be out of range, or in range and taken/not taken.
    bmi .out_of_range
    lda sign_v_shirts_taken,y
    beq .found
    bne .again

.out_of_range
    ldy #shared_c_NUM_SHIRT_COLORS
    bne .again

.found
    jsr sign_s_wear_shirt

    +prelude_m_browse_sfx

    rts
; end sub sign_s_previous_shirt
} ; !zone

; **************************************************

; INPUTS:   X = current player, Y = shirt index.
; NOTE: 'wear', rather than 'try on', because we're also setting the sprite
; color!
!zone {
sign_s_wear_shirt
    tya
    sta shared_v_player_shirt_color_indices,x
    lda shared_l_PLAYER_SHIRT_COLORS,y
    sta spr_v_color,x
    rts
; end sub sign_s_wear_shirt
} ; !zone

; **************************************************

; OUTPUTS:  Y = shirt index.
!zone {
sign_s_try_on_first_available_shirt
    ldy #0
-
    lda sign_v_shirts_taken,y
    beq .found
    iny
    bne -

.found
    tya
    sta shared_v_player_shirt_color_indices,x

    rts
; end sub sign_s_try_on_first_available_shirt
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

