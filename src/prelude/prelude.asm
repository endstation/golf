; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "prelude.o",cbm
!source "../core/labels.asm"
!source "../core/mymacros.asm"

; Place assets into memory.
*= $6000
!bin "mypic.bin"
prelude_c_END_OF_PIC
*= $a000
!bin "../../assets/settings/icons.bin"
prelude_c_END_OF_ICONS
prelude_c_BADGES_BASE
!bin "../../assets/settings/badges.bin"
*= $c000
prelude_l_BLANK_SPRITE
    !fill   64,0
*= $d000
!bin "../../assets/sprites/settings_sprites.bin"
!bin "../../assets/sprites/titles_options.bin"
!bin "../../assets/sprites/shirts.bin"
prelude_c_GLOBAL_POS_SPRITES
!bin "../../assets/sprites/global_pos_sprites.bin"
prelude_c_END_OF_SPRITES






*= end_of_core
!zone {
prelude_s_loop
    jsr fader_s_prepare_greyscale_table
    jsr spr_s_clear_all_expands
    jsr sfx_s_init
    jsr snd_s_clear_all
    jsr snd_s_set_max_volume

    sec
    jsr titles_s_init
    lda #prelude_c_MODE_TITLES
    sta prelude_v_current_mode
    jsr interrupts_s_install

.loop_top
    lda prelude_v_current_mode
    cmp #prelude_c_MODE_TITLES         
    beq .titles
    cmp #prelude_c_MODE_SIGN_IN
    beq .sign_in
    cmp #prelude_c_MODE_SETTINGS
    beq .settings
    cmp #prelude_c_MODE_FADING
    beq .fading

    ; Must be 'tee off'.
    rts ; EXIT POINT.

.titles
    jsr titles_s_loop
    jsr prelude_s_init_fade
    jmp .loop_top

.sign_in
    jsr sign_s_loop
    jsr prelude_s_init_fade
    jmp .loop_top

.settings
    jsr settings_s_loop
    jsr prelude_s_init_fade
    jmp .loop_top

.fading
    lda fader_v_finished
    +branch_if_false .loop_top
    jsr prelude_s_handle_fade_finished
    jmp .loop_top

; end sub prelude_s_loop
} ; !zone




; *****************
; *** CONSTANTS ***
; *****************
prelude_c_MODE_TITLES           = 0
prelude_c_MODE_SIGN_IN          = 1
prelude_c_MODE_SETTINGS         = 2
prelude_c_MODE_TEE_OFF          = 3
prelude_c_MODE_FADING           = 4

; Composed of four chars arranged in a square (top row a,b; then c,d).
; Use 01 for 'background' color and 11 for lines.
prelude_l_HAMBURGER_ICON_DATA
    !byte   $55,$55,$7f,$7f,$55,$55,$7f,$7f    
    !byte   $55,$55,$f5,$f5,$55,$55,$f5,$f5
    !byte   $55,$55,$7f,$7f,$55,$55,$55,$55
    !byte   $55,$55,$f5,$f5,$55,$55,$55,$55
prelude_l_HAMBURGER_COLRAM !byte GREY2,YELLOW
prelude_c_HAMBURGER_SCRRAM = GREY3<<4
prelude_l_HAMBURGER_PULSE_COLORS
    !byte   WHITE,GREY3,GREY2,GREY1,BLACK,GREY1,GREY2,GREY3,$ff
prelude_c_HAMBURGER_DEFAULT_COLOR_I = 2

prelude_c_LOADING_MSG   !raw    "Loading game code"
prelude_c_LOADING_MSG_LEN = 17


; *****************
; *** VARIABLES ***
; *****************
prelude_v_dummy0  !byte   0
prelude_v_dummy1  !fill   12
prelude_v_current_mode !byte  prelude_c_MODE_TITLES
prelude_v_mode_after_fade   !byte   0

prelude_v_pulse_iter    !byte   0
prelude_v_pulse_count   !byte   0


; *******************
; ****** MACROS *****
; *******************
!macro prelude_m_invalid_sfx {
    ldy #sfx_c_INVALID
    jsr snd_s_init_sfx
} ; prelude_m_invalid_sfx

!macro prelude_m_select_sfx {
    ldy #sfx_c_SELECT
    jsr snd_s_init_sfx
} ; prelude_m_select_sfx 

!macro prelude_m_browse_sfx {
    ldy #sfx_c_BROWSE2
    jsr snd_s_init_sfx
} ; prelude_m_browse_sfx 

!macro prelude_m_type_sfx {
    ldy #sfx_c_TYPE
    jsr snd_s_init_sfx
} ; prelude_m_type_sfx

!macro prelude_m_delete_sfx {
    ldy #sfx_c_DELETE
    jsr snd_s_init_sfx
} ; prelude_m_delete_sfx

!macro prelude_m_back_sfx {
    ldy #sfx_c_BACK
    jsr snd_s_init_sfx
} ; prelude_m_back_sfx

!macro prelude_m_delete_player_sfx {
    ldy #sfx_c_DELETE_PLAYER
    jsr snd_s_init_sfx
} ; prelude_m_delete_player_sfx


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
prelude_s_init_fade
    lda prelude_v_mode_after_fade
    cmp #prelude_c_MODE_TITLES
    beq .back_to_titles
    cmp #prelude_c_MODE_TEE_OFF
    beq .tee_off

    ; For all other target modes, fade down.
    +clr SPENA
    lda #fader_c_DOWN
    sta fader_v_direction   
    lda #fader_c_TYPE_GREYSCALE  
    sta fader_v_type
    jmp +

.tee_off
    +clr SPENA
    ; Fade DOWN to BLACK.
    lda #fader_c_DOWN
    sta fader_v_direction
    lda #fader_c_TYPE_BLACK
    sta fader_v_type
    jmp +

.back_to_titles
    ; RECOLORIZE UP.
    lda #fader_c_UP
    sta fader_v_direction
    lda #fader_c_TYPE_RECOLORIZE
    sta fader_v_type

+
    jsr fader_s_init
    lda #prelude_c_MODE_FADING
    sta prelude_v_current_mode
    inc interrupts_v_must_change_cb
    rts
; end sub prelude_s_init_fade
} ; !zone

; **************************************************

!zone {
prelude_s_handle_fade_finished
    lda prelude_v_mode_after_fade
    sta prelude_v_current_mode

    cmp #prelude_c_MODE_SIGN_IN
    beq .sign_in
    cmp #prelude_c_MODE_SETTINGS
    beq .settings
    cmp #prelude_c_MODE_TEE_OFF
    beq .tee_off
    
    ; Back to titles...
    clc
    jsr titles_s_init
    inc interrupts_v_must_change_cb
    rts ; EXIT POINT.

.sign_in
    jsr sign_s_init
    inc interrupts_v_must_change_cb
    rts ; EXIT POINT.

.settings
    jsr settings_init
    inc interrupts_v_must_change_cb
    rts ; EXIT POINT.

.tee_off
    jsr gfxs_s_clear_bitmap
    jsr prelude_s_display_loading_msg
    jsr snd_s_clear_all
    jsr spr_s_hide_all
    jsr interrupts_s_uninstall
    rts

; end sub prelude_s_handle_fade_finished
} ; !zone

; **************************************************

!zone {
prelude_s_draw_hamburger
    ldx #15
-
    lda prelude_l_HAMBURGER_ICON_DATA,x
    sta gfxs_c_BITMAP_BASE,x 
    lda prelude_l_HAMBURGER_ICON_DATA+16,x
    sta gfxs_c_BITMAP_BASE+(40*8),x 
    dex
    bpl -

    ; And the colors...
    lda prelude_l_HAMBURGER_COLRAM 
    ldx #prelude_c_HAMBURGER_SCRRAM 
    sta COLOR_RAM 
    sta COLOR_RAM+1
    sta COLOR_RAM+40 
    sta COLOR_RAM+41 
    stx gfxs_c_DISPLAY_BASE 
    stx gfxs_c_DISPLAY_BASE+1 
    stx gfxs_c_DISPLAY_BASE+40 
    stx gfxs_c_DISPLAY_BASE+41 

    rts
; end sub prelude_s_draw_hamburger
} ; !zone

; **************************************************

; INPUTS:   C flag clear = 'ghosted'; C flag set = highlighted.
!zone {
prelude_s_highlight_hamburger
    bcc .turn_off

    ; Highlighting turned on - so we must initialize pulsing.
    ; TODO: generic 'pulse' module for whole program...
    lda #prelude_c_HAMBURGER_DEFAULT_COLOR_I
    sta prelude_v_pulse_iter
    lda #sign_c_CURSOR_PULSE_DELAY 
    sta prelude_v_pulse_count
    rts ; EXIT POINT.

.turn_off
    lda #GREY2
    sta COLOR_RAM 
    sta COLOR_RAM+1
    sta COLOR_RAM+40 
    sta COLOR_RAM+41 

    rts
; end sub prelude_s_highlight_hamburger
} ; !zone

; **************************************************

; OUTPUT:   C flag set if color changed; otherwise clear.
!zone {
prelude_s_update_hamburger_pulse
    dec prelude_v_pulse_count
    bne .end

    ; Reset count.
    lda #sign_c_CURSOR_PULSE_DELAY 
    sta prelude_v_pulse_count

    ; Load in the next color.
    ldx prelude_v_pulse_iter
    inx
.again
    lda prelude_l_HAMBURGER_PULSE_COLORS,x
    bpl +
    ldx #0
    beq .again
+
    stx prelude_v_pulse_iter
    ; Next color is in the accumulator.
    sta COLOR_RAM 
    sta COLOR_RAM+1
    sta COLOR_RAM+40 
    sta COLOR_RAM+41 

    sec
    rts ; EXIT POINT.
        
.end
    clc
    rts
; end sub prelude_s_update_hamburger_pulse
} ; !zone

; **************************************************

!zone {
prelude_s_display_loading_msg
    ; Row.
    lda #24
    sta P0
    ; Column.
    lda #0
    sta P1
    ; b/g color (= BLACK = 0).
    sta P3
    ; f/g color.
    lda #GREY3
    sta P2
    ; Length.
    lda #40
    sta P4
    jsr font_s_prepare_colors

    lda #<prelude_c_LOADING_MSG   
    sta P0
    lda #>prelude_c_LOADING_MSG   
    sta P1
    lda #prelude_c_LOADING_MSG_LEN 
    sta P4
    jsr msg_s_display

    rts
; end sub prelude_s_display_loading_msg
} ; !zone

; **************************************************

; NOTE: called only when round will be match play.
!zone {
.USE_COMMA      = MATHS0
.DEST_ITER      = MATHS1
.PLAYER_ITER    = MATHS3
.ONE_PAST_END   = MATHS4
.src_offsets    !byte   0,10,20,30

prelude_s_build_team_names
    lda #font_c_ASCII_SPACE 
    ldx #(shared_c_MAX_TEAM_NAME_LEN*2)-1
-
    sta shared_v_team_names,x
    dex
    bpl -

    ; If there are four players, separate team member names with a comma.
    ldy #0
    ldx shared_v_num_players
    cpx #4
    bne +
    ldy #SCR_CODE_COMMA  
+
    sty .USE_COMMA

    ldy #0
    sty .PLAYER_ITER
    sty .DEST_ITER

    ; NOTE: Y holds .DEST_ITER throughout rest of routine.
    ; Find 'one past end' index for source string.
    ldx .PLAYER_ITER
.loop_top
    stx .PLAYER_ITER

    ; Record offset for start of name.
    tya
    sta shared_v_team_names_offsets_begin,x

    ; Find beginning and (one past) end for current name.
    lda shared_v_player_name_indices,x
    tax
    lda shared_l_NAME_OFFSETS,x
    ; Begin offset onto stack.
    pha
    ; Now find end offset and store in variable.
    ldx .PLAYER_ITER
    clc
    adc shared_v_player_name_lens,x
    sta .ONE_PAST_END
    ; X = source, Y = destination
    pla
    tax

    ; Copy the name.
-
    lda shared_v_player_names,x
    sta shared_v_team_names,y
    inx
    iny
    cpx .ONE_PAST_END
    bne -

    ; Destination onto stack - need it later to record end of player name.
    tya
    pha

    ; Place a comma here?
    ldx .USE_COMMA
    beq .next
    ; Add comma to 0th and 2nd names only.
    lda .PLAYER_ITER
    and #$01
    bne .next
    ; Pull comma code into accumulator.
    txa
    sta shared_v_team_names,y
    iny

.next
    ldx .PLAYER_ITER

    ; Record offset for 'one-past-end' of player's name.
    ; Correct value is on top of stack.
    pla
    inx
    cpx shared_v_num_players
    beq .team_membership
    jsr prelude_s_check_if_next_team
    bne .loop_top
    ; It's the next team, so move destination iter to 21.
    ldy #(shared_c_MAX_NAME_LEN*2)+1 
    bne .loop_top

.team_membership
    ; Just need to adjust value for player #1.
    lda #0
    ldx shared_v_num_players
    cpx #4
    beq +
    lda #1
+
    sta shared_v_team_membership+1

    ; Set begin index for team offsets and lengths.
    lda shared_v_num_players
    cmp #4
    beq .four_players

    ; So two players...
    lda shared_v_player_name_lens
    sta shared_v_team_lens
    lda shared_v_player_name_lens+1
    sta shared_v_team_lens+1

    ; FIXME: temp hack!
    lda #0
    sta shared_v_team_names_offsets_begin+1
    rts ; EXIT POINT.

.four_players
    lda #1  ; For the comma.
    clc
    adc shared_v_player_name_lens
    adc shared_v_player_name_lens+1
    sta shared_v_team_lens
    lda #1  ; For the comma.
    clc
    adc shared_v_player_name_lens+2
    adc shared_v_player_name_lens+3
    sta shared_v_team_lens+1

    ; FIXME: temp hack!
    lda #0
    sta shared_v_team_names_offsets_begin+2
    ldx shared_v_player_name_lens
    inx
    stx shared_v_team_names_offsets_begin+1
    ldx shared_v_player_name_lens+2
    inx
    stx shared_v_team_names_offsets_begin+3

    rts
; end sub prelude_s_build_team_names
} ; !zone

; **************************************************

; A helper routine for prelude_s_build_team_names.
; INPUTS:   X = current player #
; OUTPUTS:  Z flag set if next team (2 players or 4 players AND this is 
;           player #2 (counting 0 to 3)); otherwise Z flag clear.
; NOTE: preserves values of X and Y!
!zone {
prelude_s_check_if_next_team
    lda shared_v_num_players
    cmp #2
    beq .end
    ; So there are 4 players.
    ; Following instruction will set Z flag if player #2.
    cpx #2
.end
    rts
; end sub prelude_s_check_if_next_team
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

!source "titles.asm"
!source "sign_in.asm"
!source "settings.asm"
!source "interrupts.asm"
!source "fader.asm"
!source "sfx.asm"

prelude_END

