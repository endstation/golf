; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************
titles_c_SPR_BASE = 80
titles_c_SPR_PLAYERS0  = titles_c_SPR_BASE 
titles_c_SPR_PLAYERS1  = titles_c_SPR_BASE+1 
titles_c_SPR_SETTINGS0 = titles_c_SPR_BASE+2 
titles_c_SPR_SETTINGS1 = titles_c_SPR_BASE+3 
titles_c_SPR_TEEOFF0   = titles_c_SPR_BASE+4 
titles_c_SPR_TEEOFF1   = titles_c_SPR_BASE+5 
titles_c_SPR_NUM_PLAYERS   = 0

titles_c_SPR_X_LHS = spr_c_VISIBLE_ALL_L+54
titles_c_SPR_X_RHS = spr_c_VISIBLE_ALL_L+54+24
titles_l_SPR_Y !byte   spr_c_VISIBLE_ALL_T+123,spr_c_VISIBLE_ALL_T+132,spr_c_VISIBLE_ALL_T+141
titles_l_SPR_PTRS_LHS  !byte   titles_c_SPR_PLAYERS0,titles_c_SPR_SETTINGS0,titles_c_SPR_TEEOFF0
titles_l_SPR_PTRS_RHS  !byte   titles_c_SPR_PLAYERS1,titles_c_SPR_SETTINGS1,titles_c_SPR_TEEOFF1

titles_c_SPR_NUM_PLAYERS_X = $81
titles_c_SPR_NUM_PLAYERS_Y = $b0

titles_c_TEE_OFF_OK                        = 0
titles_c_TEE_OFF_ERROR_NO_PLAYERS          = 1
titles_c_TEE_OFF_ERROR_INVALID_MATCH_PLAY  = 2
titles_l_ERROR_MSG !raw "There are no golfers yet!You need 2 or 4 golfers for match play!"
titles_l_ERROR_MSG_LENS    !byte   25,39 
titles_l_ERROR_MSG_ADDR_LO !byte   <titles_l_ERROR_MSG,<(titles_l_ERROR_MSG+25)
titles_l_ERROR_MSG_ADDR_HI !byte   >titles_l_ERROR_MSG,>(titles_l_ERROR_MSG+25)

; 'EXIT' refers to exit from loop.
titles_c_EXIT_MSG_NONE              = 0
titles_c_EXIT_MSG_GO_TO_SIGN_IN     = 1
titles_c_EXIT_MSG_GO_TO_SETTINGS    = 2
titles_c_EXIT_MSG_TEE_OFF           = 3


; *****************
; *** VARIABLES ***
; *****************
titles_v_cursor_pos     !byte   0
titles_v_must_exit_loop !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   C flag set = do colorize; C clear = already colorized.
!zone {
titles_s_init
    php
    +utils_m_enable_bitmap_mode 
    +utils_m_enable_multicolor_mode 
    jsr titles_s_draw_bitmap
    plp
    bcc +
    jsr titles_s_colorize_bitmap
+
    jsr titles_s_init_sprites

    ldx #0
    stx titles_v_must_exit_loop
    jsr titles_s_refresh_highlight

    rts
; end sub titles_s_init
} ; !zone

; **************************************************

!zone {
titles_s_loop
    lda titles_v_must_exit_loop
    +branch_if_false titles_s_loop
    rts
    
; end sub titles_s_loop
} ; !zone

; **************************************************

!zone {
titles_s_draw_bitmap
    ; 8000 bytes from $6000 to $e000.
    lda #$00
    sta P0
    lda #$60
    sta P1
    lda #$00
    sta P2
    lda #$e0
    sta P3
    ; 31*256 = 7936
    ldx #0
    ldy #0
-
    lda (P0),y
    sta (P2),y
    iny
    bne -
    inx
    cpx #32
    beq .end
    inc P1
    inc P3
    ; P2 will never be 0 so can use a branch here.
    bne -

.end
    rts

; end sub titles_s_draw_bitmap
} ; !zone

; **************************************************

; FIXME: writes 1024 bytes in each case instead of (the more correct)
; 1000 bytes.
!zone {
titles_s_colorize_bitmap
    ; screen RAM: from $7f40 to gfxs_c_DISPLAY_BASE.
    ; color RAM: from $8328 to COLOR_RAM.
    lda #$40
    sta P0
    lda #$7f
    sta P1
    lda #<gfxs_c_DISPLAY_BASE
    sta P2
    lda #>gfxs_c_DISPLAY_BASE
    sta P3
    lda #$28
    sta P4
    lda #$83
    sta P5
    lda #<COLOR_RAM
    sta P6
    lda #>COLOR_RAM
    sta P7
    ldx #0
    ldy #0
-
    lda (P0),y
    sta (P2),y
    lda (P4),y
    sta (P6),y
    iny
    bne -
    inx
    cpx #4
    beq .end
    inc P1
    inc P3
    inc P5
    inc P7
    jmp -

.end
    rts

; end sub titles_s_colorize_bitmap
} ; !zone

; **************************************************

; NOTE: h/w sprite #6 (the 'num players' sprite) must be handled separately
;       because it doesn't form part of a regular pair.
; INPUTS:   X = old cursor pos
!zone {
titles_s_refresh_highlight
    lda #GREY3
    sta spr_v_color,x
    sta spr_v_color+3,x
    sta spr_v_color+6

    lda #WHITE
    ldx titles_v_cursor_pos
    bne +
    sta spr_v_color+6
+
    sta spr_v_color,x
    sta spr_v_color+3,x

    jsr titles_s_draw_sprites
    rts

; end sub titles_s_refresh_highlight
} ; !zone

; **************************************************

!zone {
titles_s_move_cursor_up
    lda titles_v_cursor_pos
    ; Store in X and Y - we'll manipulate Y and save X for call to 
    ; 'titles_s_refresh_highlight' which needs to know old position.
    tax
    tay
    dey
    bpl +
    ldy #2
+
    sty titles_v_cursor_pos
    jsr titles_s_refresh_highlight
    rts
; end sub titles_s_move_cursor_up
} ; !zone

; **************************************************

!zone {
titles_s_move_cursor_down
    lda titles_v_cursor_pos
    tax
    tay
    iny
    cpy #3
    bne +
    ldy #0
+
    sty titles_v_cursor_pos
    jsr titles_s_refresh_highlight
    rts
; end sub titles_s_move_cursor_down
} ; !zone

; **************************************************

!zone {
titles_s_init_sprites
    ; Disable all sprites (for now).
    ; We will use sprites #0-5, all hi-res.
    lda #$7f
    sta SPENA

    ldx #2
-
    lda #titles_c_SPR_X_LHS
    sta spr_v_x_lo,x
    lda #titles_c_SPR_X_RHS
    sta spr_v_x_lo+3,x
    lda titles_l_SPR_Y,x
    sta spr_v_y,x
    sta spr_v_y+3,x
    lda titles_l_SPR_PTRS_LHS,x
    sta spr_v_current_ptr,x
    lda titles_l_SPR_PTRS_RHS,x
    sta spr_v_current_ptr+3,x
    lda #GREY3
    sta spr_v_color,x
    sta spr_v_color+3,x
    lda #0
    sta spr_v_xxpand,x
    sta spr_v_xxpand+3,x
    lda #1
    sta spr_v_hires,x
    sta spr_v_hires+3,x
    dex
    bpl -

    jsr titles_s_prepare_n_sprite

    rts
; end sub titles_s_init_sprites
} ; !zone

; **************************************************

!zone {
titles_s_draw_sprites
    ldx #6
    ldy #6
-
    jsr spr_s_write_to_vic_ii
    dex
    dey
    bpl -

    rts
; end sub titles_s_draw_sprites
} ; !zone

; **************************************************

; NOTE: this sprite is placed next to the 'Players' text to indicate how many
; players are currently signed in.
!zone {
; Put lookup tables here as they'll only ever be used by this routine!
.offsets    !byte   0,3,6,9
.patterns   !byte   $e0,$00, $ee,$00, $ee,$e0, $ee,$ee

titles_s_prepare_n_sprite
    +utils_m_clear_sprite_data prelude_l_BLANK_SPRITE

    ; What is drawn into the sprite area depends on how many players there are!
    ldx shared_v_num_players
    beq .end
    ; If there's 1 player, we want index=0 for the tables, so decrement the
    ; 'num_players' value before use.  Then multiply it by 2 to get an index
    ; into .patterns.  Copy the two byte patterns into MATHS0 and MATHS1.
    dex
    txa
    asl
    tax
    lda .patterns,x
    sta MATHS0
    inx
    lda .patterns,x
    sta MATHS1

    ldx #3
.loop
    ldy .offsets,x
    lda MATHS0
    sta prelude_l_BLANK_SPRITE,y
    lda MATHS1
    sta prelude_l_BLANK_SPRITE+1,y
    dex
    bpl .loop

    lda #GREY3
    sta spr_v_color+6
    sta spr_v_hires+6
    lda #titles_c_SPR_NUM_PLAYERS_X
    sta spr_v_x_lo+6
    lda #titles_c_SPR_NUM_PLAYERS_Y
    sta spr_v_y+6
    lda #0
    sta spr_v_x_hi+6
    lda #titles_c_SPR_NUM_PLAYERS
    sta spr_v_current_ptr+6

.end    
    rts
; end sub titles_s_prepare_n_sprite
} ; !zone

; **************************************************

; OUTPUTS:  Z flag set = OK; otherwise error code in accumulator.
!zone {
titles_s_check_tee_off
    lda shared_v_num_players    
    beq .error_no_players

    ldx shared_v_scoring
    cpx #shared_c_STROKE_PLAY    
    beq .ok

    ; Must be match play, so check that there's an even number of players.
    ; Number of players is still in the accumulator.  'lsr' to put bit #0 into
    ; the C flag.
    lsr
    bcc .ok
    ; Error - invalid number of players for match play.
    lda #titles_c_TEE_OFF_ERROR_INVALID_MATCH_PLAY
    rts ; EXIT POINT.

.error_no_players
    +prelude_m_invalid_sfx
    lda #titles_c_TEE_OFF_ERROR_NO_PLAYERS          
    rts ; EXIT POINT.
    
.ok
    jsr sign_s_tidy_up_names
    lda #titles_c_TEE_OFF_OK
    rts
; end sub titles_s_check_tee_off
} ; !zone

; **************************************************

; INPUTS:   A = error 'code'
!zone {
titles_s_display_error_msg
    ; Subtract 1 to get correct index into the relevant lookup tables.
    tax
    dex
    lda titles_l_ERROR_MSG_ADDR_LO,x 
    sta P0
    lda titles_l_ERROR_MSG_ADDR_HI,x 
    sta P1
    lda #(24*8)
    sta P2
    lda #0
    sta P3
    lda titles_l_ERROR_MSG_LENS,x
    sta P4
    jsr font_s_draw_text
    rts
; end sub titles_s_display_error_msg
} ; !zone

; **************************************************

; NOTE: called by interrupt.  Joystick handling must go here!
!zone {
titles_s_update
    ; Listen for joystick up/down (port #2).
    ldx #joy_c_PORT2 
    +joy_m_is_up
    beq .up
    +joy_m_is_down
    beq .down
    +joy_m_is_fire
    beq .fire

    ; Neither up nor down nor fire - make sure locks are released.
    jsr joy_s_release_all_locks
    rts ; EXIT POINT.

.up
    +joy_m_is_locked_up
    +branch_if_true .end
    +joy_m_lock_up
    jsr titles_s_move_cursor_up
    jmp .browse_sfx

.down
    +joy_m_is_locked_down
    +branch_if_true .end
    +joy_m_lock_down
    jsr titles_s_move_cursor_down
.browse_sfx
    +prelude_m_browse_sfx
    rts ; EXIT POINT.

.fire
    +joy_m_is_locked_fire
    +branch_if_true .end
    +joy_m_lock_fire
    lda titles_v_cursor_pos
    beq .sign_in
    cmp #1
    beq .settings

    ; Tee off.
    jsr titles_s_check_tee_off
    beq +
    jsr titles_s_display_error_msg
    rts ; EXIT POINT.
+
    lda shared_v_is_match_play
    +branch_if_false +
    jsr prelude_s_build_team_names
+
    lda #prelude_c_MODE_TEE_OFF
    bne +

.sign_in
    lda #prelude_c_MODE_SIGN_IN
    bne +

.settings
    lda #prelude_c_MODE_SETTINGS
+
    sta prelude_v_mode_after_fade
    inc titles_v_must_exit_loop
    +prelude_m_select_sfx

.end
    rts

; end sub titles_s_update
} ; !zone

; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************
; **************************************************

