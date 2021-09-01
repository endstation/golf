; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


interrupts_l_TITLES_SPLITS      !byte   $db,0
interrupts_l_SETTINGS_SPLITS    !byte   $a4,$e6,0
interrupts_l_FADING_SPLITS      !byte   $fa,0
interrupts_l_SIGN_IN_SPLITS     !byte   $89,$fa,0


; *****************
; *** VARIABLES ***
; *****************
interrupts_v_current_raster !byte   0
; Look up cb routine to switch to via 'prelude_v_current_mode'.
interrupts_v_must_change_cb !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
interrupts_s_cb_titles
    lda VICIRQ
    bpl interrupts_clean_up

    jsr interrupts_s_check_cb
    bcs +
    jsr titles_s_update
+

    jmp interrupts_s_reset
; end sub interrupts_s_cb_titles
} ; !zone

; **************************************************
!zone {
interrupts_s_cb_settings
    lda VICIRQ
    bpl interrupts_clean_up

    jsr interrupts_s_check_cb
    bcs +

    lda interrupts_v_current_raster
    beq .lower_sprites
    jsr settings_s_update
    jmp interrupts_s_reset
.lower_sprites
    jsr settings_draw_lower_sprites
+
    jmp interrupts_s_reset

; end sub interrupts_s_cb_settings
} ; !zone

; **************************************************

!zone {
interrupts_s_cb_fading
    lda VICIRQ
    bpl interrupts_clean_up
    jsr interrupts_s_check_cb
    bcs +
    jsr fader_s_update
+
    jmp interrupts_s_reset

; end sub interrupts_s_cb_fading
} ; !zone

; **************************************************

!zone {
interrupts_s_cb_sign_in
    lda VICIRQ
    bpl interrupts_clean_up

    jsr interrupts_s_check_cb
    bcs +

    lda interrupts_v_current_raster
    beq .lower_sprites

    ; So update and draw upper sprites...
    jsr sign_s_update
+
    jmp interrupts_s_reset

.lower_sprites
    jsr sign_s_draw_lower_sprites
    jmp interrupts_s_reset

; end sub interrupts_s_cb_sign_in
} ; !zone

; **************************************************

; NOTE: this is 'jmp'd to by the callback (cb) routines.
!zone {
interrupts_s_reset
    jsr snd_s_update
    inc shared_v_random_seed

    ldy interrupts_v_current_raster
    iny
-
    lda (INTERRUPTS_LO),y
    bne +
    ldy #0
    beq -
+
    sty interrupts_v_current_raster
    sta RASTER
    +utils_m_clear_raster_bit9 
    ; Release latch.
    asl VICIRQ

; NOTE: will often jump straight to here.
interrupts_clean_up
    ; Pull registers off of stack and restore.
    pla
    tay
    pla
    tax
    pla
    rti

; end sub interrupts_s_reset
} ; !zone

; **************************************************

!zone {
interrupts_s_uninstall
    sei
    lda #0
    sta IRQMSK
    cli
    rts
; end sub interrupts_s_uninstall
} ; !zone

; **************************************************

!zone {
interrupts_s_install
    sei

    lda #0
    sta interrupts_v_must_change_cb

    lda #<interrupts_s_cb_titles
    sta CINV
    lda #>interrupts_s_cb_titles
    sta CINV+1
    lda #<interrupts_l_TITLES_SPLITS
    sta INTERRUPTS_LO
    lda #>interrupts_l_TITLES_SPLITS
    sta INTERRUPTS_HI

    ; Enable raster interrupts.
    lda #$01
    sta IRQMSK
    ; Turn off CIA interrupts.
    lda #$7f
    sta $dc0d

    ldy #0
    sty interrupts_v_current_raster
    lda (INTERRUPTS_LO),y
    sta RASTER
    +utils_m_clear_raster_bit9 

    cli

    rts
; end sub interrupts_s_install
} ; !zone

; **************************************************

; OUTPUTS:  C flag clear if no change; C flag set if changed.
!zone {
interrupts_s_check_cb
    lda interrupts_v_must_change_cb
    +branch_if_false .end
    
    lda #0
    sta interrupts_v_current_raster
    sta interrupts_v_must_change_cb

    ; Change interrupt callback depending on value of prelude_v_current_mode.
    ; No need to turn off interrupts - we're inside an interrupt routine!
    lda prelude_v_current_mode
    cmp #prelude_c_MODE_TITLES
    beq .titles
    cmp #prelude_c_MODE_SIGN_IN
    beq .sign_in
    cmp #prelude_c_MODE_SETTINGS
    beq .settings

    ; So fading...
    lda #<interrupts_s_cb_fading
    sta CINV
    lda #>interrupts_s_cb_fading
    sta CINV+1
    lda #<interrupts_l_FADING_SPLITS
    sta INTERRUPTS_LO
    lda #>interrupts_l_FADING_SPLITS
    sta INTERRUPTS_HI
    sec
    rts ; EXIT POINT.

.titles
    lda #<interrupts_s_cb_titles
    sta CINV
    lda #>interrupts_s_cb_titles
    sta CINV+1
    lda #<interrupts_l_TITLES_SPLITS
    sta INTERRUPTS_LO
    lda #>interrupts_l_TITLES_SPLITS
    sta INTERRUPTS_HI
    sec
    rts ; EXIT POINT.

.sign_in
    lda #<interrupts_s_cb_sign_in
    sta CINV
    lda #>interrupts_s_cb_sign_in
    sta CINV+1
    lda #<interrupts_l_SIGN_IN_SPLITS
    sta INTERRUPTS_LO
    lda #>interrupts_l_SIGN_IN_SPLITS
    sta INTERRUPTS_HI
    sec
    rts ; EXIT POINT.

.settings
    lda #<interrupts_s_cb_settings
    sta CINV
    lda #>interrupts_s_cb_settings
    sta CINV+1
    lda #<interrupts_l_SETTINGS_SPLITS
    sta INTERRUPTS_LO
    lda #>interrupts_l_SETTINGS_SPLITS
    sta INTERRUPTS_HI
    sec
    rts ; EXIT POINT.

.end
    clc
    rts
; end sub interrupts_s_check_cb
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

