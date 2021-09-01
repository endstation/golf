; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "splash.o",cbm
!source "../core/labels.asm"
!source "../core/mymacros.asm"
!source "../core/vic_ii.asm"

*= end_of_core
!zone {
splash_s_init
    +utils_m_enable_bitmap_mode 

    +clr splash_v_must_exit

    ; Copy pixel data to bitmap.
    ; 40*25*8 = 8000 bytes.
    ; = 32*256...
    ; Source = MATHS0/1, destination = MATHS2/3.
    lda #<splash_c_PIXELS
    sta MATHS0
    lda #>splash_c_PIXELS
    sta MATHS1
    lda #<gfxs_c_BITMAP_BASE
    sta MATHS2
    lda #>gfxs_c_BITMAP_BASE
    sta MATHS3

    ldx #0
    ldy #0
-
    lda (MATHS0),y
    sta (MATHS2),y
    iny
    bne -

    inx
    cpx #32
    beq .colors

    inc MATHS1
    inc MATHS3
    jmp -

.colors
    ldx #0
-
    lda splash_c_VIDEO_RAM,x
    sta gfxs_c_DISPLAY_BASE,x 
    lda splash_c_VIDEO_RAM+250,x
    sta gfxs_c_DISPLAY_BASE+250,x 
    lda splash_c_VIDEO_RAM+500,x
    sta gfxs_c_DISPLAY_BASE+500,x 
    lda splash_c_VIDEO_RAM+750,x
    sta gfxs_c_DISPLAY_BASE+750,x 
    inx
    cpx #250
    bne -
    
    lda #BLACK
    sta EXTCOL

    jsr interrupts_s_install

-
    lda splash_v_must_exit
    +branch_if_false -

    rts
; end sub splash_s_init
} ; !zone




; *****************
; *** CONSTANTS ***
; *****************
!source "../../assets/pictures/mysplash.asm"


; *****************
; *** VARIABLES ***
; *****************
splash_v_must_exit  !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; **************************************************

!zone {
splash_s_update
    ; Play music (?!)
    ; Update any graphical effects (?!)
    ; Listen for button/key press...

    ldx #joy_c_PORT2
    +joy_m_is_fire 
    bne .end

    jsr interrupts_s_uninstall
    inc splash_v_must_exit

.end
    rts
; end sub splash_s_update
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

!source "interrupts.asm"

