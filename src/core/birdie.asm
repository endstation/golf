; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "birdie.o",cbm
!source "vic_ii.asm"
!source "zeropage.asm"
!source "screen_codes.asm"
!source "sprite_data.asm"
!source "mymacros.asm"
!source "kb_matrix_codes.asm"

*= $059c
!bin "loader_main.bin"
; NOTE: 'end_of_loader' is initial entry point into the program.
end_of_loader

    jsr CB_INITLOADER

    ; Turn off BASIC ROM.
    lda R6510
    and #$fe
    sta R6510

    lda #BLACK
    sta EXTCOL
    sta BGCOL0
    jsr gfxs_s_init
    jsr app_s_run

!source "joystick.asm"
!source "utilities.asm"
!source "gfx_setup.asm"
!source "app.asm"
!source "sprite_engine.asm"
!source "draw_primitives.asm"
!source "shared_data.asm"
!source "mc_bitmap_font.asm"
!source "icons.asm"
!source "messaging.asm"
!source "sound_engine.asm"

end_of_core

*= $3000
!bin "loader_init.bin"


