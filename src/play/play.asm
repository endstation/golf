; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!to "play.o",cbm
!source "../core/labels.asm"
!source "../core/mymacros.asm"
!source "../core/sprite_data.asm"

; NOTE: this is to preserve the colors for the messaging area - otherwise
; the message disappears while game code loading!
*= gfxs_c_DISPLAY_BASE+24*40    ;$c3c0
    !fill   40,GREY3

*= $cc00
; sprite #48
play_l_BLANK_SPRITE_FLAG    !fill   64,0
; sprite #49...
; Reserve four sprite slots (4*64=256 bytes) for particles (*2), ball &
; shadow.
    !fill 4*64,0

; sprite #53
!bin "../../assets/sprites/clubs.bin"
!bin "../../assets/sprites/crosshair.bin"
; Reserve thirteen slots for backdrop sprites (animated or static).
    !fill 13*64,0
play_l_OVERHEAD_BALL_SPRITE
    !bin "../../assets/sprites/overhead_ball.bin"
play_l_MAX_STR_SPRITE
    !bin "../../assets/sprites/top_spr.bin"
play_c_END_OF_SPRITES

*= end_of_core
play_c_BEGIN = *

!zone {
play_s_loop
    ; NOTE: multiload routines turn off the SuperCPU!
    +utils_m_turn_on_supercpu

    jsr sfx_s_init
    jsr snd_s_set_max_volume
    jsr round_s_init
    jsr round_s_loop
    rts
; end sub play_s_loop
} ; !zone






; *****************
; *** CONSTANTS ***
; *****************


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
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

play_c_SIZE = *-play_c_BEGIN

!source "backdrop.asm"
!source "slope.asm"
!source "ball.asm"
!source "bitmap_tiles.asm"
!source "caddie.asm"
!source "camera.asm"
!source "clubs.asm"
!source "golfer.asm"
!source "hole.asm"
!source "ingame_gfx.asm"
!source "interrupts.asm"
!source "magnus.asm"
!source "maths.asm"
!source "particle_system.asm"
!source "players.asm"
!source "play_messages.asm"
!source "power_arc.asm"
!source "pythagoras.asm"
!source "quads.asm"
!source "target.asm"
!source "random.asm"
!source "round_manager.asm"
!source "score_cards.asm"
;!source "../common/sound_engine.asm"
!source "sfx.asm"
!source "sprite_store.asm"
!source "stats.asm"
!source "tee_markers.asm"
!source "terrain_indicator.asm"
!source "trees.asm"
!source "tree_trunks.asm"
!source "waypoints.asm"
!source "wind.asm"
!source "wind_slope_shared.asm"
!source "transition.asm"
;!source "duck.asm"
!source "color_wash.asm"
!source "tweeter.asm"
end_of_play

