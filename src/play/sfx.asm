; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


!source "../common/sound_notes.asm"


; *****************
; *** CONSTANTS ***
; *****************
; Data tables for sound effects:
; FL,FH,PL,PH,[placeholder],AD,SR,WV,<frames>, 
;   FILTER-MODE,RESONANCE,CUTOFF-LO,CUTOFF-HI,
;   <must loop>
; Initial value of $ff means end sound effect.
; NOTE: final value in '_DATA' array is how many frames to wait before
; deactivating the given channel.  This gives time for 'release' part of
; sound's envelope.
sfx_l_BROWSE_INIT       !byte   195,16, 0,0, 0, $22,$f2, $11, 2, 0,0,0,0, 0
sfx_l_BROWSE_DATA       !byte   30,25,2, $ff,$10, 3
sfx_l_BOUNCE_INIT       !byte   71,6, 0,0, 0, $22,$f8, $11, 2, 0,0,0,0, 0
sfx_l_BOUNCE_DATA       !byte   233,7,2, 97,8,2, $ff,$10, 15
sfx_l_SWISH_INIT        !byte   97,8, 0,0, 0, $b1,$f6, $81, 2, 0,0,0,0, 0
sfx_l_SWISH_DATA        !byte   143,10,2, 195,16,2, 31,21,2, 135,33,2, 62,42,2
                        !byte   15,67,2, 125,84,2, $ff,$80, 11

sfx_l_BALL_CLUB_INIT    !byte   135,33, 0,0, 0, $32,$f3, $11, 2 ;, 0,0,0,0, 0
;                        !byte   snd_c_FILTER_LOWPASS,$70,$00,$40,0   
                        !byte   0,0,0,0,0
sfx_l_BALL_CLUB_DATA    !byte   135,33,2, $ff,$10, 4
sfx_l_BALL_CUP_INIT     !byte   30,25, 0,0, 0, $23,$f7, $11, 2, 0,0,0,0, 0
sfx_l_BALL_CUP_DATA     !byte   45,1,1, 30,25,2, 45,1,1, 30,25,2, $ff,$10, 12
sfx_l_SPLASH_INIT       !byte   209,18, 0,0, 0, $63,$fa, $81, 2, 0,0,0,0, 0   
sfx_l_SPLASH_DATA       !byte   $ff,$80, 75
sfx_l_BALL_TREE_INIT    !byte   24,14, 0,0, 0, $22,$f4, $11, 3, 0,0,0,0, 0
sfx_l_BALL_TREE_DATA    !byte   $ff,$10, 8
sfx_l_MAX_POWER_INIT    !byte   195,16, 0,0, 0, $33,$f9, $11, 2, 0,0,0,0, 0
sfx_l_MAX_POWER_DATA    !byte   209,18,2, 96,22,2, 49,28,2, 135,33,2, $ff,$10, 36

sfx_l_BIRD1_INIT        !byte   <E7,>E7, 0,0, 0, $65,$f2, $11, 6
                        !byte   snd_c_FILTER_HIGHPASS,$f0,$07,$b8, 0 
;                        !byte   0,0,0,0, 0 
sfx_l_BIRD1_DATA        !byte   <C7,>C7,6
                        !byte   $ff,$10, 10
sfx_l_BIRD2_INIT        !byte   <F7,>F7, 0,0, 0, $b1,$a1, $11, 2, 0,0,0,0, 0
sfx_l_BIRD2_DATA        !byte   <E7,>E7,2, <C7,>C7,3, <D7,>D7,1, <DS7,>DS7,1
                        !byte   <E7,>E7,1, <DS7,>DS7,1, <E7,>E7,2, <F7,>F7,1
                        !byte   <FS7,>FS7,1, <A7,>A7,2, $ff,$10,2

sfx_c_BROWSE    = 0
sfx_c_BOUNCE    = 1
sfx_c_SWISH     = 2
sfx_c_BALL_CLUB = 3
sfx_c_BALL_CUP  = 4
sfx_c_SPLASH    = 5
sfx_c_BALL_TREE = 6
sfx_c_MAX_POWER = 7
sfx_c_BIRD1     = 8
sfx_c_BIRD2     = 9

sfx_l_INIT_ADDRS
    !word   <sfx_l_BROWSE_INIT
    !word   <sfx_l_BOUNCE_INIT
    !word   <sfx_l_SWISH_INIT
    !word   <sfx_l_BALL_CLUB_INIT
    !word   <sfx_l_BALL_CUP_INIT
    !word   <sfx_l_SPLASH_INIT
    !word   <sfx_l_BALL_TREE_INIT    
    !word   <sfx_l_MAX_POWER_INIT
    !word   <sfx_l_BIRD1_INIT
    !word   <sfx_l_BIRD2_INIT
sfx_l_DATA_ADDRS
    !word   <sfx_l_BROWSE_DATA
    !word   <sfx_l_BOUNCE_DATA
    !word   <sfx_l_SWISH_DATA
    !word   <sfx_l_BALL_CLUB_DATA
    !word   <sfx_l_BALL_CUP_DATA
    !word   <sfx_l_SPLASH_DATA
    !word   <sfx_l_BALL_TREE_DATA
    !word   <sfx_l_MAX_POWER_DATA
    !word   <sfx_l_BIRD1_DATA
    !word   <sfx_l_BIRD2_DATA


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
sfx_s_init
    lda #<sfx_l_INIT_ADDRS
    sta SFX_INIT_ADDRS_ZP_LO
    lda #>sfx_l_INIT_ADDRS
    sta SFX_INIT_ADDRS_ZP_HI
    lda #<sfx_l_DATA_ADDRS
    sta SFX_DATA_ADDRS_ZP_LO
    lda #>sfx_l_DATA_ADDRS
    sta SFX_DATA_ADDRS_ZP_HI
    rts
; end sub sfx_s_init
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
; **************************************************

