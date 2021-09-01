; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


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
sfx_l_BROWSE2_INIT  !byte   195,16, 0,0, 0, $22,$f2, $11, 2, 0,0,0,0, 0
sfx_l_BROWSE2_DATA  !byte   30,25,2, $ff,$10, 3
sfx_l_INVALID_INIT  !byte   195,16, 0,0, 0, $22,$f2, $11, 2, 0,0,0,0, 0
sfx_l_INVALID_DATA  !byte   195,17,2, $ff,$10, 3
sfx_l_SELECT_INIT   !byte   135,33, 0,0, 0, $22,$f3, $11, 2, 0,0,0,0, 0
sfx_l_SELECT_DATA   !byte   162,37,2, 62,42,2, 60,50,2, 15,67,2, $ff,$10, 4
sfx_l_TYPE_INIT     !byte   99,56, 0,0, 0, $11,$f1, $11, 2, 0,0,0,0, 0
sfx_l_TYPE_DATA     !byte   $ff,$10, 2
sfx_l_DELETE_INIT   !byte   49,28, 0,0, 0, $11,$f1, $11, 2, 0,0,0,0, 0
sfx_l_DELETE_DATA   !byte   $ff,$10, 2
sfx_l_BACK_INIT     !byte   195,16, 0,0, 0, $11,$f9, $11, 4, 0,0,0,0, 0
sfx_l_BACK_DATA     !byte   30,25,2, $ff,$10, 37 
sfx_l_DELETE_PLAYER_INIT    !byte   135,33, 0,0, 0, $11,$f8, $11, 2, 0,0,0,0, 0
sfx_l_DELETE_PLAYER_DATA    !byte   30,25,2, 195,16,2, $ff,$10, 15

sfx_c_BROWSE2       = 0
sfx_c_SELECT        = 2
sfx_c_TYPE          = 4
sfx_c_DELETE        = 6
sfx_c_INVALID       = 8
sfx_c_BACK          = 10
sfx_c_DELETE_PLAYER = 12

; Addresses stored in two bytes, lo/hi byte format.
sfx_l_INIT_ADDRS
    !word   sfx_l_BROWSE2_INIT
    !word   sfx_l_SELECT_INIT
    !word   sfx_l_TYPE_INIT
    !word   sfx_l_DELETE_INIT
    !word   sfx_l_INVALID_INIT
    !word   sfx_l_BACK_INIT
    !word   sfx_l_DELETE_PLAYER_INIT
sfx_l_DATA_ADDRS
    !word   sfx_l_BROWSE2_DATA
    !word   sfx_l_SELECT_DATA
    !word   sfx_l_TYPE_DATA
    !word   sfx_l_DELETE_DATA
    !word   sfx_l_INVALID_DATA
    !word   sfx_l_BACK_DATA
    !word   sfx_l_DELETE_PLAYER_DATA


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

