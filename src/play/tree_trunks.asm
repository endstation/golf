; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


ttrunks_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************


; *****************
; *** VARIABLES ***
; *****************
ttrunks_v_depths_z_lo   !fill   160
ttrunks_v_depths_z_hi   !fill   160
ttrunks_v_y0            !fill   160
;ttrunks_v_y1            !fill   160


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
ttrunks_s_reset
    lda #$80
    ; NOTE: valid indices from 1 to 160, because we're offsetting from one 
    ; byte before the actual start of the array.
    ldx #160
-
;    sta ttrunks_v_depths_z_lo-1,x   
    sta ttrunks_v_depths_z_hi-1,x   
;    sta ttrunks_v_y0-1,x            
;    sta ttrunks_v_y1-1,x            
    dex
    bne -
    rts
; end sub ttrunks_s_reset
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

ttrunks_c_SIZE = *-ttrunks_c_BEGIN
