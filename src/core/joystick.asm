; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *******************
; **** CONSTANTS ****
; *******************
joy_c_UP       = $01
joy_c_DOWN     = $02
joy_c_LEFT     = $04
joy_c_RIGHT    = $08
joy_c_FIRE     = $10

; NOTE: these are offsets that we use to index CIAPRA (= $dc00).
joy_c_PORT1 = 1
joy_c_PORT2 = 0


; *******************
; **** VARIABLES ****
; *******************
joy_v_up_locked     !byte   0,0
joy_v_down_locked   !byte   0,0
joy_v_left_locked   !byte   0,0
joy_v_right_locked  !byte   0,0
joy_v_fire_locked   !byte   0,0

; Use constants joy_c_PORT1 and joy_c_PORT2 to set this variable.
joy_v_current_port  !byte   joy_c_PORT2


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; Implemented as a subroutine rather than a macro because it's a bit more
; involved.  Z flag set if left or right is 'pressed'.
; INPUTS:   X = port
!zone {
joy_s_is_horizontal
    lda CIAPRA,x
    and #joy_c_LEFT
    bne +
    ; 'Left' is pressed and Z flag set so OK to return.
    rts

+   lda CIAPRA,x
    and #joy_c_RIGHT
    ; If 'right' is pressed, Z will be set, otherwise clear. 
    rts
; end sub joy_s_is_horizontal
} ; !zone

; **************************************************

; See above for how this works.
; INPUTS:   X = port
!zone {
joy_s_is_vertical
    lda CIAPRA,x
    and #joy_c_UP
    bne +
    rts
+   lda CIAPRA,x
    and #joy_c_DOWN
    rts
; end sub joy_s_is_vertical
} ; !zone

; **************************************************

; INPUTS:   X = port
!zone {
joy_s_release_all_locks
    lda #0
    sta joy_v_up_locked,x
    sta joy_v_down_locked,x
    sta joy_v_left_locked,x
    sta joy_v_right_locked,x
    sta joy_v_fire_locked,x
    rts
; end sub joy_s_release_all_locks
} ; !zone

; **************************************************
; **************************************************

