; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************
; Branch if less than - for signed comparisons.  Do the subtraction before
; inserting this!
; NOTE: trashes accumulator.
!macro blt_s .target {
    bvc +
    eor #$80
+   bmi .target
} ; blt_s

; Branch if greater than or equal to.
; NOTE: trashes accumulator.
!macro bge_s .target {
    bvc +
    eor #$80
+   bpl .target
} ; bge_s

!macro clr .mem {
    lda #0
    sta .mem
} ; clr

!macro set .mem {
    lda #1
    sta .mem
} ; set

!macro branch_if_true .target {
    bne .target
} ; branch_if_true

!macro branch_if_false .target {
    beq .target
} ; branch_if_false

; Two's complement of whatever's in the accumulator.
!macro nega {
    eor #$ff
    clc
    adc #1
} ; nega

!macro neg .mem {
    lda .mem
    eor #$ff
    clc
    adc #1
    sta .mem
} ; neg

; Two's comp of 16-bit value.  Original memory locations overwritten.
!macro neg16 .lo {
    lda .lo
    eor #$ff
    clc
    adc #1
    sta .lo
    lda .lo+1
    eor #$ff
    adc #0
    sta .lo+1
} ; neg16

; Same as above but 24-bit.
!macro neg24 .lo {
    lda .lo
    eor #$ff
    clc
    adc #1
    sta .lo
    lda .lo+1
    eor #$ff
    adc #0
    sta .lo+1
    lda .lo+2
    eor #$ff
    adc #0
    sta .lo+2
} ; neg24

; 8-bit unsigned comparisons.
!macro ble .target {
    beq .target
    bcc .target
} ; ble
    
!macro adc16 .lhs,.rhs {
    lda .lhs
    clc
    adc .rhs
    sta .lhs
    lda .lhs+1
    adc .rhs+1
    sta .lhs+1
} ; adc16

; 16-bit subtraction.  Specify low bytes of lhs and rhs.
; Does this: .lhs = .lhs - .rhs
!macro sbc16 .lhs,.rhs {
    lda .lhs
    sec
    sbc .rhs
    sta .lhs
    lda .lhs+1
    sbc .rhs+1
    sta .lhs+1
} ; sbc16

; Add 8-bit immediate value to a 16-bit value at given location.
!macro adc16_8bit_imm .lhs,.value {
    lda .lhs
    clc
    adc #.value
    sta .lhs
    bcc +
    inc .lhs+1
+
} ; adc16_8bit_imm

!macro skip_2_bytes {
    !byte BIT_ABSOLUTE
} ; skip_2_bytes

!macro skip_1_byte {
    !byte BIT_ZEROPAGE
} ; skip_1_byte

; Extend sign into high byte.
!macro sex .hi {
    bmi +
    lda #0
    +skip_2_bytes
+
    lda #$ff
    sta .hi
} ; sex
    
!macro beq16 .lo,.target {
    lda .lo
    ora .lo+1
    beq .target
} ; beq16

!macro bne16 .lo,.target {
    lda .lo
    ora .lo+1
    bne .target
} ; bne16

; Scale down a 16-bit number by a fraction of 256.
; OUTPUT:   P5-P6 = 16-bit quotient, correctly signed.
!macro scale_down .lo,.factor {
    lda .lo
    sta P0
    lda .lo+1
    sta P1
    lda .factor
    sta P2
    lda #0
    sta P3
    jsr maths_mul16s
    ; Result is in P4-P7, but because we now want to divide by 256, take
    ; P5-P6 as the quotient.
    lda SIGN_CHANGED
    beq +
    +neg16 P5
+
} ; scale_down

; **************************************************

; joystick module
; ---------------
; NOTE: for all macros, X = port (either joy_c_PORT1 or joy_c_PORT2).
; Convenience macros for checking individual switches on joystick.
; Z flag SET if switch is ON.
!macro joy_m_is_up {
    lda CIAPRA,x
    and #joy_c_UP
} ; joy_m_is_up

!macro joy_m_is_down {
    lda CIAPRA,x
    and #joy_c_DOWN
} ; joy_m_is_down

!macro joy_m_is_left {
    lda CIAPRA,x
    and #joy_c_LEFT
} ; joy_m_is_left

!macro joy_m_is_right {
    lda CIAPRA,x
    and #joy_c_RIGHT
} ; joy_m_is_right

!macro joy_m_is_fire {
    lda CIAPRA,x
    and #joy_c_FIRE
} ; joy_m_is_fire

; **************************************************

!macro joy_m_lock_up {
    lda #1
    sta joy_v_up_locked,x
} ; joy_m_lock_up

!macro joy_m_lock_down {
    lda #1
    sta joy_v_down_locked,x
} ; joy_m_lock_down

!macro joy_m_lock_left {
    lda #1
    sta joy_v_left_locked,x
} ; joy_m_lock_left

!macro joy_m_lock_right {
    lda #1
    sta joy_v_right_locked,x
} ; joy_m_lock_right

!macro joy_m_lock_fire {
    lda #1
    sta joy_v_fire_locked,x
} ; joy_m_lock_fire

!macro joy_m_lock_vertical {
    lda #1
    sta joy_v_up_locked,x
    sta joy_v_down_locked,x
} ; joy_m_lock_vertical

; **************************************************

!macro joy_m_release_up {
    lda #0
    sta joy_v_up_locked,x
} ; joy_m_release_up

!macro joy_m_release_down {
    lda #0
    sta joy_v_down_locked,x
} ; joy_m_release_down

!macro joy_m_release_left {
    lda #0
    sta joy_v_left_locked,x
} ; joy_m_release_left

!macro joy_m_release_right {
    lda #0
    sta joy_v_right_locked,x
} ; joy_m_release_right

!macro joy_m_release_fire {
    lda #0
    sta joy_v_fire_locked,x
} ; joy_m_release_fire

!macro joy_m_release_vertical {
    lda #0
    sta joy_v_up_locked,x
    sta joy_v_down_locked,x
} ; joy_m_release_vertical

!macro joy_m_release_horizontal {
    lda #0
    sta joy_v_left_locked,x
    sta joy_v_right_locked,x
} ; joy_m_release_horizontal

; **************************************************

!macro joy_m_is_locked_up {
    lda joy_v_up_locked,x
} ; joy_m_is_locked_up

!macro joy_m_is_locked_down {
    lda joy_v_down_locked,x
} ; joy_m_is_locked_down

!macro joy_m_is_locked_left {
    lda joy_v_left_locked,x
} ; joy_m_is_locked_left

!macro joy_m_is_locked_right {
    lda joy_v_right_locked,x
} ; joy_m_is_locked_right

!macro joy_m_is_locked_fire {
    lda joy_v_fire_locked,x
} ; joy_m_is_locked_fire

; **************************************************
; utilities module
; ----------------
!macro utils_m_save_a_to_stack {
    pha
} ; utils_m_save_a_to_stack

!macro utils_m_save_x_to_stack {
    txa
    pha
} ; utils_m_save_x_to_stack 

!macro utils_m_save_y_to_stack {
    tya
    pha
} ; utils_m_save_y_to_stack 

!macro utils_m_save_xy_to_stack {
    txa
    pha
    tya
    pha
} ; utils_m_save_xy_to_stack 

!macro utils_m_save_axy_to_stack {
    pha
    txa
    pha
    tya
    pha
} ; utils_m_save_axy_to_stack

!macro utils_m_restore_a_from_stack {
    pla
} ; utils_m_restore_a_from_stack

!macro utils_m_restore_x_from_stack {
    pla
    tax
} ; utils_m_restore_x_from_stack 

!macro utils_m_restore_y_from_stack {
    pla
    tay
} ; utils_m_restore_y_from_stack 

!macro utils_m_restore_xy_from_stack {
    pla
    tay
    pla
    tax
} ; utils_m_restore_xy_from_stack 

!macro utils_m_restore_axy_from_stack {
    pla
    tay
    pla
    tax
    pla
} ; utils_m_restore_axy_from_stack 

!zone {
!macro utils_m_prepare_zp .addr, .zplo {
    lda #<.addr
    sta .zplo
    lda #>.addr
    sta .zplo+1
} ; utils_m_prepare_zp 
} ; end !zone

!macro utils_m_enable_bitmap_mode {
    lda SCROLY
    ora #$20
    and #$7f
    sta SCROLY
    lda #gfxs_c_DISPLAY_OFFSET+8
    sta VMCSB
} ; utils_m_enable_bitmap_mode 

!macro utils_m_enable_multicolor_mode {
    lda SCROLX
    ora #$10
    sta SCROLX
} ; utils_m_enable_multicolor_mode 

!macro utils_m_disable_bitmap_mode {
    lda SCROLY
    and #$5f
    sta SCROLY
} ; utils_m_disable_bitmap_mode

; For text and bitmap.
!macro utils_m_disable_multicolor_mode {
    lda SCROLX
    and #$ef
    sta SCROLX
} ; utils_m_disable_multicolor_mode 

!macro utils_m_clear_raster_bit9 {
    lda SCROLY
    and #$7f
    sta SCROLY
} ; utils_m_clear_raster_bit9 

; Acts on accumulator.
!macro utils_m_take_twos_comp {
    eor #$ff
    clc
    adc #1
} ; utils_m_take_twos_comp 

!macro utils_m_delay .n {
    ldx #.n
-   dex
    bne -
} ; utils_m_delay

!macro utils_m_inca {
    clc
    adc #1
} ; utils_m_inca

!macro utils_m_deca {
    sec
    sbc #1
} ; utils_m_deca

; Trashes accumulator.
!macro utils_m_advance_zp_iter .zp_lo,.n {
    lda .zp_lo
    clc
    adc #.n
    sta .zp_lo
    lda .zp_lo+1
    adc #0
    sta .zp_lo+1
} ; utils_m_advance_zp_iter

!macro utils_m_kernal_out {
    lda R6510
    and #%11111101
    sta R6510
} ; utils_m_kernal_out 

!macro utils_m_kernal_in {
    lda R6510
    ora #$02
    sta R6510
} ; utils_m_kernal_in

!macro utils_m_clear_sprite_data .addr {
    ldx #63
    lda #0
-   sta .addr,x
    dex
    bpl -
} ; utils_m_clear_sprite_data
    
!macro utils_m_turn_on_supercpu {
    sta $d07b
} ; utils_m_turn_on_supercpu

; **************************************************
; sprite_engine module
; --------------------
!macro spr_m_enable_all {
    lda #$ff
    sta SPENA
} ; spr_m_enable_all

!macro spr_m_disable_all {
    lda #0
    sta SPENA
} ; spr_m_disable_all

; X = sprite #.
!macro spr_m_enable {
    lda utils_l_BIT_LOOKUP,x        
    ora SPENA
    sta SPENA
} ; spr_m_enable

; X = sprite #.
!macro spr_m_disable {
    lda utils_l_EOR_BIT_LOOKUP,x    
    and SPENA
    sta SPENA
} ; spr_m_disable

; X = sprite #.
!macro spr_m_clear_msb {
    lda utils_l_BIT_LOOKUP,x
    eor #$ff
    and MSIGX
    sta MSIGX
} ; spr_m_clear_msb

; X = sprite #.
!macro spr_m_set_msb {
    lda utils_l_BIT_LOOKUP,x
    ora MSIGX
    sta MSIGX
} ; spr_m_set_msb

!macro spr_m_disable2 .num {
    lda SPENA
    and #255-(1<<.num)
    sta SPENA
} ; spr_m_disable2


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
; **************************************************
; **************************************************

