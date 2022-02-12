; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


sstore_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; Record where the 'loadable' sprites are placed and what the first
; sprite number is.
sstore_c_BASE_NUM = sprd_c_LOADABLE_BASE
sstore_c_DST_BASE = $c000+(sstore_c_BASE_NUM*64)

; Data binaries.
sstore_c_DATA_SWING_UPPER_MALE
!bin "../../assets/sprites/swing_upper_male.bin"
sstore_c_DATA_SWING_LOWER_MALE
!bin "../../assets/sprites/swing_lower_male.bin"
sstore_c_DATA_SWING_UPPER_FEMALE
!bin "../../assets/sprites/swing_upper_female.bin"
sstore_c_DATA_SWING_LOWER_FEMALE
!bin "../../assets/sprites/swing_lower_male.bin"
sstore_c_DATA_PUTT_UPPER_MALE
!bin "../../assets/sprites/putt_upper_male.bin"
sstore_c_DATA_PUTT_LOWER_MALE
!bin "../../assets/sprites/putt_lower_male.bin"
sstore_c_DATA_PUTT_UPPER_FEMALE
!bin "../../assets/sprites/putt_upper_female.bin"
sstore_c_DATA_PUTT_LOWER_FEMALE
!bin "../../assets/sprites/putt_lower_male.bin"

sstore_c_SWING_UPPER_MALE = 0
sstore_c_SWING_LOWER_MALE = 1
sstore_c_SWING_UPPER_FEMALE = 2
sstore_c_SWING_LOWER_FEMALE = 3
sstore_c_PUTT_UPPER_MALE = 4
sstore_c_PUTT_LOWER_MALE = 5
sstore_c_PUTT_UPPER_FEMALE = 6
sstore_c_PUTT_LOWER_FEMALE = 7

; Indexed by one of constants above.
sstore_l_SRC_LO
    !byte   <sstore_c_DATA_SWING_UPPER_MALE
    !byte   <sstore_c_DATA_SWING_LOWER_MALE
    !byte   <sstore_c_DATA_SWING_UPPER_FEMALE
    !byte   <sstore_c_DATA_SWING_LOWER_FEMALE
    !byte   <sstore_c_DATA_PUTT_UPPER_MALE
    !byte   <sstore_c_DATA_PUTT_LOWER_MALE
    !byte   <sstore_c_DATA_PUTT_UPPER_FEMALE
    !byte   <sstore_c_DATA_PUTT_LOWER_FEMALE
sstore_l_SRC_HI
    !byte   >sstore_c_DATA_SWING_UPPER_MALE
    !byte   >sstore_c_DATA_SWING_LOWER_MALE
    !byte   >sstore_c_DATA_SWING_UPPER_FEMALE
    !byte   >sstore_c_DATA_SWING_LOWER_FEMALE
    !byte   >sstore_c_DATA_PUTT_UPPER_MALE
    !byte   >sstore_c_DATA_PUTT_LOWER_MALE
    !byte   >sstore_c_DATA_PUTT_UPPER_FEMALE
    !byte   >sstore_c_DATA_PUTT_LOWER_FEMALE
sstore_l_DST_LO
    !byte   <sstore_c_DST_BASE 
    !byte   <(sstore_c_DST_BASE+(16*64))
    !byte   <sstore_c_DST_BASE 
    !byte   <(sstore_c_DST_BASE+(16*64))
    !byte   <sstore_c_DST_BASE 
    !byte   <(sstore_c_DST_BASE+(11*64))
    !byte   <sstore_c_DST_BASE 
    !byte   <(sstore_c_DST_BASE+(11*64))
sstore_l_DST_HI
    !byte   >sstore_c_DST_BASE 
    !byte   >(sstore_c_DST_BASE+(16*64))
    !byte   >sstore_c_DST_BASE 
    !byte   >(sstore_c_DST_BASE+(16*64))
    !byte   >sstore_c_DST_BASE 
    !byte   >(sstore_c_DST_BASE+(11*64))
    !byte   >sstore_c_DST_BASE 
    !byte   >(sstore_c_DST_BASE+(11*64))
sstore_l_NUM_SPRITES    !byte   16,14,16,14,11,1,11,1

sstore_c_SHOT_TYPE_SWING_MALE   = 0
sstore_c_SHOT_TYPE_PUTT_MALE    = 1
sstore_c_SHOT_TYPE_SWING_FEMALE = 2
sstore_c_SHOT_TYPE_PUTT_FEMALE  = 3


; *****************
; *** VARIABLES ***
; *****************
sstore_v_last_sequence  !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   X = sequence (from constants defined above).
!zone {
.SRC_ZP = P0
.DST_ZP = P2

sstore_s_copy
    ; Initialize source and destination address pointers.
    lda sstore_l_SRC_LO,x
    sta .SRC_ZP
    lda sstore_l_SRC_HI,x
    sta .SRC_ZP+1
    lda sstore_l_DST_LO,x
    sta .DST_ZP
    lda sstore_l_DST_HI,x
    sta .DST_ZP+1
    lda sstore_l_NUM_SPRITES,x
    tax

    ; NOTE: it may be that we're writing to I/O area ($d000-$dfff).
    ; Since we want to write to the underlying RAM, bank in the RAM (character
    ; ROM?) for the rest of the routine.
    sei
    lda R6510
    and #$fb
    sta R6510

.loop
    ; Copy routine begins here.
    ; 64 bytes per sprite.
    ldy #63
-
    lda (.SRC_ZP),y
    sta (.DST_ZP),y
    dey
    bpl -

    dex
    beq .end

    ; More sprites to go - advance address pointers.
    lda .SRC_ZP
    clc
    adc #64
    sta .SRC_ZP
    lda .SRC_ZP+1
    adc #0
    sta .SRC_ZP+1
    lda .DST_ZP
    clc
    adc #64
    sta .DST_ZP
    lda .DST_ZP+1
    adc #0
    sta .DST_ZP+1
    jmp .loop

.end
    ; Restore I/O area.
    lda R6510
    ora #$04
    sta R6510
    cli

    rts
; end sub sstore_s_copy
} ; !zone

; **************************************************

; INPUTS:   X = shot type.
!zone {
sstore_s_load_sequence
    cpx sstore_v_last_sequence
    bne +
    ; The requested sequence is already in video RAM, so nothing to do.
    rts ; EXIT POINT.

+   
    stx sstore_v_last_sequence
    cpx #sstore_c_SHOT_TYPE_SWING_MALE
    beq .swing_male
    cpx #sstore_c_SHOT_TYPE_SWING_FEMALE 
    beq .swing_female
    cpx #sstore_c_SHOT_TYPE_PUTT_MALE    
    beq .putt_male

    ; So must be 'putt female'...
    ldx #sstore_c_PUTT_UPPER_FEMALE
    jsr sstore_s_copy
    ldx #sstore_c_PUTT_LOWER_FEMALE
    jsr sstore_s_copy
    rts ; EXIT POINT.
.swing_male
    ldx #sstore_c_SWING_UPPER_MALE
    jsr sstore_s_copy
    ldx #sstore_c_SWING_LOWER_MALE
    jsr sstore_s_copy
    rts ; EXIT POINT.
.swing_female
    ldx #sstore_c_SWING_UPPER_FEMALE
    jsr sstore_s_copy
    ldx #sstore_c_SWING_LOWER_FEMALE
    jsr sstore_s_copy
    rts ; EXIT POINT.
.putt_male
    ldx #sstore_c_PUTT_UPPER_MALE
    jsr sstore_s_copy
    ldx #sstore_c_PUTT_LOWER_MALE
    jsr sstore_s_copy

    rts
; end sub sstore_s_load_sequence
} ; !zone

; **************************************************

; Called at start of each round.
; Set the 'last sequence' to an invalid number so it must always be loaded.
!zone {
sstore_init
    lda #$ff
    sta sstore_v_last_sequence
    rts
; end sub sstore_init
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


sstore_c_SIZE = *-sstore_c_BEGIN
