; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


snd_c_BEGIN = *

; *****************
; *** CONSTANTS ***
; *****************
snd_c_NUM_CHANNELS = 3

snd_l_REGS_BASE_ADDR_LO   !byte   $00,$07,$0e
snd_l_REGS_BASE_ADDR_HI   !byte   $d4,$d4,$d4

; Filter types.
snd_c_FILTER_LOWPASS  = 1<<4
snd_c_FILTER_BANDPASS = 1<<5
snd_c_FILTER_HIGHPASS = 1<<6

snd_c_OSC_OFF_CMD   = $fd
snd_c_SFX_END_CMD   = $ff


; *****************
; *** VARIABLES ***
; *****************
snd_v_waiting_to_end    !fill   snd_c_NUM_CHANNELS,$ff

snd_v_channel_active    !fill   snd_c_NUM_CHANNELS,0
snd_v_counter           !fill   snd_c_NUM_CHANNELS,0
snd_v_data_offset       !fill   snd_c_NUM_CHANNELS,0
snd_v_must_loop         !fill   snd_c_NUM_CHANNELS,0
snd_v_gates             !fill   snd_c_NUM_CHANNELS,0

snd_v_data_address_lo   !fill   snd_c_NUM_CHANNELS,0    
snd_v_data_address_hi   !fill   snd_c_NUM_CHANNELS,0     

; Keep an 'off-SID' buffer of these two registers, since we're unable to
; read them.  All read/write operations during update happen here; then copy
; them over at the end of the routine.
; NOTE: RESON=$d417, SIGVOL=$d418
snd_v_RESON_buffer      !byte   0
snd_v_SIGVOL_buffer     !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
snd_s_set_max_volume
    lda #$0f
    sta SIGVOL
    sta snd_v_SIGVOL_buffer
    rts
; end sub snd_s_set_max_volume
} ; !zone

; **************************************************

; INPUTS:   Y = sfx #
; OUTPUTS:  X = channel/voice (or negative if none available)
!zone {
snd_s_init_sfx
    jsr snd_s_next_free_channel
    beq .ok
    ; No channel available.
    ldx #$ff
    rts ; EXIT POINT.

.ok
    lda sfx_l_INIT_ADDR_LO,y
    sta SND_INIT_DATA_ZP_LO
    lda sfx_l_INIT_ADDR_HI,y
    sta SND_INIT_DATA_ZP_HI
    lda sfx_l_DATA_ADDR_LO,y
    sta snd_v_data_address_lo,x
    lda sfx_l_DATA_ADDR_HI,y
    sta snd_v_data_address_hi,x
    
    ; NOTE: following routine preserves value of X.
    jsr snd_s_real_init

    rts
; end sub snd_s_init_sfx
} ; !zone

; **************************************************

; OUTPUT: X holds channel number.  
; Z flag clear if no channel was found.
!zone {
snd_s_next_free_channel
    ldx #snd_c_NUM_CHANNELS-1
-   lda snd_v_channel_active,x
    beq .found
    dex
    bpl -
    ; No free channel.  Last call to 'dex' cleared Z flag.
.found
    rts
; end sub snd_s_next_free_channel
} ; !zone

; **************************************************

; INPUT: X = channel (0 to 2).
!zone {
.FILTER = P0

snd_s_real_init
    ; First initialize SND_REGS_BASE zero page vector.
    lda snd_l_REGS_BASE_ADDR_LO,x
    sta SND_REGS_BASE_ZP_LO 
    lda snd_l_REGS_BASE_ADDR_HI,x
    sta SND_REGS_BASE_ZP_HI 

    ; Activate this channel and reset data offset.
    inc snd_v_channel_active,x
    lda #0
    sta snd_v_data_offset,x

    ; Is a filter to be used?
    ldy #9
    lda (SND_INIT_DATA_ZP_LO),y
    beq .skip_filter

    sta .FILTER
    ; Record that current voice will have a filter.
    lda snd_v_RESON_buffer
    ora utils_l_BIT_LOOKUP,x
    sta snd_v_RESON_buffer

    ; Filter mode - currently in 'SND_ENGINE_TMP'.
    ; FIXME: what if another filter is currently in use?!
    lda snd_v_SIGVOL_buffer
    and #$0f
    ora .FILTER
    sta snd_v_SIGVOL_buffer

    ; Resonance.
    ; FIXME: what if an existing sfx is using resonance?!
    ldy #10
    lda snd_v_RESON_buffer
    and #$0f
    ora (SND_INIT_DATA_ZP_LO),y
    sta snd_v_RESON_buffer

    ; Cutoff - lo/hi bytes.
    ldy #11
    lda (SND_INIT_DATA_ZP_LO),y
    sta CUTLO
    ldy #12
    lda (SND_INIT_DATA_ZP_LO),y
    sta CUTHI
    jmp .filter_in_use

.skip_filter
    ; Turn off filter for this voice.
    lda utils_l_EOR_BIT_LOOKUP,x
    and snd_v_RESON_buffer
    sta snd_v_RESON_buffer

.filter_in_use
    ; Set the initial sound data.
    ldy #6
-   lda (SND_INIT_DATA_ZP_LO),y
    sta (SND_REGS_BASE_ZP_LO),y
    dey
    bpl -

    ; Write buffers to SID.
    lda snd_v_RESON_buffer
    sta RESON
    lda snd_v_SIGVOL_buffer
    sta SIGVOL

    ; VCREG last.  Offset from beginning of init data is 7.  Offset from
    ; FRELOx is 4.
    ldy #7
    lda (SND_INIT_DATA_ZP_LO),y
    sta snd_v_gates,x
    ldy #4
    sta (SND_REGS_BASE_ZP_LO),y
    
    ldy #8
    lda (SND_INIT_DATA_ZP_LO),y
    sta snd_v_counter,x

    ; Must loop?
    ldy #13
    lda (SND_INIT_DATA_ZP_LO),y
    sta snd_v_must_loop,x

    rts
; end sub snd_real_init
} ; !zone

; **************************************************

!zone {
snd_s_update
    ldx #snd_c_NUM_CHANNELS-1 

.loop_top
    lda snd_v_channel_active,x
    beq .next

    ; We may just be waiting to deactivate the channel.
    ; CHECK FOR THIS FIRST!!!
    ; A positive value means we are waiting.  Deactivate channel as soon as 
    ; 'waiting' value becomes negative after decrementing.
    lda snd_v_waiting_to_end,x
    bmi .not_waiting
    dec snd_v_waiting_to_end,x
    bpl .next

.deactivate
    lda #0
    sta snd_v_channel_active,x
    beq .next

.not_waiting
    dec snd_v_counter,x
    ; Until this reaches 0, we're not ready for the next note/command.
    bne .next

    ; Need to look at the next chunk of frequency data.  
    ; Load relevant source address into zp memory.
    lda snd_v_data_address_lo,x     
    sta SND_INIT_DATA_ZP_LO 
    lda snd_v_data_address_hi,x     
    sta SND_INIT_DATA_ZP_HI 
    ; And the destination address.
    lda snd_l_REGS_BASE_ADDR_LO,x   
    sta SND_REGS_BASE_ZP_LO 
    lda snd_l_REGS_BASE_ADDR_HI,x   
    sta SND_REGS_BASE_ZP_HI 

.read_data
    ldy snd_v_data_offset,x         
    ; Get the next byte of data.  Check if it's $ff (- that's the end of 
    ; the sound effect).
    lda (SND_INIT_DATA_ZP_LO),y
    cmp #snd_c_SFX_END_CMD
    bne .check_for_oscillator_off

    ; End of sound effect has been reached.
    ; Must loop?
    lda snd_v_must_loop,x
    beq .init_wait
    lda #0
    sta snd_v_data_offset,x
    beq .read_data

.init_wait
    ; Close gate bit for channel.
    iny
    lda (SND_INIT_DATA_ZP_LO),y
    ; Push this value onto stack while we get 'waiting' value...
    pha
    iny
    lda (SND_INIT_DATA_ZP_LO),y
    sta snd_v_waiting_to_end,x
    pla
    ldy #VCREG1-FRELO1
    sta (SND_REGS_BASE_ZP_LO),y
    
.next
    dex
    bpl .loop_top
    rts ; EXIT POINT.

.check_for_oscillator_off   
    cmp #snd_c_OSC_OFF_CMD
    bne .read_frequency
    ; Store counter.
    iny
    lda (SND_INIT_DATA_ZP_LO),y
    sta snd_v_counter,x
    iny
    tya
    sta snd_v_data_offset,x
    ; Close gate.
    ldy #4
    lda snd_v_gates,x
    and #$fe
    sta (SND_REGS_BASE_ZP_LO),y
    bne .next 

.read_frequency
    ; Must be new value for frequency (low byte).
    ; Push FRELO and FREHI onto stack.  We'll deal with them later because
    ; we'll need the Y register to write those values to the SID regs.
    pha
    iny
    lda (SND_INIT_DATA_ZP_LO),y
    pha
    iny
    ; Counter we can store straight away though.
    lda (SND_INIT_DATA_ZP_LO),y
    sta snd_v_counter,x

    ; Increment again and save as offset for next time.
    iny
    tya
    sta snd_v_data_offset,x

    ; Pull FREHI and FRELO off stack and write to relevant SID registers.
    ldy #1
    pla
    sta (SND_REGS_BASE_ZP_LO),y
    dey
    pla
    sta (SND_REGS_BASE_ZP_LO),y
    ; REOPEN GATE!
    ldy #4
    lda snd_v_gates,x
    sta (SND_REGS_BASE_ZP_LO),y
    bne .next

    ; rts not necessary - we'll never get to this point.
;    rts

; end sub snd_s_update
} ; !zone

; **************************************************

;; INPUTS:   X = voice
;!zone {
;snd_s_kill_voice
;    lda #0
;    sta snd_v_channel_active,x
;    lda snd_l_REGS_BASE_ADDR_LO,x
;    sta P0
;    lda snd_l_REGS_BASE_ADDR_HI,x
;    sta P1
;    ldy #4
;    lda #0
;    sta (P0),y
;    rts
;; end sub snd_s_kill_voice
;} ; !zone

; **************************************************

!zone {
snd_s_kill_all
;    ldx #2
;-
;    jsr snd_s_kill_voice
;    dex
;    bpl -

    lda #0
    sta VCREG1
    sta VCREG2
    sta VCREG3
    sta snd_v_channel_active    
    sta snd_v_channel_active+1
    sta snd_v_channel_active+2    

    rts
; end sub snd_s_kill_all
} ; !zone

; **************************************************

!zone {
snd_s_clear_all
    ; First clear all SID registers.
    lda #0
;    ldx #SIGVOL-FRELO1
    ldx #RESON-FRELO1
-   
    sta FRELO1,x
    dex
    bpl -

    ; Now all 'sound engine' variables.
;    ldx #snd_v_SIGVOL_buffer-snd_v_channel_active
    ldx #snd_v_RESON_buffer-snd_v_channel_active
-
    sta snd_v_channel_active,x
    dex
    bpl -

    lda #$ff
    sta snd_v_waiting_to_end
    sta snd_v_waiting_to_end+1
    sta snd_v_waiting_to_end+2

    lda #$0f
    sta SIGVOL
    sta snd_v_SIGVOL_buffer

    rts
; end sub snd_s_clear_all
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

snd_c_SIZE = *-snd_c_BEGIN

