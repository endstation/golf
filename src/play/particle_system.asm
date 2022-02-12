; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


partsys_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
partsys_c_SPR_NUM = 4

partsys_c_SPR_DATA_ADDR = $c000+(sprd_c_PARTICLES_SHOT*64) 
partsys_v_OFFSCREEN_BUFFER  !fill   64

partsys_l_SPR_ROWS_LO
    !for i,21 {
        !byte <(partsys_v_OFFSCREEN_BUFFER+((21-i)*3))
    } ; !for
partsys_l_SPR_ROWS_HI
    !for i,21 {
        !byte >(partsys_v_OFFSCREEN_BUFFER+((21-i)*3))
    } ; !for
; Divide x-position by 8 to get correct byte offset (0-2), and then remainder
; (i.e. least-significant 3 bits) is index into this table.
partsys_l_BYTE_PATTERNS
    !byte   $80,$40,$20,$10,$08,$04,$02,$01

partsys_c_MAX_PARTICLES = 8
partsys_c_ORIGIN_X = 12

partsys_c_SPR_X = spr_c_VISIBLE_ALL_L+160-24 
partsys_c_SPR_Y = spr_c_VISIBLE_ALL_T+180-21-21;-24

; Use this to initialize the 'partsys_alive' variable - index is 'partsys_n'.   
; NOTE: first value is a dummy so indices will be correctly aligned.
partsys_l_ALIVE_LOOKUP    
    !byte   0,$01,$03,$07,$0f,$1f,$3f,$7f,$ff

; NOTE: these are for vy-lo; vy-hi is always 1.
partsys_l_BASE_VY_LO    !byte   3,10,20,25,150
partsys_l_VY_HI         !byte   1,1,1,1,0
; NOTE: use powarc_v_power_offset to index this table.  For each iteration,
; check if offset is < table entry.
partsys_l_POWER_STEPS       !byte   9,17,25,32
partsys_l_HOW_MANY          !byte   3,5,7,8,7
partsys_l_BASE_LAND         !byte   2,2,3,3,0
partsys_l_LAND_DELTA_MAX    !byte   2,2,2,2,1
partsys_l_LAND_DELTA_MASK   !byte   $03,$03,$03,$03,$02
; Index into this array is 'ball_v_current_terrain'.
partsys_l_COLORS    !byte   GREY3,LIGHT_RED,BROWN,CYAN

partsys_c_G_DELAY = 1


; *****************
; *** VARIABLES ***
; *****************
partsys_v_n     !byte   0
partsys_x_lo    !fill   partsys_c_MAX_PARTICLES
partsys_x_hi    !fill   partsys_c_MAX_PARTICLES
partsys_y_lo    !fill   partsys_c_MAX_PARTICLES
partsys_y_hi    !fill   partsys_c_MAX_PARTICLES
partsys_vx_lo   !fill   partsys_c_MAX_PARTICLES
partsys_vx_hi   !fill   partsys_c_MAX_PARTICLES
partsys_vy_lo   !fill   partsys_c_MAX_PARTICLES
partsys_vy_hi   !fill   partsys_c_MAX_PARTICLES
partsys_land_y  !fill   partsys_c_MAX_PARTICLES
partsys_g_lo    !byte   0
partsys_g_hi    !byte   0
;partsys_g_count !byte   0
partsys_color   !byte   0
partsys_v_alive !byte   0
partsys_v_animating   !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS:   P0 = shot power
!zone {
.BASE_VY_LO         = MATHS0
.VY_HI              = MATHS1
.BASE_LAND          = MATHS2
.LAND_DELTA_MAX     = MATHS3
.LAND_DELTA_MASK    = MATHS3

partsys_s_init
    lda #sprd_c_PARTICLES_SHOT
    sta spr_v_current_ptr+partsys_c_SPR_NUM

    ; Select base-vy for particles based on shot power and store.
    ldx #0
-
    lda P0
    cmp partsys_l_POWER_STEPS,x
    bcc .found
    inx
    cpx #4
    bne -

.found
    lda partsys_l_BASE_VY_LO,x
    sta .BASE_VY_LO
    lda partsys_l_VY_HI,x
    sta .VY_HI
    lda partsys_l_HOW_MANY,x
    sta partsys_v_n
    lda partsys_l_BASE_LAND,x
    sta .BASE_LAND
    lda partsys_l_LAND_DELTA_MAX,x
    sta .LAND_DELTA_MAX
    lda partsys_l_LAND_DELTA_MASK,x
    sta .LAND_DELTA_MASK
    ldx partsys_v_n
    lda partsys_l_ALIVE_LOOKUP,x
    sta partsys_v_alive

    ; Gravity.
    lda #$0c
    sta partsys_g_lo
    lda #0
    sta partsys_g_hi

    ; Use X for particle count.
    ldx #0

.loop_top
    ; Position.
    lda #0
    sta partsys_y_lo,x
    sta partsys_y_hi,x
    sta partsys_x_lo,x
    lda #partsys_c_ORIGIN_X 
    sta partsys_x_hi,x

    lda #0
    sta partsys_vx_hi,x
    lda .VY_HI
    sta partsys_vy_hi,x

    ; Add a small variation to vy.
    jsr rand_s_get_fast
    and #$0f
    clc
    adc .BASE_VY_LO
    sta partsys_vy_lo,x

    ; And set vx.
    jsr rand_s_get_fast
    bmi .neg_vx
    and #$1f
    sta partsys_vx_lo,x
    jmp +
.neg_vx
    and #$1f
    eor #$ff
    clc
    adc #1
    sta partsys_vx_lo,x
    ; Set high byte to $ff!
    dec partsys_vx_hi,x

+
    ; Where to land?
    jsr rand_s_get_fast
    and .LAND_DELTA_MASK
    cmp .LAND_DELTA_MAX
    beq +
    bcc +
    lda .LAND_DELTA_MAX
+
    clc
    adc .BASE_LAND
    sta partsys_land_y,x

    ; Go to next particle.
    inx
    cpx partsys_v_n
    bne .loop_top

    ; Select color based on current terrain.
    ldx ball_v_current_terrain
    lda partsys_l_COLORS,x
    sta partsys_color

;    lda #partsys_c_G_DELAY
;    sta partsys_g_count

    sec
    jsr partsys_s_clear_sprite

    ; Set sprite position, color and data ptr and then enable it.
    lda #partsys_c_SPR_X 
    sta spr_v_x_lo+partsys_c_SPR_NUM 
    lda #0
    sta spr_v_x_hi+partsys_c_SPR_NUM 
    lda #partsys_c_SPR_Y 
    sta spr_v_y+partsys_c_SPR_NUM 
    lda partsys_color
    sta spr_v_color+partsys_c_SPR_NUM

    lda #1
    sta spr_v_yxpand+partsys_c_SPR_NUM
    sta spr_v_xxpand+partsys_c_SPR_NUM

    rts
; end sub partsys_s_init
} ; !zone

; **************************************************

!zone {
partsys_s_update
    lda partsys_v_animating
    +branch_if_true +
    rts ; EXIT POINT - not animating so nothing to do.

+
    clc
    jsr partsys_s_clear_sprite

    ; Update gravity.
;    dec partsys_g_count
;    bne +
;    lda #partsys_c_G_DELAY
;    sta partsys_g_count
;
;+
    ldx partsys_v_n
    dex

.loop_top
    ; Is this particle still alive?
    lda partsys_v_alive
    and utils_l_BIT_LOOKUP,x
    bne +
    jmp .draw

+
stop_here
    ; Update vy with gravity.
    lda partsys_vy_lo,x
    sec
    sbc partsys_g_lo
    sta partsys_vy_lo,x
    lda partsys_vy_hi,x
    sbc partsys_g_hi
    sta partsys_vy_hi,x

    ; Move particle on x-axis.
    ; BUG: make sure particles stay within sprite area!
    lda partsys_x_lo,x
    clc 
    adc partsys_vx_lo,x
    sta partsys_x_lo,x
    lda partsys_x_hi,x
    adc partsys_vx_hi,x
    sta partsys_x_hi,x
    ; FIXME: or just lower velocity values?!
    bmi .clamp_left
    cmp #24
    bcc ++
    ; Clamp right.
    lda #23
    jmp +
.clamp_left
    lda #0
+   sta partsys_x_hi,x
++

    ; Move particle on y-axis.
    lda partsys_y_lo,x
    clc
    adc partsys_vy_lo,x
    sta partsys_y_lo,x
    lda partsys_y_hi,x
    adc partsys_vy_hi,x
    sta partsys_y_hi,x
    ; Check we've not gone too high!  Only do this if particle is climbing
    ; (i.e. vy is positive).
    ldy partsys_vy_hi,x
    bmi +
    cmp #21
    bcc +
    lda #20
    sta partsys_y_hi,x
+

    ; Check whether particle should now land.
    lda partsys_vy_hi,x
    bpl .draw
    lda partsys_y_hi,x
    bpl +
    ; Y-position has become negative so it's time to land.
    lda partsys_land_y,x
    sta partsys_y_hi,x
    jmp .land
+
    cmp partsys_land_y,x
    beq .land
    bcs .draw

.land
    ; This particle has now expired (though we still draw it!)...
    ; Clear the relevant bit in partsys_alive.
    lda partsys_v_alive
    and utils_l_EOR_BIT_LOOKUP,x
    sta partsys_v_alive

.draw
    ; Draw particle.
    ; First need to find the row.
    lda partsys_y_hi,x
    tay
    lda partsys_l_SPR_ROWS_LO,y
    sta PARTSYS_LO
    lda partsys_l_SPR_ROWS_HI,y
    sta PARTSYS_HI
    ; Now add column - it's x-position divided by 8.
    lda partsys_x_hi,x
    lsr
    lsr
    lsr
    clc
    adc PARTSYS_LO
    sta PARTSYS_LO
    lda PARTSYS_HI
    adc #0
    sta PARTSYS_HI
    ; Find the pattern that we're going to 'ORA' into this byte.  Index into
    ; PARTSYS_BYTE_PATTERNS table is remainder of x-position after dividing
    ; by 8.
    lda partsys_x_hi,x
    and #$07
    tay
    lda partsys_l_BYTE_PATTERNS,y 
    ldy #0
    ora (PARTSYS_LO),y
    sta (PARTSYS_LO),y

    dex 
    bmi .end
    jmp .loop_top

.end
    ; We'll stop animating the system if all particles have expired.
    lda partsys_v_alive
    +branch_if_true +
    sta partsys_v_animating

+
    rts
; end sub partsys_s_update
} ; !zone

; **************************************************

; INPUTS:   C flag set = clear buffer and sprite data
;           C flag clear = just buffer
!zone {
partsys_s_clear_sprite
    ; FIXME: hack!  So junk doesn't appear at beginning of shot (before
    ; particle system is used).
    lda #sprd_c_PARTICLES_SHOT
    sta spr_v_current_ptr+partsys_c_SPR_NUM

    ldx #63
    lda #0
    
    bcc .just_buffer
        
-   sta partsys_v_OFFSCREEN_BUFFER,x
    sta partsys_c_SPR_DATA_ADDR,x
    dex
    bpl -
    rts ; EXIT POINT.

.just_buffer
-   sta partsys_v_OFFSCREEN_BUFFER,x
    dex
    bpl -

    rts
; end sub partsys_s_clear_sprite
} ; !zone

; **************************************************

!zone {
partsys_s_start
    lda #1
    sta partsys_v_animating
    ldx #partsys_c_SPR_NUM  ;partsys_v_current_spr_num
    +spr_m_enable
    rts
; end sub partsys_s_start
} ; !zone

; **************************************************

!zone {
partsys_s_copy_buffer_to_vram
    ldx #63
-
    lda partsys_v_OFFSCREEN_BUFFER,x
    sta partsys_c_SPR_DATA_ADDR,x
    dex
    bpl -
    rts
; end sub partsys_s_copy_buffer_to_vram
} ; !zone

; **************************************************

!zone {
partsys_s_draw
    ldx #partsys_c_SPR_NUM
    ldy #partsys_c_SPR_NUM
    jsr spr_s_write_to_vic_ii
    rts
; end sub partsys_s_draw
} ; !zone

; **************************************************

!zone {
partsys_s_deactivate
    lda #0
    sta partsys_v_animating
    rts
; end sub partsys_s_deactivate
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

partsys_c_SIZE = *-partsys_c_BEGIN
