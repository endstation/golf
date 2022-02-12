; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


duck_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; NOTE: each frame of data is 16 bytes in length.
duck_l_IMAGE_DATA  
    !bin "../../assets/sprites/duck_frames.bin"
; This table stores the LAST valid index for each frame!!!
duck_l_IMAGE_OFFSETS   !byte   15,31,47,63,79

; States:
duck_c_STATE_INACTIVE       = 0
duck_c_STATE_CHILLING       = 1
duck_c_STATE_DIVING         = 2
duck_c_STATE_FEEDING        = 3
duck_c_STATE_RESURFACING    = 4

duck_c_FIRST_POSSIBLE_ROW = 19
duck_l_FIRST_POSSIBLE_COLUMNS   !byte   8,24

duck_c_FRAME_RATE = 5


; *****************
; *** VARIABLES ***
; *****************
duck_v_current_state    !byte   0
; Destination on the bitmap.
duck_v_dest_lo          !byte   0
duck_v_dest_hi          !byte   0
duck_v_current_frame    !byte   0
duck_v_delay_count      !byte   0
duck_v_feed_duration    !byte   0
duck_v_row_request      !byte   0
duck_v_column_request   !byte   0
; NOTE: only bits #0 and #1 are of any significance here.
duck_v_cells_free       !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
duck_s_init
    lda duck_v_cells_free
    cmp #%00000011
    bne .end

    ; Set bitmap destination address.  Must also find video RAM address and
    ; set color code 10 (lower nybble) to be white.
    lda duck_v_row_request
    asl
    asl
    asl
    tax
    lda duck_v_column_request
    asl
    asl
    tay

    lda dp_l_BITMAP_ROWS_LO,x
    clc
    adc dp_l_BITMAP_COLS_LO,y
    sta duck_v_dest_lo
    lda dp_l_BITMAP_ROWS_HI,x
    adc dp_l_BITMAP_COLS_HI,y
    sta duck_v_dest_hi

    ldx duck_v_row_request
    lda dp_l_VIDEO_RAM_ROWS_LO,x
    clc
    adc duck_v_column_request
    sta MATHS0
    lda dp_l_VIDEO_RAM_ROWS_HI,x
    adc #0
    sta MATHS1
    ldy #0
    lda #WHITE|(YELLOW<<4)
    sta (MATHS0),y
    iny
    sta (MATHS0),y

    jsr duck_s_redraw
    ; State will currently be INACTIVE (as set in 'duck_s_request_position').
    inc duck_v_current_state

.end
    rts
; end sub duck_s_init
} ; !zone

; **************************************************

!zone {
duck_s_update
    lda duck_v_current_state
    bne +
    rts ; EXIT POINT.

+
    cmp #duck_c_STATE_CHILLING
    beq .chilling
    cmp #duck_c_STATE_DIVING
    beq .diving
    cmp #duck_c_STATE_FEEDING
    beq .feeding

    ; So must be RESURFACING...
    dec duck_v_delay_count
    bne .end
    dec duck_v_current_frame
    php
    jsr duck_s_redraw
    plp
    beq .start_chilling
    lda #duck_c_FRAME_RATE
    sta duck_v_delay_count
    rts ; EXIT POINT.
.start_chilling
    lda #duck_c_STATE_CHILLING
    sta duck_v_current_state
    rts ; EXIT POINT.

.chilling
    lda duck_v_delay_count
    bne +

    ; Set random wait time till dive.
    jsr duck_s_get_wait_time
    sta duck_v_delay_count

+
    dec duck_v_delay_count
    bne .end
    ; Start to dive:
    inc duck_v_current_state
    inc duck_v_current_frame
    lda #duck_c_FRAME_RATE
    sta duck_v_delay_count
    jsr duck_s_redraw
    rts ; EXIT POINT.

.diving
    dec duck_v_delay_count
    bne .end
    ; We are ready for the next frame of animation.
    lda #duck_c_FRAME_RATE
    sta duck_v_delay_count
    inc duck_v_current_frame
    jsr duck_s_redraw
    lda duck_v_current_frame
    cmp #3
    beq .start_feeding
    rts ; EXIT POINT.
.start_feeding
    inc duck_v_current_state
    jsr duck_s_get_wait_time
    sta duck_v_feed_duration
    rts ; EXIT POINT.

.feeding
    dec duck_v_feed_duration
    bne .end
;    lda #duck_c_FRAME_RATE
;    sta duck_v_delay_count
;    dec duck_v_feed_duration
;    beq .start_resurfacing
;    ; TODO: 'feeding' animation?!
;    rts ; EXIT POINT.
;.start_resurfacing
    inc duck_v_current_state
    lda #2
    sta duck_v_current_frame
    jsr duck_s_redraw
    rts ; EXIT POINT.

.end
    rts
; end sub duck_s_update
} ; !zone

; **************************************************

!zone {
duck_s_redraw
    lda duck_v_dest_lo
    sta MATHS0
    lda duck_v_dest_hi
    sta MATHS1

    ldx duck_v_current_frame
    lda duck_l_IMAGE_OFFSETS,x
    tax

    ldy #15
-
    lda duck_l_IMAGE_DATA,x
    sta (MATHS0),y
    dex
    dey
    bpl -

    rts
; end sub duck_s_redraw
} ; !zone

; **************************************************

; FIXME: this may overwrite map on rhs?!
!zone {
duck_s_request_position
    lda #0
    sta duck_v_cells_free
    sta duck_v_current_state
    sta duck_v_current_frame
    sta duck_v_delay_count
    
    jsr rand_s_get
    and #%00000011
    clc
    adc #duck_c_FIRST_POSSIBLE_ROW
    sta duck_v_row_request

    jsr rand_s_get
    pha
    ; Use bit #0 to determine whether we want the left- or right-hand side.
    and #$01
    tax
    pla
    and #%00000111
    clc
    adc duck_l_FIRST_POSSIBLE_COLUMNS,x
    sta duck_v_column_request

    rts
; end sub duck_s_request_position
} ; !zone

; **************************************************

; INPUTS:   CAMERA0 = row, CAMERA1 = column
; OUTPUTS:  C flag set if we want to reserve this cell, otherwise clear.
!zone {
.CHAR_ROW = CAMERA0
.CHAR_COL = CAMERA1

duck_s_check_cell
    lda .CHAR_ROW
    cmp duck_v_row_request
    bne .do_not_reserve
    lda .CHAR_COL
    sec
    sbc duck_v_column_request
    beq .reserve
    cmp #1
    bne .do_not_reserve

.reserve
    ; So we do want to reserve this cell.  Accumulator holds either 0 (if lhs)
    ; or 1 (if rhs)...
    tax
    lda duck_v_cells_free
    ora utils_l_BIT_LOOKUP,x
    sta duck_v_cells_free
    sec
    rts ; EXIT POINT.
    
.do_not_reserve
    clc
    rts
; end sub duck_s_check_cell
} ; !zone

; **************************************************

; OUTPUT:   a random wait time between 128 and 255 frames.
!zone {
duck_s_get_wait_time
    jsr rand_s_get_fast
    and #%01111111
    clc
    adc #128
    rts
; end sub duck_s_get_wait_time
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

duck_c_SIZE = *-duck_c_BEGIN

