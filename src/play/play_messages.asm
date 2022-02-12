; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


playmsg_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
playmsg_c_ADJECTIVE_STR !raw "Nice",0,"Good",0,"Great",0,"Super",0
playmsg_c_SHOT_TYPE_STR !raw "Double bogey",0,"Bogey",0,"par",0,"birdie",0,"eagle",0,"putt",0
playmsg_c_NUM_ADJECTIVES = 4
; NOTE: these displayed before player takes shot, if appropriate.
playmsg_c_ALERT_STR     !raw "To save par...Birdie try!For eagle!"
playmsg_l_ALERT_ADDR_LO
    !byte   <playmsg_c_ALERT_STR
    !byte   <(playmsg_c_ALERT_STR+14)
    !byte   <(playmsg_c_ALERT_STR+25)
playmsg_l_ALERT_ADDR_HI
    !byte   >playmsg_c_ALERT_STR
    !byte   >(playmsg_c_ALERT_STR+14)
    !byte   >(playmsg_c_ALERT_STR+25)
playmsg_l_ALERT_LENS    !byte   14,11,10

; NOTE: order these so that they correspond to (par - shots).
playmsg_c_TYPE_DOUBLE_BOGEY = 0
playmsg_c_TYPE_BOGEY        = 1
playmsg_c_TYPE_PAR          = 2
playmsg_c_TYPE_BIRDIE       = 3
playmsg_c_TYPE_EAGLE        = 4
playmsg_c_TYPE_PUTT         = 5

playmsg_l_ADJ_OFFSETS   !byte   0,5,10,16
playmsg_l_TYPE_OFFSETS  !byte   0,13,19,23,30,36

playmsg_c_BUFFER_LEN = 13


; *****************
; *** VARIABLES ***
; *****************
playmsg_v_buffer            !fill   5+6+2
playmsg_v_last_index        !byte   0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; INPUTS: X = shot type.
; OUTPUT: C flag set if par or better, or a praiseworthy putt; else clear.
!zone {
.SHOT_TYPE = MATHS2

playmsg_s_praise_shot
    stx .SHOT_TYPE    
    jsr playmsg_s_clear_buffer

    ; NOTE: that routine above didn't alter X!
    cpx #playmsg_c_TYPE_PAR
    bcs +
    ; We're going to skip the adjective.
    ldy #0
    beq .skip_adjective

+
    ; Pick a random adjective.
    ; FIXME: separate routine in random module?!
    jsr rand_s_get
    and #$03

    ; Put adjective offset into X; Y will keep track of destination offset.
    tax
    lda playmsg_l_ADJ_OFFSETS,x
    tax
    ldy #0
    ; Copy adjective into buffer.
-
    lda playmsg_c_ADJECTIVE_STR,x
    beq +
    sta playmsg_v_buffer,y
    iny
    inx
    bne -

+
    ; Advance destination offset by one space, ready for shot type.
    ; Offset of shot type string goes into X.
    iny
.skip_adjective
    ldx .SHOT_TYPE
    lda playmsg_l_TYPE_OFFSETS,x
    tax
    ; Copy shot type into buffer.
-
    lda playmsg_c_SHOT_TYPE_STR,x
    beq .end_type
    sta playmsg_v_buffer,y
    iny
    inx
    bne -

.end_type
    ; And finally an exclamation mark.
    lda #SCR_CODE_BANG   
    sta playmsg_v_buffer,y
    sty playmsg_v_last_index

;!if _DEBUG_ {
;    ; FIXME: check for bug where incorrect message is displayed (no noun)!
;    lda playmsg_v_buffer-1,y
;    cmp #65     ; 'A'
;    bcc .error
;    cmp #91     ; 'Z'+1
;    bcc .ok
;    cmp #97     ; 'a'
;    bcc .error
;    cmp #123    ; 'z'+1
;    bcc .ok
;.error
;-
;    inc EXTCOL
;    jmp -
;.ok
;} ; !if

    lda #<playmsg_v_buffer
    sta P0
    lda #>playmsg_v_buffer
    sta P1
    lda #playmsg_c_BUFFER_LEN
    sta P4
    jsr msg_s_display

    lda .SHOT_TYPE
    cmp #playmsg_c_TYPE_PAR

    rts
; end sub playmsg_s_praise_shot
} ; !zone

; **************************************************

!zone {
playmsg_s_clear_buffer
    ldy #playmsg_c_BUFFER_LEN-1
    lda #SCR_CODE_SPACE  
-
    sta playmsg_v_buffer,y
    dey
    bpl -

    rts
; end sub playmsg_s_clear_buffer
} ; !zone

; **************************************************

; Display a message if player is putting for par, birdie or eagle...
!zone {
playmsg_s_display_alert
    lda round_v_must_putt
    +branch_if_false .end

    ldx round_v_current_player
    lda hole_v_par
    sec
    sbc players_v_current_shots,x
    tax
    dex
    ; X should be in range [0,2] if alert is valid...
    cpx #3
    bcs .end

    lda playmsg_l_ALERT_ADDR_LO,x
    sta P0
    lda playmsg_l_ALERT_ADDR_HI,x
    sta P1
    lda playmsg_l_ALERT_LENS,x
    sta P4
    jsr msg_s_display

.end
    rts
; end sub playmsg_s_display_alert
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

playmsg_c_SIZE = *-playmsg_c_BEGIN 

