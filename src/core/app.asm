; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; *****************
; *** CONSTANTS ***
; *****************


; *****************
; *** VARIABLES ***
; *****************
app_v_filename  !pet    "prelude.prg",0
app_v_filename2 !pet    "play.prg",0
app_v_filename3 !pet    "splash.prg",0
app_v_filename4 !pet    "nineteenth.prg",0
app_v_filename5 !pet    "finale.prg",0


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
!zone {
app_s_run
    jsr app_s_run_splash
.again
    jsr app_s_run_prelude
    jsr app_s_run_play
    jsr app_s_run_nineteenth
    jsr app_s_run_finale
    jmp .again
    rts
; end sub app_s_run
} ; !zone

; **************************************************

!zone {
app_s_run_splash
    ldx #<app_v_filename3
    ldy #>app_v_filename3
    jsr CB_LOADFILE_EXOMIZER
    jsr end_of_core
    rts
; end sub app_s_run_splash
} ; !zone

; **************************************************

!zone {
app_s_run_prelude
    ldx #<app_v_filename  
    ldy #>app_v_filename  
    sei
    jsr CB_LOADFILE_EXOMIZER
    cli
    jsr end_of_core
    rts
; end sub app_s_run_prelude
} ; !zone

; **************************************************

!zone {
app_s_run_play
    ldx #<app_v_filename2 
    ldy #>app_v_filename2 
    jsr CB_LOADFILE_EXOMIZER
    jsr end_of_core
    rts
; end sub app_s_run_play
} ; !zone

; **************************************************

!zone {
app_s_run_nineteenth
    ldx #<app_v_filename4
    ldy #>app_v_filename4
    jsr CB_LOADFILE_EXOMIZER
    jsr end_of_core
    rts
; end sub app_s_run_nineteenth
} ; !zone

; **************************************************

!zone {
app_s_run_finale
    ldx #<app_v_filename5
    ldy #>app_v_filename5
    jsr CB_LOADFILE_EXOMIZER
    jsr end_of_core
    rts
; end sub app_s_run_finale
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

