; *****************
; *** CONSTANTS ***
; *****************
; 8 slots to use for subroutine parameters (or return values).
P0 = $02
P1 = $03
P2 = $04
P3 = $05
P4 = $06
P5 = $07
P6 = $08
P7 = $09

; 8 slots for use inside maths routines.
MATHS0 = $0a
MATHS1 = $0b
MATHS2 = $0c
MATHS3 = $0d
MATHS4 = $0e
MATHS5 = $0f
MATHS6 = $10
MATHS7 = $11

XCOS_LO = $12
XCOS_HI = $13
XSIN_LO = $14
XSIN_HI = $15
ZCOS_LO = $16
ZCOS_HI = $17
ZSIN_LO = $18
ZSIN_HI = $19

; This is the last value that needs to be saved when entering an
; interrupt routine.
SIGN_CHANGED = $1a

TREES_LO = $1b
TREES_HI = $1c

CAMERA0 = $1d
CAMERA1 = $1e
CAMERA2 = $1f
CAMERA3 = $20

BITMAP_LO = $21
BITMAP_HI = $22
PATTERN_LO = $23
PATTERN_HI = $24
VM_LO = $25
VM_HI = $26
COLORS_LO = $27
COLORS_HI = $28

LINE_X0_LO = $29
LINE_Y0_LO = $2a
LINE_X1_LO = $2b
LINE_Y1_LO = $2c

EDGES_LO = $2d
EDGES_HI = $2e

; Use this with the 'wind' and 'slope' modules.
WS_X_LO = $2f
WS_X_HI = $30
WS_Z_LO = $31
WS_Z_HI = $32

PARTSYS_LO = $33
PARTSYS_HI = $34

BALL_TRI_X_LO = $35
BALL_TRI_X_HI = $36
BALL_TRI_Z_LO = $37
BALL_TRI_Z_HI = $38

POWARC_LO = $39
POWARC_HI = $3a
POWARC_FILL_SRC_ITER_LO = $3b
POWARC_FILL_SRC_ITER_HI = $3c
POWARC_COPY_LO = $3d
POWARC_COPY_HI = $3e

INTERRUPTS_LO = $3f
INTERRUPTS_HI = $40

RANDOM_X_TMP = $41

; For exclusive use of 'titles2' module (fading stuff).
FADE_CR_LO = $42
FADE_CR_HI = $43
FADE_SR_LO = $44
FADE_SR_HI = $45
FADE_CR_SRC_LO = $46
FADE_CR_SRC_HI = $47
FADE_SR_SRC_LO = $48
FADE_SR_SRC_HI = $49

; When signing in (editing name).
CURSOR_POS_LO = $4a
CURSOR_POS_HI = $4b
CURSOR_POS_SR_LO = $4c
CURSOR_POS_SR_HI = $4d

SND_CH1_DATA_ZP_LO = $4e
SND_CH1_DATA_ZP_HI = $4f
SND_CH2_DATA_ZP_LO = $50
SND_CH2_DATA_ZP_HI = $51
SND_CH3_DATA_ZP_LO = $52
SND_CH3_DATA_ZP_HI = $53
; Temporary zp store for 'init' data.
SND_INIT_DATA_ZP_LO = $54
SND_INIT_DATA_ZP_HI = $55
; Temporary zp store for SID regs base address.
SND_REGS_BASE_ZP_LO = $56
SND_REGS_BASE_ZP_HI = $57

; For each section, where 'init' and 'note' data for set of sfx is stored.
; 'sfx' module must initialize these on start-up so they can be used by
; sound engine.
SFX_INIT_ADDRS_ZP_LO = $58
SFX_INIT_ADDRS_ZP_HI = $59
SFX_DATA_ADDRS_ZP_LO = $5a
SFX_DATA_ADDRS_ZP_HI = $5b

; NOTE: source is offscreen copy of the power-arc icon.
; Destination is the bitmap.
; FIXME: probably don't need separate z.p. variables for this!!!
PUTT_ASSIST_SRC_ZP_LO = $5c
PUTT_ASSIST_SRC_ZP_HI = $5d
PUTT_ASSIST_DEST_ZP_LO = $5e
PUTT_ASSIST_DEST_ZP_HI = $5f

; 12 slots required by loader.
ZP_LOADER_01 = $60
ZP_LOADER_02 = $61
ZP_LOADER_03 = $62
ZP_LOADER_04 = $63
ZP_LOADER_05 = $64
ZP_LOADER_06 = $65
ZP_LOADER_07 = $66
ZP_LOADER_08 = $67
ZP_LOADER_09 = $68
ZP_LOADER_10 = $69
ZP_LOADER_11 = $6a
ZP_LOADER_12 = $6b

; Copies of zp values go here.


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


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

ZP_END = $6c
