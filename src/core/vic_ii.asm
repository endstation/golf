; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


; Labels for VIC-II registers.
; Sprite horizontal and vertical position registers.
SP0X    = $d000
SP0Y    = $d001
SP1X    = $d002
SP1Y    = $d003
SP2X    = $d004
SP2Y    = $d005
SP3X    = $d006
SP3Y    = $d007
SP4X    = $d008
SP4Y    = $d009
SP5X    = $d00a
SP5Y    = $d00b
SP6X    = $d00c
SP6Y    = $d00d
SP7X    = $d00e
SP7Y    = $d00f
; Most significant bits of sprites 0-7.
MSIGX   = $d010
; Vertical fine scrolling & control register.
SCROLY  = $d011
RASTER  = $d012
; Light pen registers.
LPENX   = $d013
LPENY   = $d014
; Sprite enable register.
SPENA   = $d015
; Horizontal fine scrolling & control register.
SCROLX  = $d016
; Sprite vertical expansion register.
YXPAND  = $d017
; VIC-II chip memory control register.
VMCSB   = $d018
; VIC interrupt flag register.
VICIRQ  = $d019
; IRQ mask register.
IRQMSK  = $d01a
; Sprite to foreground display priority register.
SPBGPR  = $d01b
; Sprite multicolor registers.
SPMC    = $d01c
; Sprite horizontal expansion register.
XXPAND  = $d01d
; Sprite to sprite collision register.
SPSPCL  = $d01e
; Sprite to foreground collision register.
SPBGCL  = $d01f
; Border Color Register.
EXTCOL  = $d020     
BGCOL0  = $d021
BGCOL1  = $d022
BGCOL2  = $d023
BGCOL3  = $d024
; Sprite multicolor register 0.
SPMC0   = $d025
; Sprite multicolor register 1.
SPMC1   = $d026
; Sprite color registers.
SP0COL  = $d027
SP1COL  = $d028
SP2COL  = $d029
SP3COL  = $d02a
SP4COL  = $d02b
SP5COL  = $d02c
SP6COL  = $d02d
SP7COL  = $d02e

; Some other useful stuff.
D6510   = $0
R6510   = $1
; Pointer to highest address used by BASIC in low byte/high byte format.
; MEMSIZ+1 will refer to the high byte.
MEMSIZ  = $37
; Top page of screen memory.
HIBASE  = $288
; CIA#1 data port A.
CIAPRA  = $dc00    
; Data port register A.
CI2PRA  = $dd00
; Data direction register A.
C2DDRA  = $dd02
; Vector to IRQ interrupt routine.
CINV    = $314
; Reserved for use by SCNKEY (Kernal's keyboard scan routine).
SFDX    = $cb


; Kernal routines for disk I/O.
SETNAM  = $ffbd
SETLFS  = $ffba
LOAD    = $ffd5
SCNKEY  = $ff9f
KB_MATRIX_DECODE_TBL = $eb81
SHFLAG  = $028d

; System colors.
BLACK       = 0
WHITE       = 1
RED         = 2
CYAN        = 3
VIOLET      = 4
GREEN       = 5
BLUE        = 6
YELLOW      = 7
ORANGE      = 8
BROWN       = 9
LIGHT_RED   = 10
GREY1       = 11
GREY2       = 12
LIGHT_GREEN = 13
LIGHT_BLUE  = 14 
GREY3       = 15

COLOR_RAM = $d800

; To skip over 2 bytes!
BIT_ABSOLUTE = $2c
BIT_ZEROPAGE = $24

; Labels for SID chip registers.
; VOICE 1
FRELO1  = $d400     ; frequency control (low byte)
FREHI1  = $d401     ; frequency control (high byte)
PWLO1   = $d402     ; pulse waveform width (low byte)
PWHI1   = $d403     ; pulse waveform width (high byte)
VCREG1  = $d404     ; voice 1 control register
ATDCY1  = $d405     ; attack/decay register
SUREL1  = $d406     ; sustain/release register
; VOICE 2
FRELO2  = $d407
FREHI2  = $d408
PWLO2   = $d409
PWHI2   = $d40a
VCREG2  = $d40b
ATDCY2  = $d40c
SUREL2  = $d40d
; VOICE 3
FRELO3  = $d40e
FREHI3  = $d40f
PWLO3   = $d410
PWHI3   = $d411
VCREG3  = $d412
ATDCY3  = $d413
SUREL3  = $d414
; For all voices.
CUTLO   = $d415     ; filter cutoff frequency (low portion)
CUTHI   = $d416     ; filter cutoff frequency (high byte)
RESON   = $d417     ; filter resonance control register
SIGVOL  = $d418     ; volume & filter select register

_DEBUG_APEX_            = 0
_DEBUG_PHYSICS_VARS_    = 0
_DEBUG_TEST_HOLE_       = 0

CB_INITLOADER = $3000
CB_LOADFILE = $059c
CB_LOADFILE_EXOMIZER = $05c5
