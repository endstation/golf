; Top-hole Golf
; Copyright 2020-2021 Matthew Clarke


camera_c_BEGIN = *


; *****************
; *** CONSTANTS ***
; *****************
; To adjust camera there are two procedures:
; 1) Change focal distance (between lens & plate).  The smaller this distance,
;    the wider the field of view (FOV).
; 2) Move the whole camera (lens & plate together).  As the camera is moved
;    further away, the origin moves further into the visible scene.

; NOTE: when the FOV is too narrow, balls hit to the side have a tendency to
; appear as if they're swerving into the middle of the screen (even though,
; mathematically, they're not).  This can be corrected, to some extent, by 
; increasing the field of view (see above).  
; However it's also the case, or at least seems to be, that any actual spin 
; applied to the ball is more pronounced visually when the ball's initial
; launch direction is to the left or right (and spin acting in the opposite
; direction to this).  Or this could even be a bug in the physics engine -
; i.e. swerve IS actually more pronounced in those circumstances...

CAMERA_Y_LO         =   <64
CAMERA_Y_HI         =   >64
CAMERA_Z_LO         =   <(-153)
CAMERA_Z_HI         =   >(-153)
CAMERA_PLATE_Z_LO   =   <(-278)
CAMERA_PLATE_Z_HI   =   >(-278)
;CAMERA_Z_LO         =   <(-110)
;CAMERA_Z_HI         =   >(-110)
;CAMERA_PLATE_Z_LO   =   <(-200)
;CAMERA_PLATE_Z_HI   =   >(-200)
CAMERA_HORIZON      =   128
CAMERA_Z_MINUS_PLATE_Z_LO   =   <(CAMERA_Z_LO-CAMERA_PLATE_Z_LO)
CAMERA_Z_MINUS_PLATE_Z_HI   =   >(CAMERA_Z_HI-CAMERA_PLATE_Z_HI)
CAMERA_PLATE_DY_LO =   <(128-64)
CAMERA_PLATE_DY_HI =   >(128-64)
; Any Z lower than this will project below the visible screen or not at all
; (- i.e. off to infinity in the opposite direction).
CAMERA_MIN_VALID_Z_LO   =   <(-30)
CAMERA_MIN_VALID_Z_HI   =   >(-30)
CAMERA_MAX_VALID_Z_LO   =   <(150*hole_c_PIXELS_PER_YARD)
CAMERA_MAX_VALID_Z_HI   =   >(150*hole_c_PIXELS_PER_YARD)

; NOTE: total width in pixel elements for C64 is 320, though in multicolor
; bitmap mode (as used here), each 'virtual' pixel is 2 pixels wide.  We'll
; still treat it as being of width 320 here, though, because that's the 
; resolution we'll use to plot the golf ball sprite...
CAMERA_HALF_SCREEN_WIDTH_LO =   <160
CAMERA_HALF_SCREEN_WIDTH_HI =   >160


; *****************
; *** VARIABLES ***
; *****************


; *******************
; ****** MACROS *****
; *******************


; *******************
; *** SUBROUTINES ***
; *******************
; FIXME: this could be made more efficient!!!
; INPUTS:   P0-P1 = object y, P2-P3 = object z
; OUTPUTS:  Y-position is in P0-P1.
;           C flag set if result out of bounds; otherwise clear.
!zone {
.second_term_neg    !byte   0

camera_project_onto_plate_y
    ; NOTE: p = plate, c = camera, o = object.
    ; Equation: py = oy + ((oz - pz) * (cy - oy) / (oz - cz))

    lda #0
    sta .second_term_neg
    ; Save oy in MATHS6-MATHS7 for later.
    lda P0
    sta MATHS6
    lda P1
    sta MATHS7

    ; First, put oz - pz into MATHS0-MATHS1.
    lda P2
    sec
    sbc #CAMERA_PLATE_Z_LO
    sta MATHS0
    lda P3
    sbc #CAMERA_PLATE_Z_HI
    sta MATHS1
    ; And cy - oy into MATHS2-MATHS3.
    lda #CAMERA_Y_LO
    sec
    sbc P0
    sta MATHS2
    lda #CAMERA_Y_HI
    sbc P1
    sta MATHS3
    bcs +
    ; The result was negative, meaning the object is positioned higher than
    ; the camera.  Record this fact and take 2's complement (- the
    ; multiplication and division routines can't handle signed numbers).
    inc .second_term_neg
    ; NOTE: here we trash P0-P1 (which is why we saved oy earlier!).
    ; FIXME: does this work with zeropage addresses?!
    lda #<MATHS2
    sta P0
    lda #>MATHS2
    sta P1
    jsr maths_adjust_vec_signs

+
    ; Finally, oz - cz into MATHS4-MATHS5.
    lda P2
    sec
    sbc #CAMERA_Z_LO
    sta MATHS4
    lda P3
    sbc #CAMERA_Z_HI
    sta MATHS5

    ; Prepare args for the multiplication.
    lda MATHS0
    sta P0
    lda MATHS1
    sta P1
    lda MATHS2 
    sta P2
    lda MATHS3 
    sta P3
    jsr maths_mul16

    ; If the result doesn't fit into 24 bits, we won't draw this object on the
    ; screen.  Result is in P4-P7.
    lda P7
    bne .invalid
-   lda P6
    beq .ok
    ; Divide dividend and divisor by 2 until dividend fits into 16 bits.
    lsr P6
    ror P5
    ror P4
    lsr MATHS5
    ror MATHS4
    jmp -

.ok
    ; FIXME: check for division by zero!!!
    ; Prepare args for division.
    lda P4
    sta P0
    lda P5
    sta P1
    lda MATHS4
    sta P2
    lda MATHS5
    sta P3
    jsr maths_div16

    ; Swap sign if second term was negative...
    lda .second_term_neg
    beq +
    ; Re-use MATHS0-MATHS1 to hold division result (because address must
    ; be loaded into P0-P1 for call to maths_adjust_vec_signs).
    ; FIXME: rubbish & clunky!
    lda P0
    sta MATHS0
    lda P1
    sta MATHS1
    lda #<MATHS0
    sta P0
    lda #>MATHS0
    sta P1
    jsr maths_adjust_vec_signs
    lda MATHS0
    sta P0
    lda MATHS1
    sta P1
+    
    ; Result is in P0-P1.  Add plate-dy.
    lda P0
    clc
    adc #CAMERA_PLATE_DY_LO
    sta P0
    lda P1
    adc #CAMERA_PLATE_DY_HI
    sta P1
    clc
    rts

.invalid
    sec
    rts
; end sub camera_project_onto_plate_y
} ; !zone

; **************************************************

; INPUTS:   CAMERA0-CAMERA1 = object x, CAMERA2-CAMERA3 = object z
; Equation: px = ((cz - pz) * ox) / (oz - cz)
; Then to turn it into a screen position (on x-axis):
;           px' = px * (-1) + 160
; OUTPUT:   CAMERA0-CAMERA1 = screen x.
!zone {
.ox_neg     !byte   0
.div_by_2   !byte   0

camera_project_onto_plate_x
    lda #0
    sta .ox_neg
    sta .div_by_2

    ; If ox is negative, record this fact and take 2's complement.
    lda CAMERA1
    bpl +
    inc .ox_neg
    lda #<CAMERA0
    sta P0
    lda #>CAMERA1
    sta P1
    jsr maths_adjust_vec_signs
+

    ; Do the multiplication.
    lda CAMERA0
    sta P0
    lda CAMERA1
    sta P1
    lda #CAMERA_Z_MINUS_PLATE_Z_LO
    sta P2
    lda #CAMERA_Z_MINUS_PLATE_Z_HI
    sta P3
    jsr maths_mul16
    ; NOTE: make sure result fits into 16 bits.
    ; If not, keep dividing by 2 until it does.
    ; X counts how many times we had to divide.
    ldx #0
-   lda P6
    beq +
    lsr P7
    ror P6
    ror P5
    ror P4
    inx
    jmp -
+
    ; Result into P0-P1 ready for division.
    lda P4
    sta P0
    lda P5
    sta P1

    ; oz - cz into P2-P3.
    lda CAMERA2
    sec
    sbc #CAMERA_Z_LO
    sta P2
    lda CAMERA3
    sbc #CAMERA_Z_HI
    sta P3
    ; May need to divide by 2 first...  X register still holds number of
    ; times that needs to happen.
-   cpx #0
    beq +
    lsr P3
    ror P2
    dex
    jmp -
+   
    jsr maths_div16
    
    ; Result into CAMERA0-CAMERA1.
    lda P0
    sta CAMERA0
    lda P1
    sta CAMERA1
    ; Swap sign if necessary.
    lda .ox_neg
    beq +
    lda #<CAMERA0
    sta P0
    lda #>CAMERA0
    sta P1
    jsr maths_adjust_vec_signs
+

    ; Now add 160 (i.e. half screen width).
    ; FIXME: what if multicolor bitmap/char mode?!
    lda CAMERA0
    clc
    adc #CAMERA_HALF_SCREEN_WIDTH_LO
    sta CAMERA0
    lda CAMERA1
    adc #CAMERA_HALF_SCREEN_WIDTH_HI
    sta CAMERA1
    rts
; end sub camera_project_onto_plate_x
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
; **************************************************

camera_c_SIZE = *-camera_c_BEGIN 

