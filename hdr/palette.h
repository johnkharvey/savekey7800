; palette.h
; This header file contains a macro for converting NTSC colors to PAL colors
; as well as a converstion table for easy reference

	processor 6502

	;NTSC to PAL color Macro
	mac	NTSC2PALColor
	;
	; ***********************************
	; Here is the color conversion table:
	; ***********************************
	;------------
	; NTSC   PAL
	;------------
	; $0x    $0x
	; $1x    $3x
	; $2x    $4x
	; $3x    $5x
	; $4x    $6x
	; $5x    $7x
	; $6x    $8x
	; $7x    $9x
	; $8x    $Ax
	; $9x    $Bx
	; $Ax    $Cx
	; $Bx    $Dx
	; $Cx    $EX
	; $Dx    $Fx
	; $Ex    $1x
	; $Fx    $2x
	;
	; ****************
	; Converter macro:
	; ****************
	if	([{1}]>>4 == $00)
		dc.b	[{1}]		; 0->0
		mexit
	endif
	if	([{1}]>>4 == $0E)
		dc.b	[{1}]-$D0	; E->1
		mexit
	endif

	if	([{1}]>>4 == $0F)
		dc.b	[{1}]-$D0	; F->2
	else
		dc.b	[{1}]+$20	; all else
	endif
	endm
