;################################################################
; SaveKey demo for the Atari 7800
; by John K. Harvey
;################################################################

	include "hdr/maria.h"
	include "hdr/palette.h"

	; SaveKey / AtariVox functions (not included)
	include "hdr/savekey/i2c7800.inc"

	seg.u	RAMdata

;################################################################
; Zero Page RAM
	ORG	$40	;	$40-$FF
;################################################################
PalOrNtsc			ds	1	;	$40
SaveKeyScratch			ds	1	;	$41 ; SaveKey needs a 1-byte scratchpad
PrevJoystickState		ds	1	;	$42
HighlightIndex			ds	1	;	$43
IndirPtr			ds	2	;	$44-$45
; Canary RAM address, courtesy of Atariage user RevEng
Canary				ds	1	;	$46

;################################################################
; CHMAP RAM
	ORG	$1800
;################################################################

CHMAP_RAM_Start			ds	256	;	$1800-$18FF

;################################################################
; DL RAM
	ORG	$1900
;################################################################
DL_RAM_Start			ds	512	;	$1900-$1AFF

;################################################################
; Another RAM area (this is a good place for SaveKey info)
	ORG	$1F00
;################################################################
Save_Info_RAM	ds	8


;################################################################
; End of Memory 1 - for origin reverse-indexed trapping
	ORG	$203F
;################################################################


;################################################################
; Control equates
	ORG	$2200
;################################################################


;################################################################
	ORG	$2300
;################################################################


;################################################################
; DLL RAM
	ORG	$2400
;################################################################

DLLRam:

;################################################################
; CHMap RAM
	ORG	$2500
;################################################################


;################################################################
; End of Memory 2 - for origin reverse-indexed trapping
	ORG	$27FF
;################################################################


;################################################################
; Equates
;################################################################

GRAPHICS_SPACE		equ	$00	; a null character graphic pointer

PALETTE0		equ	#%00000000
BLUE			equ	PALETTE0

PALETTE1		equ	#%00100000
GREEN			equ	PALETTE1

PALETTE2		equ	#%01000000
RED			equ	PALETTE2

PALETTE3		equ	#%01100000
YELLOW			equ	PALETTE3

PALETTE4		equ	#%10000000
GREY			equ	PALETTE4

PALETTE5		equ	#%10100000
WHITE			equ	PALETTE5

PALETTE6		equ	#%11000000
PALETTE7		equ	#%11100000

; SaveKey equates
SK_BYTES		equ	3		; define how many bytes your want to store
SAVEKEY_ADR		equ	$1E80		; ask Albert for a free slot!
						; This lines up with: 1E80 - 1EBF
						; 1EC0 - 1EFF
						; 1F00 - 1F3F
						; see https://atariage.com/atarivox/atarivox_mem_list.html

;################################################################
; Let's define macros here
;################################################################

  MAC STR_LEN ; string, name
.start
	dc.b	{1}
STR_LEN_{2} = . - .start
  ENDM

	seg	code

;################################################################
	ORG	$4000
;################################################################

;==========================================================
; Calling this macro puts some routines from "i2c7800.inc"
; into source code, plus defines ; SaveKeyScratch as the
; "scratchpad" zero-page address parameter used for
; temporary storage in SaveKey operations
;==========================================================
	i2c_subs	SaveKeyScratch  ; this requires a "scratchpad" zero-page address as a parameter

;===========================================================
; A model of this code can be found in the file:
;  * AtariVoxDevelopment.pdf
; that can be found as an attachment here:
;  * https://forums.atariage.com/applications/core/interface/file/attachment.php?id=322257
;===========================================================

;===============
; Write SaveKey
;===============

WriteSaveKey
	JSR	i2c_startwrite ; Start signal and $a0 command byte
	BCS	eeprom_error ; exit if command byte not acknowledged
	LDA	#$30 ; upper byte of address.  We'll use the Scratchpad address as defined in https://atariage.com/atarivox/atarivox_mem_list.html
	JSR	i2c_txbyte
	LDA	#$00 ; lower byte of address.  We'll use the Scratchpad address as defined in https://atariage.com/atarivox/atarivox_mem_list.html
	JSR	i2c_txbyte
	LDX	#$00
write_loop
	LDA	Save_Info_RAM,x ; get byte from RAM
	JSR	i2c_txbyte ; transmit to EEPROM
	INX
	CPX	#$08 ; 8 bytes sent?
	BNE	write_loop
	JSR	i2c_stopwrite ; terminate write and commit to memory
	LDA	#0 ; Let's set the Accumulator to Zero as a "success" error code
	RTS

;===============
; Read SaveKey
;===============

ReadSaveKey
	JSR	i2c_startwrite ; Start signal and $a0 command byte
	BCS	eeprom_error ; exit if command byte not acknowledged
	LDA	#$30 ; upper byte of address.  We'll use the Scratchpad address as defined in https://atariage.com/atarivox/atarivox_mem_list.html
	JSR	i2c_txbyte
	LDA	#$00 ; lower byte of address.  We'll use the Scratchpad address as defined in https://atariage.com/atarivox/atarivox_mem_list.html
	JSR	i2c_txbyte
	JSR	i2c_stopwrite ; end of “fake” write
	JSR	i2c_startread ; Start signal and $a1 command byte
	LDX	#$00
read_loop
	JSR	i2c_rxbyte ; read byte from EEPROM
	STA	Save_Info_RAM,x ; store in buffer
	INX
	CPX	#$08 ; 8 bytes read?
	BNE	read_loop
	JSR	i2c_stopread ; terminate read
	LDA	#0 ; Let's set the Accumulator to Zero as a "success" error code
	RTS

;================
; Detect SaveKey
;================
DetectSaveKey
	JSR	i2c_startwrite ; Start signal and $a0 command byte
	BCS	eeprom_error ; exit if command byte not acknowledged
	LDA	#$30 ; upper byte of address.  We'll use the Scratchpad address as defined in https://atariage.com/atarivox/atarivox_mem_list.html
	JSR	i2c_txbyte
	LDA	#$00 ; lower byte of address.  We'll use the Scratchpad address as defined in https://atariage.com/atarivox/atarivox_mem_list.html
	JSR	i2c_txbyte
	JSR	i2c_stopwrite
	LDA	#0 ; Let's set the Accumulator to Zero as a "success" error code
	RTS

;===========================
; Handle any SaveKey Errors
;===========================
eeprom_error
	LDA	#$ff ; Let's set the Accumulator to a non-zero value ($FF) as a "failure" error code
	RTS

;################################################################
; Graphics data for 128 characters
	ORG	$8000
;################################################################

; graphics data for 128 characters - line 8
	dc.b	$00,$7E,$7E,$00,$00,$7C,$7C,$00
	dc.b	$FF,$00,$FF,$78,$18,$E0,$C0,$99
	dc.b	$00,$00,$18,$00,$00,$78,$00,$FF
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$60,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$60,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$FF
	dc.b	$00,$00,$00,$00,$00,$00,$00,$F8
	dc.b	$00,$00,$78,$00,$00,$00,$00,$00
	dc.b	$F0,$1E,$00,$00,$00,$00,$00,$00
	dc.b	$00,$F8,$00,$00,$00,$00,$00,$00
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"
 
	ALIGN	256
; graphics data for 128 characters - line 7
	dc.b	$00,$81,$FF,$10,$10,$38,$38,$00
	dc.b	$FF,$3C,$C3,$CC,$7E,$F0,$E6,$5A
	dc.b	$80,$02,$3C,$66,$1B,$CC,$7E,$18
	dc.b	$18,$18,$00,$00,$00,$00,$00,$00
	dc.b	$00,$30,$00,$6C,$30,$C6,$76,$00
	dc.b	$18,$60,$00,$00,$30,$00,$30,$80
	dc.b	$7C,$FC,$FC,$78,$1E,$78,$78,$30
	dc.b	$78,$70,$30,$30,$18,$00,$60,$30
	dc.b	$78,$CC,$FC,$3C,$F8,$FE,$F0,$3E
	dc.b	$CC,$78,$78,$E6,$FE,$C6,$C6,$38
	dc.b	$F0,$1C,$E6,$78,$78,$FC,$30,$C6
	dc.b	$C6,$78,$FE,$78,$02,$78,$00,$00
	dc.b	$00,$76,$DC,$78,$76,$78,$F0,$0C
	dc.b	$E6,$78,$CC,$E6,$78,$C6,$CC,$78
	dc.b	$60,$0C,$F0,$F8,$18,$76,$30,$6C
	dc.b	$C6,$0C,$FC,$1C,$18,$E0,$00,$FE
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"

	ALIGN	256
; graphics data for 128 characters - line 6
	dc.b	$00,$99,$E7,$38,$38,$7C,$7C,$18
	dc.b	$E7,$66,$99,$CC,$18,$70,$67,$3C
	dc.b	$E0,$0E,$7E,$00,$1B,$38,$7E,$3C
	dc.b	$18,$3C,$18,$30,$FE,$24,$FF,$18
	dc.b	$00,$00,$00,$6C,$F8,$66,$CC,$00
	dc.b	$30,$30,$66,$30,$30,$00,$30,$C0
	dc.b	$E6,$30,$CC,$CC,$0C,$CC,$CC,$30
	dc.b	$CC,$18,$30,$30,$30,$FC,$30,$00
	dc.b	$C0,$CC,$66,$66,$6C,$62,$60,$66
	dc.b	$CC,$30,$CC,$66,$66,$C6,$C6,$6C
	dc.b	$60,$78,$66,$CC,$30,$CC,$78,$EE
	dc.b	$6C,$30,$66,$60,$06,$18,$00,$00
	dc.b	$00,$CC,$66,$CC,$CC,$C0,$60,$7C
	dc.b	$66,$30,$CC,$6C,$30,$D6,$CC,$CC
	dc.b	$7C,$7C,$60,$0C,$34,$CC,$78,$FE
	dc.b	$6C,$7C,$64,$30,$18,$30,$00,$C6
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"

	ALIGN	256
; graphics data for 128 characters - line 5
	dc.b	$00,$BD,$C3,$7C,$7C,$FE,$FE,$3C
	dc.b	$C3,$42,$BD,$CC,$3C,$30,$63,$E7
	dc.b	$F8,$3E,$18,$66,$1B,$6C,$7E,$7E
	dc.b	$18,$7E,$0C,$60,$C0,$66,$FF,$3C
	dc.b	$00,$30,$00,$FE,$0C,$30,$DC,$00
	dc.b	$60,$18,$3C,$30,$00,$00,$00,$60
	dc.b	$F6,$30,$60,$0C,$FE,$0C,$CC,$30
	dc.b	$CC,$0C,$00,$00,$60,$00,$18,$30
	dc.b	$DE,$FC,$66,$C0,$66,$68,$68,$CE
	dc.b	$CC,$30,$CC,$6C,$62,$D6,$CE,$C6
	dc.b	$60,$DC,$6C,$1C,$30,$CC,$CC,$FE
	dc.b	$38,$30,$32,$60,$0C,$18,$00,$00
	dc.b	$00,$7C,$66,$C0,$CC,$FC,$60,$CC
	dc.b	$66,$30,$0C,$78,$30,$FE,$CC,$CC
	dc.b	$66,$CC,$66,$78,$30,$CC,$CC,$FE
	dc.b	$38,$CC,$30,$30,$18,$30,$00,$C6
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"
 
	ALIGN	256
; graphics data for 128 characters - line 4
	dc.b	$ff,$81,$FF,$FE,$FE,$FE,$7C,$3C
	dc.b	$C3,$42,$BD,$7D,$66,$30,$63,$E7
	dc.b	$FE,$FE,$18,$66,$7B,$6C,$00,$18
	dc.b	$18,$18,$FE,$FE,$C0,$FF,$7E,$7E
	dc.b	$00,$30,$00,$6C,$78,$18,$76,$00
	dc.b	$60,$18,$FF,$FC,$00,$FC,$00,$30
	dc.b	$DE,$30,$38,$38,$CC,$0C,$F8,$18
	dc.b	$78,$7C,$00,$00,$C0,$00,$0C,$18
	dc.b	$DE,$CC,$7C,$C0,$66,$78,$78,$C0
	dc.b	$FC,$30,$0C,$78,$60,$FE,$DE,$C6
	dc.b	$7C,$CC,$7C,$70,$30,$CC,$CC,$D6
	dc.b	$38,$78,$18,$60,$18,$18,$C6,$00
	dc.b	$00,$0C,$7C,$CC,$7C,$CC,$F0,$CC
	dc.b	$76,$30,$0C,$6C,$30,$FE,$CC,$CC
	dc.b	$66,$CC,$76,$C0,$30,$CC,$CC,$D6
	dc.b	$5C,$CC,$98,$E0,$00,$1C,$00,$6C
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"
 
	ALIGN	256
; graphics data for 128 characters - line 3
	dc.b	$ff,$A5,$DB,$FE,$7C,$38,$38,$18
	dc.b	$E7,$66,$99,$0F,$66,$3F,$7F,$3C
	dc.b	$F8,$3E,$7E,$66,$DB,$38,$00,$7E
	dc.b	$7E,$18,$0C,$60,$C0,$66,$3C,$FF
	dc.b	$00,$78,$6C,$FE,$C0,$CC,$38,$C0
	dc.b	$60,$18,$3C,$30,$00,$00,$00,$18
	dc.b	$CE,$30,$0C,$0C,$6C,$F8,$C0,$0C
	dc.b	$CC,$CC,$30,$30,$60,$FC,$18,$0C
	dc.b	$DE,$CC,$66,$C0,$66,$68,$68,$C0
	dc.b	$CC,$30,$0C,$6C,$60,$FE,$F6,$C6
	dc.b	$66,$CC,$66,$E0,$30,$CC,$CC,$C6
	dc.b	$6C,$CC,$8C,$60,$30,$18,$6C,$00
	dc.b	$18,$78,$60,$78,$0C,$78,$60,$76
	dc.b	$6C,$70,$0C,$66,$30,$CC,$F8,$78
	dc.b	$DC,$76,$DC,$7C,$7C,$CC,$CC,$C6
	dc.b	$C6,$CC,$FC,$30,$18,$30,$00,$38
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"

	ALIGN	256
; graphics data for 128 characters - line 2
	dc.b	$00,$81,$FF,$FE,$38,$7C,$10,$00
	dc.b	$FF,$3C,$C3,$07,$66,$33,$63,$5A
	dc.b	$E0,$0E,$3C,$66,$DB,$63,$00,$3C
	dc.b	$3C,$18,$18,$30,$00,$24,$18,$FF
	dc.b	$00,$78,$6C,$6C,$7C,$C6,$6C,$60
	dc.b	$30,$30,$66,$30,$00,$00,$00,$0C
	dc.b	$C6,$70,$CC,$CC,$3C,$C0,$60,$CC
	dc.b	$CC,$CC,$30,$30,$30,$00,$30,$CC
	dc.b	$C6,$78,$66,$66,$6C,$62,$62,$66
	dc.b	$CC,$30,$0C,$66,$60,$EE,$E6,$6C
	dc.b	$66,$CC,$66,$CC,$B4,$CC,$CC,$C6
	dc.b	$C6,$CC,$C6,$60,$60,$18,$38,$00
	dc.b	$30,$00,$60,$00,$0C,$00,$6C,$00
	dc.b	$60,$00,$00,$60,$30,$00,$00,$00
	dc.b	$00,$00,$00,$00,$30,$00,$00,$00
	dc.b	$00,$00,$00,$30,$18,$30,$DC,$10
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"

	ALIGN	256
; graphics data for 128 characters - line 1
	dc.b	$00,$7E,$7E,$6C,$10,$38,$10,$00
	dc.b	$FF,$00,$FF,$0F,$3C,$3F,$7F,$99
	dc.b	$80,$02,$18,$66,$7F,$3E,$00,$18
	dc.b	$18,$18,$00,$00,$00,$00,$00,$00
	dc.b	$00,$30,$6C,$6C,$30,$00,$38,$60
	dc.b	$18,$60,$00,$00,$00,$00,$00,$06
	dc.b	$7C,$30,$78,$78,$1C,$FC,$38,$FC
	dc.b	$78,$78,$00,$00,$18,$00,$60,$78
	dc.b	$7C,$30,$FC,$3C,$F8,$FE,$FE,$3C
	dc.b	$CC,$78,$1E,$E6,$F0,$C6,$C6,$38
	dc.b	$FC,$78,$FC,$78,$FC,$CC,$CC,$C6
	dc.b	$C6,$CC,$FE,$78,$C0,$78,$10,$00
	dc.b	$30,$00,$E0,$00,$1C,$00,$38,$00
	dc.b	$E0,$30,$0C,$E0,$70,$00,$00,$00
	dc.b	$00,$00,$00,$00,$10,$00,$00,$00
	dc.b	$00,$00,$00,$1C,$18,$E0,$76,$00
	dc.b	$FF ; let's have byte $80 be equivalent to ASCII 219, "full block"

;################################################################
; Other data
	ORG	$8781
;################################################################

	dc.b	$FF ; safety in case we have too many bytes in the section above

;################################################################
; Main execution code goes here
	ORG	$C000
;################################################################

	;==============================================================
	; This routine waits for the vertical blanking period to start
	; It is called from many places, so this is a good a place as
	; any to put it
	;==============================================================
WaitVBLANK:
WaitVBoff:
	BIT	MSTAT
	BMI	WaitVBoff
WaitVBon:
	BIT	MSTAT
	BPL	WaitVBon
	RTS
	;===================

;============================================================
; Where the program begins after the encryption check passes
;============================================================
START:
	;===================================
	; Important things to do on startup
	;===================================
	SEI			; disable interrupts
	CLD
	LDA	#$07		; lock the 7800 into MARIA mode
	STA	INPTCTRL
	LDA	#$7F		; turn off DMA, since no output has
	STA	CTRL		; been set up yet
	LDX	#$FF		; set stack pointer to $01ff
	TXS

	;=======================================
	; Global Startup inits (good practices)
	;=======================================
	LDA	#$00
	STA	BACKGRND
	STA	INPTCTRL	; not really nessessary but the
	STA	OFFSET		; manual suggests it anyway
	LDA	#$80		; the font data is located at $8000
	STA	CHBASE
	;=======================================

	;=========================
	; Sound clearing routines
	;=========================
	LDA	#0
	STA	AUDV0
	STA	AUDV1
	;=========================

	;=============================
	; RAM cleanup (lots of Loops)
	;=============================
	LDA	#0
	LDX	#$40
RamCleanupLoop1
	; $40-$FF / $140-$1FF
	STA	$0,X
	STA	$100,X
	INX
	BNE	RamCleanupLoop1

	LDX	#$3F
RamCleanupLoop2
	; $2000-$203F
	STA	$2000,X
	DEX
	BPL	RamCleanupLoop2

	LDX	#$0
RamCleanupLoop3
	; $1800-$1FFF
	STA	$1800,X
	STA	$1900,X
	STA	$1A00,X
	STA	$1B00,X
	STA	$1C00,X
	STA	$1D00,X
	STA	$1E00,X
	STA	$1F00,X
	; $2200-$27FF
	STA	$2200,X
	STA	$2300,X
	STA	$2400,X
	STA	$2500,X
	STA	$2600,X
	STA	$2700,X

	DEX
	BNE	RamCleanupLoop3
	;=============================

	;=====================================
	; Joystick button initialization code
	;=====================================
	LDA	#$14
	STA	SWBCNT
	LDA	#0
	STA	SWCHB
	;=====================================

	;================================
	; Program-specific Startup inits
	;================================
	LDA	#$00		; black background, for starters
	STA	BACKGRND
	STA	HighlightIndex
	LDA	#$80		; the font data is located at $8000
	STA	CHBASE
	;================================

	;===============================================
	; Let's make the default for all Palletes white
	;===============================================
	LDA	#$0F	; white for both NTSC and PAL
	STA	P0C1
	STA	P0C2
	STA	P0C3
	STA	P1C1
	STA	P1C2
	STA	P1C3
	STA	P2C1
	STA	P2C2
	STA	P2C3
	STA	P3C1
	STA	P3C2
	STA	P3C3
	STA	P4C1
	STA	P4C2
	STA	P4C3
	STA	P5C1
	STA	P5C2
	STA	P5C3
	STA	P6C1
	STA	P6C2
	STA	P6C3
	STA	P7C1
	STA	P7C2
	STA	P7C3
	;===============================================

        ;===============================================
        ; Set up Character maps, DLL's, DL's.
	; This is all safe; the CTRL bit has the display
	; bit off
        ;===============================================
	JSR	Copy_CHMAPs
	JSR	Copy_DLs
	JSR	Copy_DLLs
	JSR	SetupPALorNTSCPalettes
        ;===============================================

	;================================
	; SaveKey / AtariVox integration
	;================================
	; Detect if there's a SaveKey there
	JSR	DetectSaveKey
	; if the above subroutine passes, it returns $00, if it fails, it returns $FF
	BNE	NoSaveKeyInstalled
SaveKeyInstalled
	LDA	#$0 ; Now that we're done with SaveKey functions for now, let's turn both Joystick ports back to INPUTS
	STA	SWACNT
	; If there's a savekey installed, delete the "not" DL
	LDA	$00
	;STA	$1900+DL_Not-$E100
	;STA	$1901+DL_Not-$E100
	;LDA	#$C3 ; green
	;STA	BACKGRND
	JMP	AfterSaveKeyRead
NoSaveKeyInstalled
	;LDA	#$43 ; red
	;STA	BACKGRND
AfterSaveKeyRead
	JSR	WaitVBLANK

	; Default background Color
	;LDA	#$0F
	LDA	#$00
	STA	BACKGRND

	;=========================
	; Fall into the Main Loop
	;=========================
MainLoop:
	JSR	WaitVBLANK	; avoid any process-corruption issues with DMA-interrupts
				; also, force calc's once per frame
	; Canary check, courtesy of Atariage user RevEng
	LDA	Canary
	BEQ	CanaryIsAlive
	LDA	#$45 ; RED
	STA	BACKGRND
	dc.b	$02 ; KIL opcode
CanaryIsAlive
	;===================================
	; Handle all housekeeping functions
	;===================================
	;JSR	CheckReset
	;JSR	CheckSelect
	JSR	CheckJoystick
	JSR	AfterJoystickProcessHighlighting
	; Now, turn on the screen
	LDA	#%01001011	; then enable normal color output,
	STA	CTRL		; turn on DMA, set one byte wide
				; characters, make the border black
				; enable transparent output, set screen
				; mode to 320A or 320C
				; (D1 and D0 = %11, so 320A or 320C)
				; (DL mode byte D7 = 0, so 320A)
	JMP	MainLoop
	;===================

	;=======================================================
	; Subroutine - Copy the text data into the RAM at $1800
	;=======================================================
Copy_CHMAPs:
	LDX	#$00
Copy_CHMAPs_Loop:
	LDA	$D000,X
	STA	$1800,X
	INX
	BNE	Copy_CHMAPs_Loop
	RTS

	;=====================================================
	; Subroutine - Copy the and DL's into the RAM at $1900
	;=====================================================
Copy_DLs:
	LDX	#$00
Copy_DLs_Loop:
	LDA	$E000,X
	STA	$1900,X
	LDA	$E100,X
	STA	$1A00,X
	INX
	BNE	Copy_DLs_Loop
	RTS

	;==================================
	; Subroutine - Copy DLL's into RAM
	;==================================
Copy_DLLs
	;===================
	; Set up empty DL
	;===================
	LDA	#0
	STA	DL_Empty
	STA	DL_Empty+1
	;===================
	; Copy all 256dc.bs of DLL data from code into RAM at DLLRam.
	;===================
	LDX	#0
Copy_DLL_Loop:
	LDA	DLL_PAL,x
	STA	DLLRam,x
	INX
	BNE	Copy_DLL_Loop
	;===================
	RTS
	;===================

; Differences between the NTSC and PAL 7800 consoles' output:
;     NTSC     PAL
;      15       15     automatic VBLANK lines
;      25       33     output lines, that can't be seen on many TVs
;     192      228     actual display lines
;      26       32     output lines, that can't be seen on many TVs
;       4        4     automatic VBLANK lines
;
;     262      312     total number of lines per frame
;     243      293     total number of DMA output lines per frame
;      60       50     frames per second
;
; So to make a NTSC game run properly on a PAL console, you can simply
; set up 25 extra blank lines before and after the normal NTSC display.
; Also you would have to adjust anything that is timed by counting the
; number of frames, since PAL consoles do less frames per second than
; NTSC consoles.

;======================
SetupPALorNTSCPalettes
;======================

	;===================
	; To make a NTSC game run on a PAL console, put 25 blank lines before & after NTSC display.
	;===================
	JSR	WaitVBLANK
WaitVBoverForPALNTSC:
	BIT	MSTAT
	BMI	WaitVBoverForPALNTSC	; wait for the VBLANK to end
	;===================

	;===================
	; PAL detector
	;===================
	LDX	#$00
	TXA	; zero accumulator for BIT testing of MSTAT
CountLines:
	BIT	MSTAT
	BMI	CompareCounter
	STA	WSYNC
	STA	WSYNC
	DEX
	BNE	CountLines
CompareCounter:
	CPX	#$78
	LDA	#>DLLRam	; DPPH is common for PAL/NTSC
	STA	DPPH
	BCS	NoPalSetup
;PALSetup:
	LDA	#1 ; PAL
	STA	PalOrNtsc
	LDA	#<DLL_PAL	; prepare PAL setup here
	STA	DPPL		; setup the pointers for the output
	JSR	WaitVBLANK	; wait until no DMA would happen

	;============================
	; PAL Palette initialization
	;============================
	; We could theoretically use the NTSC2PALColor macro, but there are
	; so few colors in this Utility that we can just hardcode
	;============================
	LDA	#$A7 ; blue text
	STA	P0C2
	LDA	#$E3 ; green text
	STA	P1C2
	LDA	#$53 ; red text
	STA	P2C2
	LDA	#$3A ; yellow text
	STA	P3C2
	;============================

	;===================
NoPalSetup:
	LDA	#0 ; NTSC
	STA	PalOrNtsc
	LDA	#<DLL_NTSC	; prepare NTSC setup here
	STA	DPPL	; setup the pointers for the output
	JSR	WaitVBLANK	; wait until no DMA would happen
	;===================

;=============================
; NTSC Palette initialization
;=============================
	LDA	#$87 ; blue text
	STA	P0C2
	LDA	#$C3 ; green text
	STA	P1C2
	LDA	#$33 ; red text
	STA	P2C2
	LDA	#$1A ; yellow text
	STA	P3C2
	LDA	#$03
	STA	P4C2
	RTS
	;===================

	;============================
	; Function - Joystick checks
	;============================
CheckJoystick
	LDA	SWCHA
	BMI	AfterRight
	LDA	PrevJoystickState
	BPL	AfterRight

	;===================
	; Handle RIGHT here
	;===================
	INC	HighlightIndex
	LDA	HighlightIndex
	CMP	#14
	BNE	AfterRight
	LDA	#0
	STA	HighlightIndex

AfterRight
	LDA	SWCHA
	ROL
	BMI	AfterLeft
	LDA	PrevJoystickState
	ROL
	BPL	AfterLeft

	;==================
	; Handle LEFT here
	;==================
	DEC	HighlightIndex
	LDA	HighlightIndex
	CMP	#$FF
	BNE	AfterLeft
	LDA	#13
	STA	HighlightIndex

AfterLeft
	LDA	SWCHA
	ROL
	ROL
	BMI	AfterDown
	LDA	PrevJoystickState
	ROL
	ROL
	BPL	AfterDown

	;==================
	; Handle DOWN here
	;==================
	INC	HighlightIndex
	LDA	HighlightIndex
	CMP	#14
	BNE	AfterDown
	LDA	#0
	STA	HighlightIndex

AfterDown
	LDA	SWCHA
	ROL
	ROL
	ROL
	BMI	AfterUp
	LDA	PrevJoystickState
	ROL
	ROL
	ROL
	BPL	AfterUp

	;================
	; Handle UP here
	;================
	DEC	HighlightIndex
	LDA	HighlightIndex
	CMP	#$FF
	BNE	AfterUp
	LDA	#13
	STA	HighlightIndex

AfterUp
	LDA	SWCHA
	AND	#$F0
	STA	PrevJoystickState
	RTS

AfterJoystickProcessHighlighting
	;=======================
	; 1 - savekey detection
	; 2a 2b - savekey address
	; 3a 3b 3c 3d 3e - savekey data 1
	; 4a 4b 4c 4d 4e - savekey data 2
	; 5 - send data
	; 6 - read data
	; 7 - bytes received
	;=======================
	; First things first-- make every editable text GREY
	LDX	#13
LoopToCleanAllText
	LDA	ColorPtrTableLSB,X
	STA	IndirPtr
	LDA	ColorPtrTableMSB,X
	STA	IndirPtr+1
	LDY	#0
	LDA	(IndirPtr),Y ; load the current color
	AND	#%00011111
	ORA	#GREY
	STA	(IndirPtr),Y
	DEX
	BPL	LoopToCleanAllText

	; Next, we want the "Received byte" to be Green to make it stand out
	LDA	Color7
	AND	#%00011111
	ORA	#GREEN
	STA	Color7

	; Next, let's highlight the currently selected word
	LDX	HighlightIndex
	LDA	ColorPtrTableLSB,X
	STA	IndirPtr
	LDA	ColorPtrTableMSB,X
	STA	IndirPtr+1
	LDY	#0
	LDA	(IndirPtr),Y ; load the current color
	AND	#%00011111
	ORA	#WHITE
	STA	(IndirPtr),Y
	RTS

ColorPtrTableLSB
	dc.b	#<Color2a
	dc.b	#<Color2b
	dc.b	#<Color3a
	dc.b	#<Color3b
	dc.b	#<Color3c
	dc.b	#<Color3d
	dc.b	#<Color3e
	dc.b	#<Color4a
	dc.b	#<Color4b
	dc.b	#<Color4c
	dc.b	#<Color4d
	dc.b	#<Color4e
	dc.b	#<Color5
	dc.b	#<Color6

ColorPtrTableMSB
	dc.b	#>Color2a
	dc.b	#>Color2b
	dc.b	#>Color3a
	dc.b	#>Color3b
	dc.b	#>Color3c
	dc.b	#>Color3d
	dc.b	#>Color3e
	dc.b	#>Color4a
	dc.b	#>Color4b
	dc.b	#>Color4c
	dc.b	#>Color4d
	dc.b	#>Color4e
	dc.b	#>Color5
	dc.b	#>Color6

;############################################################
; CHMAP Pointers to the graphic data are here - $1800 in RAM
	ORG	$D000
;############################################################

CHMAP_Space
   STR_LEN " ", CHMAP_Space

CHMAP_Hyphen
   STR_LEN "-", CHMAP_Hyphen

CHMAP_Slash
   STR_LEN "/", CHMAP_Slash

STR_LEN_CHMAP_Highlight equ 12 ; the longest item to highlight is 12 characters
CHMAP_Highlight
	dc.b	$80,$80,$80,$80
	dc.b	$80,$80,$80,$80
	dc.b	$80,$80,$80,$80

;===============================================================================
; Any "dynamic" CHMAP goes at the top of the list, so they can be copied to RAM
;===============================================================================

CHMAP_Line8_Dynamic_Passed
   STR_LEN "PASSED", CHMAP_Line8_Dynamic_Passed
CHMAP_Line8_Dynamic_Failed
   STR_LEN "FAILED", CHMAP_Line8_Dynamic_Failed

;====================================
;The rest of the CHMAPs are hardcoded
;====================================

CHMAP_Line1_and_6_Half
   STR_LEN "====================", CHMAP_Line1_and_6_Half

CHMAP_Line2_Part1
   STR_LEN "Assembly SaveKey Tester -", CHMAP_Line2_Part1

CHMAP_Line2_Part2
   STR_LEN "Version 1.0", CHMAP_Line2_Part2

CHMAP_Line3_Part1
   STR_LEN "by John K. Harvey (Atari Age", CHMAP_Line3_Part1
CHMAP_Line3_Part2
   STR_LEN "@Propane13)", CHMAP_Line3_Part2

; Skip line 4

CHMAP_Line5_Part1
   STR_LEN "Code: github.com/johnkharvey/", CHMAP_Line5_Part1
CHMAP_Line5_Part2
   STR_LEN "savekey7800", CHMAP_Line5_Part2

; Line 6 uses CHMAP_Line1_and_6_Half

; Skip Line 7

CHMAP_Line8_Hardcode
   STR_LEN "SaveKey detection:", CHMAP_Line8_Hardcode

; Skip line 9

CHMAP_Line10_Part1
   STR_LEN "Move the joystick to select an", CHMAP_Line10_Part1
CHMAP_Line10_Part2
   STR_LEN "option", CHMAP_Line10_Part2

CHMAP_Line11
   STR_LEN "and then press FIRE:", CHMAP_Line11

; Skip line 12

CHMAP_Line13
   STR_LEN "ADDRESS:", CHMAP_Line13

CHMAP_Line14_String1
   STR_LEN "$3040", CHMAP_Line14_String1

CHMAP_Line14_String2
   STR_LEN "<->", CHMAP_Line14_String2

CHMAP_Line14_String3
   STR_LEN "$3048", CHMAP_Line14_String3

; Skip line 15

CHMAP_Line16
   STR_LEN "SEND DATA:", CHMAP_Line16

CHMAP_Line17_String1
   STR_LEN "Chicken", CHMAP_Line17_String1
CHMAP_Line17_String2
   STR_LEN "Weasel", CHMAP_Line17_String2
CHMAP_Line17_String3
   STR_LEN "Stork", CHMAP_Line17_String3
CHMAP_Line17_String4
   STR_LEN "Pig", CHMAP_Line17_String4
CHMAP_Line17_String5
   STR_LEN "Ox", CHMAP_Line17_String5

CHMAP_Line18_String1
   STR_LEN "0000000", CHMAP_Line18_String1
CHMAP_Line18_String2
   STR_LEN "<blank>", CHMAP_Line18_String2
CHMAP_Line18_String3
   STR_LEN "Test", CHMAP_Line18_String3
CHMAP_Line18_String4
   STR_LEN "333", CHMAP_Line18_String4
CHMAP_Line18_String5
   STR_LEN "22", CHMAP_Line18_String5

; Skip line 19

CHMAP_Line20
   STR_LEN "ACTION:", CHMAP_Line20

CHMAP_Line21
   STR_LEN "Send Data", CHMAP_Line21

CHMAP_Line22
   STR_LEN "Read 8 bytes", CHMAP_Line22

; Skip line 23

CHMAP_Line24
   STR_LEN "BYTES READ:", CHMAP_Line24
CHMAP_Bytes_Read
   STR_LEN "<TBD>", CHMAP_Bytes_Read

;###################################
; The DL starts here - $1900 in RAM
	ORG	$E000
;###################################
Code_DL_Start

DL_Empty
	dc.b	$00,$00

DL_Space
	dc.b	<CHMAP_Space
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_RAM_Start
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Space
	dc.b	0 ; HPos (0-159)
	dc.b	$00,$00

;================
DL_Line1_and_6
	dc.b	<CHMAP_Line1_and_6_Half
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line1_and_6_Half
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line1_and_6_Half
	dc.b	0 ; HPos (0-159)

	dc.b	<CHMAP_Line1_and_6_Half
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line1_and_6_Half
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line1_and_6_Half
	dc.b	80 ; HPos (0-159)

	dc.b	$00,$00

DL_Line2
	dc.b	<CHMAP_Line2_Part1
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line2_Part1
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line2_Part1
	dc.b	0 ; HPos (0-159)

	dc.b	<CHMAP_Line2_Part2
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line2_Part2
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line2_Part2
	dc.b	104 ; HPos (0-159)

	dc.b	$00,$00

DL_Line3
	dc.b	<CHMAP_Line3_Part1
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line3_Part1
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line3_Part1
	dc.b	0 ; HPos (0-159)

	dc.b	<CHMAP_Line3_Part2
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line3_Part2
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line3_Part2
	dc.b	116 ; HPos (0-159)

	dc.b	$00,$00

DL_Line5
	dc.b	<CHMAP_Line5_Part1
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line5_Part1
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line5_Part1
	dc.b	0 ; HPos (0-159)

	dc.b	<CHMAP_Line5_Part2
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line5_Part2
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line5_Part2
	dc.b	116 ; HPos (0-159)

	dc.b	$00,$00

DL_Line8
	dc.b	<CHMAP_Line8_Hardcode
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line8_Hardcode
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line8_Hardcode
	dc.b	0 ; HPos (0-159)

	dc.b	<CHMAP_Line8_Dynamic_Passed
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_RAM_Start
CodeColor1
Color1	equ	CodeColor1-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE1+$20-STR_LEN_CHMAP_Line8_Dynamic_Passed ; PALETTE 1 = green
	dc.b	76 ; HPos (0-159)

	dc.b	$00,$00

DL_Line10
	dc.b	<CHMAP_Line10_Part1
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line10_Part1
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line10_Part1
	dc.b	0 ; HPos (0-159)

	dc.b	<CHMAP_Line10_Part2
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line10_Part2
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line10_Part2
	dc.b	124 ; HPos (0-159)

	dc.b	$00,$00

DL_Line11
	dc.b	<CHMAP_Line11
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line11
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line11
	dc.b	0 ; HPos (0-159)

	dc.b	$00,$00

DL_Line13
	dc.b	<CHMAP_Line13
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line13
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line13
	dc.b	0 ; HPos (0-159)

	dc.b	$00,$00

DL_Line14
	dc.b	<CHMAP_Hyphen
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Hyphen
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Hyphen
	dc.b	4 ; HPos (0-159)

	dc.b	<CHMAP_Line14_String1
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line14_String1
CodeColor2a
Color2a	equ	CodeColor2a-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line14_String1
	dc.b	12 ; HPos (0-159)

	dc.b	<CHMAP_Line14_String2
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line14_String2
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line14_String2
	dc.b	36 ; HPos (0-159)

	dc.b	<CHMAP_Line14_String3
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line14_String3
CodeColor2b
Color2b	equ	CodeColor2b-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line14_String3
	dc.b	52 ; HPos (0-159)

	dc.b	$00,$00

DL_Line16
	dc.b	<CHMAP_Line16
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line16
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line16
	dc.b	0 ; HPos (0-159)

	dc.b	$00,$00

DL_Line17
	dc.b	<CHMAP_Hyphen
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Hyphen
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Hyphen
	dc.b	4 ; HPos (0-159)

	dc.b	<CHMAP_Line17_String1
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line17_String1
CodeColor3a
Color3a	equ	CodeColor3a-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line17_String1
	dc.b	12 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	44 ; HPos (0-159)

	dc.b	<CHMAP_Line17_String2
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line17_String2
CodeColor3b
Color3b	equ	CodeColor3b-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line17_String2
	dc.b	52 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	80 ; HPos (0-159)

	dc.b	<CHMAP_Line17_String3
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line17_String3
CodeColor3c
Color3c	equ	CodeColor3c-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line17_String3
	dc.b	88 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	112 ; HPos (0-159)

	dc.b	<CHMAP_Line17_String4
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line17_String4
CodeColor3d
Color3d	equ	CodeColor3d-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line17_String4
	dc.b	120 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	136 ; HPos (0-159)

	dc.b	<CHMAP_Line17_String5
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line17_String5
CodeColor3e
Color3e	equ	CodeColor3e-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line17_String5
	dc.b	144 ; HPos (0-159)

	dc.b	$00,$00

DL_Line18
	dc.b	<CHMAP_Hyphen
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Hyphen
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Hyphen
	dc.b	4 ; HPos (0-159)

	dc.b	<CHMAP_Line18_String1
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line18_String1
CodeColor4a
Color4a	equ	CodeColor4a-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line18_String1
	dc.b	12 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	44 ; HPos (0-159)

	dc.b	<CHMAP_Line18_String2
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line18_String2
CodeColor4b
Color4b	equ	CodeColor4b-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line18_String2
	dc.b	52 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	84 ; HPos (0-159)

	dc.b	<CHMAP_Line18_String3
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line18_String3
CodeColor4c
Color4c	equ	CodeColor4c-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line18_String3
	dc.b	92 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	112 ; HPos (0-159)

	dc.b	<CHMAP_Line18_String4
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line18_String4
CodeColor4d
Color4d	equ	CodeColor4d-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line18_String4
	dc.b	120 ; HPos (0-159)

	dc.b	<CHMAP_Slash
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Slash
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Slash
	dc.b	136 ; HPos (0-159)

	dc.b	<CHMAP_Line18_String5
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line18_String5
CodeColor4e
Color4e	equ	CodeColor4e-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line18_String5
	dc.b	144 ; HPos (0-159)

	dc.b	$00,$00

DL_Line20
	dc.b	<CHMAP_Line20
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line20
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line20
	dc.b	0 ; HPos (0-159)

	dc.b	$00,$00

DL_Line21
	dc.b	<CHMAP_Hyphen
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Hyphen
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Hyphen
	dc.b	4 ; HPos (0-159)

	dc.b	<CHMAP_Line21
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line21
CodeColor5
Color5	equ	CodeColor5-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line21
	dc.b	12 ; HPos (0-159)

	dc.b	$00,$00

DL_Line22
	dc.b	<CHMAP_Hyphen
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Hyphen
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Hyphen
	dc.b	4 ; HPos (0-159)

	dc.b	<CHMAP_Line22
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line22
CodeColor6
Color6	equ	CodeColor6-Code_DL_Start+DL_RAM_Start
	dc.b	PALETTE3+$20-STR_LEN_CHMAP_Line22
	dc.b	12 ; HPos (0-159)

	dc.b	$00,$00

DL_Line24
	dc.b	<CHMAP_Line24
	dc.b	$60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
	dc.b	>CHMAP_Line24
	dc.b	PALETTE0+$20-STR_LEN_CHMAP_Line24
	dc.b	0 ; HPos (0-159)

        dc.b    <CHMAP_Bytes_Read
        dc.b    $60 ; D7 = Write Mode bit: 0=160x2 or 320x1, 1=160x4 or 320x2. D6=1. D5 = Indirect mode bit: 0=direct, 1=indirect mode.
        dc.b    >CHMAP_Bytes_Read
CodeColor7
Color7	equ	CodeColor7-Code_DL_Start+DL_RAM_Start
        dc.b    PALETTE3+$20-STR_LEN_CHMAP_Bytes_Read
        dc.b    48 ; HPos (0-159)

	dc.b	$00,$00

;#####################################
; The DLL starts here - $2400 in RAM
	ORG	$F000
;#####################################

DLL_PAL:
	dc.b	$00, >DL_Empty, <DL_Empty	; 25 extra blank lines for PAL
	dc.b	$07, >DL_Empty, <DL_Empty
	dc.b	$07, >DL_Empty, <DL_Empty
	dc.b	$07, >DL_Empty, <DL_Empty

	; $F20C
DLL_NTSC:
	dc.b	$00, >DL_Empty, <DL_Empty	; 25 blank lines that can't be seen
	dc.b	$07, >DL_Empty, <DL_Empty	; on most NTSC TVs anyway
	dc.b	$07, >DL_Empty, <DL_Empty
	dc.b	$07, >DL_Empty, <DL_Empty	; if the highest bit of the firstdc.b
						; in a DLL entry is set, an interrupt
						; will occur after the DMA for the
						; last line in this block of scanlines
						; has ended

Code_DLL_On_Screen:
DLL_On_Screen	EQU	Code_DLL_On_Screen - DLL_PAL + DLLRam

	dc.b	$07, >(DL_Line1_and_6-Code_DL_Start+DL_RAM_Start), <DL_Line1_and_6 ; [8] blank
	dc.b	$07, >(DL_Line2-Code_DL_Start+DL_RAM_Start), <DL_Line2 ; [16] blank
	dc.b	$07, >(DL_Line3-Code_DL_Start+DL_RAM_Start), <DL_Line3   ; [24] blank
	dc.b	$07, >(DL_Space-Code_DL_Start+DL_RAM_Start), <DL_Space   ; [32] blank
	dc.b	$07, >(DL_Line5-Code_DL_Start+DL_RAM_Start), <DL_Line5	; [40] blank
	dc.b	$07, >(DL_Line1_and_6-Code_DL_Start+DL_RAM_Start), <DL_Line1_and_6 ; [48] blank
	dc.b	$07, >(DL_Space-Code_DL_Start+DL_RAM_Start), <DL_Space   ; [56] blank
	dc.b	$07, >(DL_Line8-Code_DL_Start+DL_RAM_Start), <DL_Line8   ; [64] blank
	dc.b	$07, >(DL_Space-Code_DL_Start+DL_RAM_Start), <DL_Space   ; [72] blank
	dc.b	$07, >(DL_Line10-Code_DL_Start+DL_RAM_Start), <DL_Line10  ; [80] blank
	dc.b	$07, >(DL_Line11-Code_DL_Start+DL_RAM_Start), <DL_Line11  ; [88] blank
	dc.b	$07, >(DL_Space-Code_DL_Start+DL_RAM_Start), <DL_Space   ; [96] blank
	dc.b	$07, >(DL_Line13-Code_DL_Start+DL_RAM_Start), <DL_Line13  ; [104] blank
	dc.b	$07, >(DL_Line14-Code_DL_Start+DL_RAM_Start), <DL_Line14  ; [112] blank
	dc.b	$07, >(DL_Space-Code_DL_Start+DL_RAM_Start), <DL_Space	; [120] blank
	dc.b	$07, >(DL_Line16-Code_DL_Start+DL_RAM_Start), <DL_Line16  ; [128] blank
	dc.b	$07, >(DL_Line17-Code_DL_Start+DL_RAM_Start), <DL_Line17  ; [136] blank
	dc.b	$07, >(DL_Line18-Code_DL_Start+DL_RAM_Start), <DL_Line18  ; [144] blank
	dc.b	$07, >(DL_Space-Code_DL_Start+DL_RAM_Start), <DL_Space	; [152] blank
	dc.b	$07, >(DL_Line20-Code_DL_Start+DL_RAM_Start), <DL_Line20  ; [160] blank
	dc.b	$07, >(DL_Line21-Code_DL_Start+DL_RAM_Start), <DL_Line21  ; [168] blank
	dc.b	$07, >(DL_Line22-Code_DL_Start+DL_RAM_Start), <DL_Line22  ; [176] blank
	dc.b	$07, >(DL_Space-Code_DL_Start+DL_RAM_Start), <DL_Space	; [184] blank
	dc.b	$07, >(DL_Line24-Code_DL_Start+DL_RAM_Start), <DL_Line24  ; [192] blank

	dc.b	$07, >DL_Empty, <DL_Empty	; The DMA keeps doing another 26 lines
	dc.b	$07, >DL_Empty, <DL_Empty	; which can't be seen on most NTSC
	dc.b	$07, >DL_Empty, <DL_Empty	; TVs, so they are set to be blank
	dc.b	$01, >DL_Empty, <DL_Empty

	dc.b	$00, >DL_Empty, <DL_Empty	; 25 extra blank lines for PAL
	dc.b	$07, >DL_Empty, <DL_Empty
	dc.b	$07, >DL_Empty, <DL_Empty
	dc.b	$07, >DL_Empty, <DL_Empty


;#####################
; DLI Interrupt code
	ORG	$FF00
;#####################

; At this address the execution will continue after an interrupt has
; occured. Here we change the horizontal position for the two lines of
; text to shift them continuously over the screen. In this example code
; the interrupt occurs in the scanline before the PAL/NTSC text starts
; displaying in every frame.
INTERRUPT:

	RTI
	; Canary code, courtesy of Atariage user RevEng
	LDA	#$1A ; YELLOW
	STA	BACKGRND
	dc.b	$02 ; KIL opcode. Stop the 6502.

;#####################
; BRK interrupt code
	ORG	$FF7F
;#####################

BRKroutine:
	RTI

;#####################
; Encryption space
	ORG	$FF80
;#####################

	; reserved for encryption
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00

	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00

	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00

	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00

	ORG	$FFF8
	dc.b	$FF	; Region verification
	dc.b	$47	; Starts at $4000

	.word	INTERRUPT
	.word	START
	.word	BRKroutine

