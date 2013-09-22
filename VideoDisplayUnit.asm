;
; ********************************************
; * Video Display Unit        
; * (C)2010 by Greg Lindsey
; ********************************************
;
; Included header file for target AVR type
;.NOLIST
;.INCLUDE "m644def.inc"
;.LIST
;
; ============================================
;   Hardware Information
; ============================================
;
; [Add all hardware information here]
;
; ============================================
;   Ports And Pins
; ============================================
;
; [Add names for hardware ports and pins here]
; Format: .EQU Controlportout = PORTA
; .EQU Controlportin = PINA
; .EQU LedOutputPin = PORTA2
.EQU port_data = PORTA
.EQU ddr_data = DDRA

.EQU port_toggles1 = PORTB
.EQU ddr_toggles1 = DDRB

.EQU pin_Nvblank = PB0
.EQU pin_Nmemaccess = PB1	; When LOW, memory can be accessed externally

.EQU port_addr = PORTC
.EQU ddr_addr = DDRC

.EQU port_toggles2 = PORTD
.EQU ddr_toggles2 = DDRD

.EQU pin_Nmemwrite = PD2
.EQU pin_Nmemread = PD3
.EQU pin_hsync = PD4
.EQU pin_vsync = PD5
.EQU pin_addrlatch = PD6	; Latches HIGH address byte
.EQU pin_Nlatchout = PD7	; When LOW, latch output is ENABLED

.EQU videoddr = DDRA
.EQU videoport = PORTA
.EQU red2 = PA0
.EQU red1 = PA1
.EQU red0 = PA2
.EQU green2 = PA3
.EQU green1 = PA4
.EQU green0 = PA5
.EQU blue2 = PA6
.EQU blue1 = PA7

; Video flags, set in rvidflags
.EQU flag_frameend = 0		; Set at start of VSYNC period, cleared at end

;
; ============================================
;   Defined Constants
; ============================================
;
; [Add all constants here that can be subject
; to change by the user]
; Format: .EQU const = $ABCD
;
; ============================================
;   Derived Constants
; ============================================
;
; [Add all constants here that are not subject
; to change or calculated from constants]
; Format: .EQU const = $ABCD
;
; ============================================
;   Register Definitions
; ============================================
;
; [Add all register names here, include info on
; all used registers without specific names]
; Format: .DEF rtmp = R16

.DEF rtmp = R16		; Temporary register
.DEF rtmp2 = R17	; Temporary register 2

; X register is used as memory address counter
; Y register is used as video line counter

.DEF rZero = R25  ; Standard "zero" register
.DEF rFF = R24    ; Standard "FF" register

; Persistent registers
.DEF rvidflags = R23 ; Video flags register

;
; ============================================
;   Macro Definitions
; ============================================
.MACRO ZERO
  eor @0, @0
.ENDMACRO

.MACRO LOAD_IMM
  ldi rtmp, @1
  mov @0, rtmp
.ENDMACRO

.MACRO MEMORY_READ_ENABLED
  ; Set data DDR to input and turn on memory read.
  out ddr_data, rZero
  cbi port_toggles2, pin_Nmemread
.ENDMACRO

.MACRO MEMORY_READ_DISABLED
  ; Set data DDR to output and disable memory read.
  out ddr_data, rFF
  sbi port_toggles2, pin_Nmemread
.ENDMACRO

.MACRO DO_MEMORY_WRITE
  cbi port_toggles2, pin_Nmemwrite
  nop
  sbi port_toggles2, pin_Nmemwrite
.ENDMACRO

.MACRO DO_ADDR_LATCH
  sbi port_toggles2, pin_addrlatch
  nop
  cbi port_toggles2, pin_addrlatch
.ENDMACRO

.MACRO DO_OUT_COL_ADDR
  out port_data, XL
  inc XL
.ENDMACRO

.MACRO DO_OUT_ROW_ADDR
  out port_data, XH
  inc XL
.ENDMACRO

.MACRO DO_OUT_PIXEL
  out port_addr, XL
  inc XL
.ENDMACRO

.MACRO DO_16_COLADDR_PIXELS
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
  DO_OUT_COL_ADDR
.ENDMACRO

.MACRO DO_16_ROWADDR_PIXELS
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
  DO_OUT_ROW_ADDR
.ENDMACRO

.MACRO DO_16_PIXELS
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
  DO_OUT_PIXEL
.ENDMACRO

.MACRO DO_240_COLADDR_PIXELS
  DO_16_COLADDR_PIXELS			; 000-015
  DO_16_COLADDR_PIXELS			; 016-031
  DO_16_COLADDR_PIXELS			; 032-047
  DO_16_COLADDR_PIXELS			; 048-063
  DO_16_COLADDR_PIXELS			; 064-079
  DO_16_COLADDR_PIXELS			; 080-095
  DO_16_COLADDR_PIXELS			; 096-111
  DO_16_COLADDR_PIXELS			; 112-127
  DO_16_COLADDR_PIXELS			; 128-143
  DO_16_COLADDR_PIXELS			; 144-159
  DO_16_COLADDR_PIXELS			; 160-175
  DO_16_COLADDR_PIXELS			; 176-191
  DO_16_COLADDR_PIXELS			; 192-207
  DO_16_COLADDR_PIXELS			; 208-223
  DO_16_COLADDR_PIXELS			; 224-239
.ENDMACRO

.MACRO DO_240_ROWADDR_PIXELS
  DO_16_ROWADDR_PIXELS			; 000-015
  DO_16_ROWADDR_PIXELS			; 016-031
  DO_16_ROWADDR_PIXELS			; 032-047
  DO_16_ROWADDR_PIXELS			; 048-063
  DO_16_ROWADDR_PIXELS			; 064-079
  DO_16_ROWADDR_PIXELS			; 080-095
  DO_16_ROWADDR_PIXELS			; 096-111
  DO_16_ROWADDR_PIXELS			; 112-127
  DO_16_ROWADDR_PIXELS			; 128-143
  DO_16_ROWADDR_PIXELS			; 144-159
  DO_16_ROWADDR_PIXELS			; 160-175
  DO_16_ROWADDR_PIXELS			; 176-191
  DO_16_ROWADDR_PIXELS			; 192-207
  DO_16_ROWADDR_PIXELS			; 208-223
  DO_16_ROWADDR_PIXELS			; 224-239
.ENDMACRO

.MACRO DO_240_PIXELS
  DO_16_PIXELS			; 000-015
  DO_16_PIXELS			; 016-031
  DO_16_PIXELS			; 032-047
  DO_16_PIXELS			; 048-063
  DO_16_PIXELS			; 064-079
  DO_16_PIXELS			; 080-095
  DO_16_PIXELS			; 096-111
  DO_16_PIXELS			; 112-127
  DO_16_PIXELS			; 128-143
  DO_16_PIXELS			; 144-159
  DO_16_PIXELS			; 160-175
  DO_16_PIXELS			; 176-191
  DO_16_PIXELS			; 192-207
  DO_16_PIXELS			; 208-223
  DO_16_PIXELS			; 224-239
.ENDMACRO

.MACRO START_DO_VIDEO_LINE
	; Disable external memory access.
	;sbi port_toggles1, pin_Nmemaccess

	; Push registers onto stack.
	push rtmp
	push XL
	push XH
	push YL
	push YH

	; Load current video line from memory.
	lds YH, Video_CurLine
	lds YL, Video_CurLine+1

	; Save I/O states for address and data ports.
	in rtmp, ddr_data
	push rtmp
	in rtmp, port_data
	push rtmp
	in rtmp, ddr_addr
	push rtmp
	in rtmp, port_addr
	push rtmp

	out port_data, rZero
.ENDMACRO

.MACRO END_DO_VIDEO_LINE
	; Load I/O states for address and data ports.
	pop rtmp
	out port_addr, rtmp
	pop rtmp
	out ddr_addr, rtmp
	pop rtmp
	out port_data, rtmp
	pop rtmp
	out ddr_data, rtmp

	; Store current video line into memory.	
	sts Video_CurLine, YH
	sts Video_CurLine+1, YL

	; Pop registers from stack.
	pop YH
	pop YL
	pop XH
	pop XL
	pop rtmp

	; Re-enable external memory access.
	;sbi port_toggles1, pin_Nmemaccess

	reti
.ENDMACRO

;
; ============================================
;   SRAM Definitions
; ============================================
;
.DSEG
.ORG $0100

Video_XOffset:	.BYTE 1
Video_YOffset:	.BYTE 1

Video_CurLine:  .BYTE 2

; Format: Label: .BYTE N ; reserve N Bytes from Label:
;
; ============================================
;   Interrupt Vectors
; ============================================
;
.CSEG
.ORG $0000
jmp Main         ; 01 Reset vector
jmp BadInt       ; 02 External Interrupt 0
jmp BadInt       ; 03 External Interrupt 1
jmp BadInt       ; 04 External Interrupt 2
jmp BadInt       ; 05 Pin Change Interrupt 0
jmp BadInt       ; 06 Pin Change Interrupt 1
jmp BadInt       ; 07 Pin Change Interrupt 2
jmp BadInt       ; 08 Pin Change Interrupt 3
jmp BadInt       ; 09 Watchdog Timer
jmp BadInt       ; 10 TIMER2 Compare Match A
jmp BadInt       ; 11 TIMER2 Compare Match B
jmp BadInt       ; 12 TIMER2 Overflow
jmp BadInt       ; 13 TIMER1 Capture Event
jmp DoVideoLine  ; 14 TIMER1 Compare Match A
jmp BadInt       ; 15 TIMER1 Compare Match B
jmp BadInt       ; 16 TIMER1 Overflow
jmp BadInt       ; 17 TIMER0 Compare Match A
jmp BadInt       ; 18 TIMER0 Compare Match B
jmp BadInt       ; 19 TIMER0 Overflow
jmp BadInt       ; 20 Serial Transfer Complete
jmp BadInt       ; 21 USART Rx Complete
jmp BadInt       ; 22 USART Data Register Empty
jmp BadInt       ; 23 USART Tx Complete
jmp BadInt       ; 24 Analog Comparator
jmp BadInt       ; 25 ADC Conversion Complete
jmp BadInt       ; 26 EEPROM Ready
jmp BadInt       ; 27 Two-Wire Interface
jmp BadInt       ; 28 Store Program Memory Ready

;
; ============================================
;   Interrupt Service Routines
; ============================================
;
; [Add all interrupt service routines here]
BadInt:
  reti

DoVideoLine:
  ; First: Reset timer1 for the next firing.
  ldi rtmp,(1<<COM1B1)
  sts TCCR1A, rtmp
  ldi rtmp,(1<<FOC1B)
  sts TCCR1C, rtmp
  ldi rtmp,(1<<COM1B1) | (1<<COM1B0)
  sts TCCR1A, rtmp

  START_DO_VIDEO_LINE

  ; NOPs added here if necessary to fix interrupt uncertainty
  nop
  nop

  ; Compare YH with 1... if >= go to secondhalf
  cpi YH, $01             ; 1 cycle
  brsh _secondhalf        ; 1 or 2 cycles

_firsthalf:
  ; Is YL >= 40? (40 is start of video)
  cpi YL, 40              ; 1 cycle
  brsh _vidline0          ; 1 or 2 cycles
  ; No?  Okay, then YL < 40.

  ; Is YL >= 2?  (2 is start of back porch)
  cpi YL, 2
  brsh _backporch
  ; No?  Okay, then YL < 2.

  ; Set VSYNC line low.
  cbi port_toggles2, pin_vsync
  jmp _incy

_backporch:
  ; Set VSYNC line high.
  sbi port_toggles2, pin_vsync    
  jmp _incy

_secondhalf:
  ; At this point YH is 01 or 02.
  
  ; If 256 <= YH:YL <= 511, output video.
  cpi YH, $01
  breq _vidline1 ; do video between 256 and 511

  ; If YH:YL < 515, output video.
  cpi YL, $03
  brlo _vidline2
  
  ; At this point, YH:YL is >= 515.
  
  ; Is it exactly 515?
  cpi YL, $03
  brne _checkforend
  ori rvidflags, (1 << flag_frameend)	; Set "end of frame" flag

_checkforend:
  ; If YH:YL < 524 (0x020C), increment Y.
  cpi YL, $0C
  brlo _incy

  ; Otherwise, clear YH:YL.
  clr YH
  clr YL
  END_DO_VIDEO_LINE

_incy:  
  ; Increment line counter
  adiw Y, 1
  END_DO_VIDEO_LINE

_vidline0:
  nop
_vidline1:
  nop
  nop
_vidline2:

_waitonhsync:
  ; Wait to finish HSYNC pulse + H back porch.
  ldi rtmp, 13

_hsyncbploop:
  dec rtmp
  brne _hsyncbploop
  nop

  ; We have approximately 486 cycles that are usable.
 
_pixelclock:
  ; Load current video address into X register.
  ; First, copy YH:YL into XH:XL.
  mov XH, YH
  mov XL, YL
  ; Subtract 40 to make it go from 0 to 475.
  subi XL, 40
  sbci XH, 0
  ; Shift right 1 bit to divide by 2 (giving 0 to 237).
  lsr XH
  ror XL
  ; Move XL into XH, and Video_XOffset into XL.
  mov XH, XL
  lds XL, Video_XOffset
  ; Add Video_YOffset to XH.
  lds rtmp, Video_YOffset
  add XH, rtmp

  ; Latch XH into high address.
  out port_addr, XH
  DO_ADDR_LATCH

  ; Clear data port.
  out port_data, rZero

  ; Turn off vblank.
  cbi port_toggles1, pin_Nvblank

  ; Output 240 pixels.
  DO_240_PIXELS
  
  ; Turn on vblank.
  sbi port_toggles1, pin_Nvblank

  ; Increment line counter
  adiw Y, 1
  END_DO_VIDEO_LINE

; ============================================
;   Main Program Initialization
; ============================================
;
Main:
  ; Initialize stack to the end of memory.
  ldi rtmp, HIGH(RAMEND) ; Init MSB stack
  out SPH, rtmp
  ldi rtmp, LOW(RAMEND) ; Init LSB stack
  out SPL,rtmp

  ; Initialize our "rZero" and "rFF" registers.
  ldi rZero, 0x00
  ldi rFF, 0xFF

  ; Init toggle ports to all outputs.
  out ddr_toggles1, rFF
  out port_toggles1, rZero

  ; Toggles2 port: Nmemwrite, Nmemread, Nlatchout should be high; rest should be low.
  ldi rtmp, (1 << pin_Nmemwrite) | (1 << pin_Nmemread) | (1 << pin_Nlatchout)
  out ddr_toggles2, rFF
  out port_toggles2, rtmp

  ; Init address port to low outputs.
  out ddr_addr, rFF
  out port_addr, rZero

  ; Init data port to inputs.
  out ddr_data, rZero
  out port_data, rZero

  ; Initialize TIMER1:
  ;   - Compare register 1A = 636 for 20MHz (02 7C)
  ldi rtmp,0x02
  sts OCR1AH,rtmp
  ldi rtmp,0x7C
  sts OCR1AL,rtmp

  ;   - OCR1B = 75 (3.75uS) for the HSYNC clock
  ldi rtmp, 0
  sts OCR1BH,rtmp
  ldi rtmp, 75
  sts OCR1BL,rtmp

  ;   - Interrupt on output compare
  lds rtmp, TIMSK1
  ori rtmp, (1<<OCIE1A)
  sts TIMSK1, rtmp

  ;   - Set OC1B output on OCR1B match.
  ldi rtmp,(1<<COM1B1) | (1<<COM1B0)
  sts TCCR1A,rtmp

  ;   - CTC mode
  ;   - /1 divider (16MHz or 20MHz)
  ldi rtmp,(1<<WGM12) | (1<<CS10)
  sts TCCR1B,rtmp
  
  ; Initialize video line registers
  clr YH
  clr YL
    
  ; [Add all other init routines here]
  ldi rtmp,1<<SE ; enable sleep
  out MCUCR,rtmp

  ; Lets load up our memory!
  MEMORY_READ_DISABLED

  rcall ClearMemory

  ;rcall MemoryTestPattern
  
  LOAD_IMM r0, 50
  LOAD_IMM r1, 30
  LOAD_IMM r2, 130
  LOAD_IMM r3, 130
  LOAD_IMM r4, 0b00000111
  rcall DrawFilledBox

  MEMORY_READ_ENABLED
  ; Turn on interrupts.
  sei

; ============================================
;   Main Program Loop
; ============================================
Loop:
  rcall WaitVSync

  lds rtmp, Video_YOffset
  ;inc rtmp 
  sts Video_YOffset, rtmp
  lds rtmp, Video_XOffset
  ;inc rtmp 
  sts Video_XOffset, rtmp
  rjmp loop						; Go back to top of loop


; ===========================================
;   W a i t V S y n c
; ===========================================
; Waits until a video frame has just ended.
; Parameters:
;	None
; Returns:
;	None
WaitVSync:
  sleep
  nop
  sbrs rvidflags, flag_frameend	; Has a frame ended?
  rjmp WaitVSync				; If not, loop
  andi rvidflags, ~(1 << flag_frameend)	; If so, clear the flag
  ret
; ============================================
;   C l e a r M e m o r y
; ============================================
; Clear out video memory.
; Parameters:
;   None
; Returns:
;   None
ClearMemory:
  push XH
  push XL

  ; Load XH:XL with $0000.
  eor XH, XH
  eor XL, XL

_clearMemoryLoop:
  ; Set address lines to XH, XL.
  out port_addr, XH
  DO_ADDR_LATCH
  out port_addr, XL
  
  ; For right now, set data to XH eor XL.

  ; Take the top four bits of XH and the top four bits of XL.

  out port_data, rZero

  ; Do a memory write.
  DO_MEMORY_WRITE

  ; Add 1 to XH:XL.
  adiw XH:XL, 1

  ; Check if XH:XL is 0000.
  tst XH
  brne _clearMemoryLoop
  tst XL
  brne _clearMemoryLoop

_clearMemoryDone:
  pop XL
  pop XH
  ret

; ============================================
;   D r a w P i x e l
; ============================================
; Plots a pixel at (r0, r1) in color r2.
; Parameters:
;  r0 = X coordinate
;  r1 = Y coordinate
;  r2 = Color
; Returns:
;  None
DrawPixel:
  out port_addr, r1
  DO_ADDR_LATCH
  out port_addr, r0
  out port_data, r2
  DO_MEMORY_WRITE
  ret

; ============================================
;   D r a w B o x
; ============================================
; Draws a box from (r0, r1) to (r2, r3), in color r4.
; Parameters:
;  r0 = Leftmost X location
;  r1 = Topmost Y location
;  r2 = Rightmost X location
;  r3 = Bottommost Y location
;  r4 = Color
; Returns:
;   None
DrawBox:
  push r5	; Counter
  push r6	; Temporary

  mov r6, r2
  inc r6

  ; Draw the horizontal lines.
  mov r5, r0

_hlineloop:
  out port_addr, r1
  DO_ADDR_LATCH
  out port_addr, r5
  out port_data, r4
  DO_MEMORY_WRITE
  out port_addr, r3
  DO_ADDR_LATCH
  out port_addr, r5
  DO_MEMORY_WRITE

  inc r5
  cp r5, r6
  brlo _hlineloop

  ; Draw the vertical lines.
  mov r5, r1
  inc r5
_vlineloop:
  out port_addr, r5
  DO_ADDR_LATCH
  out port_addr, r0
  out port_data, r4
  DO_MEMORY_WRITE
  out port_addr, r2
  DO_MEMORY_WRITE

  inc r5
  cp r5, r3
  brlo _vlineloop

  pop r6
  pop r5
  ret

; ============================================
;   D r a w F i l l e d B o x
; ============================================
; Draws a filled box from (r0, r1) to (r2, r3), in color r4.
; Parameters:
;  r0 = Leftmost X location
;  r1 = Topmost Y location
;  r2 = Rightmost X location
;  r3 = Bottommost Y location
;  r4 = Color
; Returns:
;   None
DrawFilledBox:
  push r5	; X Counter
  push r6	; X Temporary
  push r7   ; Y Counter
  push r8   ; Y Temporary

  mov r6, r2
  inc r6
  mov r8, r3
  inc r8

  mov r7, r1
_vloopbox:
  mov r5, r0

_hloopbox:
  out port_addr, r7
  DO_ADDR_LATCH
  out port_addr, r5
  out port_data, r4
  DO_MEMORY_WRITE

  inc r5
  cp r5, r6
  brlo _hloopbox

  inc r7
  cp r7, r8
  brlo _vloopbox

  pop r8
  pop r7
  pop r6
  pop r5
  ret


; ===========================================
;   M e m o r y T e s t P a t t e r n
; ===========================================
; Fill memory with a test pattern.
; Parameters:
;	None
; Returns:
;	None
  MemoryTestPattern:
  push XH
  push XL
  push rtmp
  push rtmp2

  ; Load XH:XL with $0000.
  eor XH, XH
  eor XL, XL

_fillMemoryLoop:
  ; Set address lines to XH, XL.
  out port_addr, XH
  DO_ADDR_LATCH
  out port_addr, XL
  
  ; For right now, set data to XH eor XL.

  ; Take the top four bits of XH and the top four bits of XL.

  mov rtmp, XH
  lsr rtmp
  lsr rtmp
  lsr rtmp
  andi rtmp, 0x0F

  mov rtmp2, XL
  lsr rtmp2
  lsr rtmp2
  lsr rtmp2
  andi rtmp2, 0x0F
  swap rtmp2

  or rtmp, rtmp2
  out port_data, rtmp

  ; Do a memory write.
  DO_MEMORY_WRITE

  ; Add 1 to XH:XL.
  adiw XH:XL, 1

  ; Check if XH:XL is 0000.
  tst XH
  brne _fillMemoryLoop
  tst XL
  brne _fillMemoryLoop

_fillMemoryDone:
  pop rtmp2
  pop rtmp
  pop XL
  pop XH
  ret