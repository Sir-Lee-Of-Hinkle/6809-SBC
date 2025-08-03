; This is a port of the 6800 version of Woz Mon to my 6809-based Single
; Board Computer. It was converted to 6809 instructions as well as
; ported to use the 6850 ACIA for input/output.

; The original 6800 port came from here: https://pastebin.com/TSM2DdRL

; Note: The code internally converts all characters to high ASCII (bit
; 7 = 1) because the Apple 1 used this format and the program logic is
; dependent on it in several places.

; The code is not quite small enough to fit in 256 bytes as the
; original 6502 and 6800 versions did.

;***********************************************************************

; This is a rewrite of the Apple 1 monitor to run on an MC6800
; microprocessor, rather than the MCS6502 microprocessor that
; was standard.  This source code will assemble with the
; AS Macro Assembler; with minor changes it should assemble
; with any MC6800 assembler.

; Copyright 2011 Eric Smith <eric@brouhaha.com>
;
; This program is free software; you can redistribute and/or modify it
; under the terms of the GNU General Public License version 3 as
; published by the Free Software Foundation.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; The text of the license may be found online at:
;     http://www.brouhaha.com/~eric/software/GPLv3
; or:
;     http://www.gnu.org/licenses/gpl-3.0.txt

cr          equ  $8d        ; Carriage return
lf          equ  $8a        ; Line feed
esc         equ  $9b        ; Escape

xam         equ  $0024      ; two bytes
st          equ  $0026      ; two bytes
h           equ  $0028
l           equ  $0029

mode        equ  $002b
ysav        equ  $002c      ; two bytes
inptr       equ  $002e      ; two bytes

in          equ  $0200

KBD         equ  $e004      ; PIA.A keyboard input 6522 VIA
KBDCR       equ  $e005      ; PIA.A keyboard control register

kb_wptr     equ  $0000      ; keyboard write pointer
kb_rptr     equ  $0001      ; keyboard read pointer

kb_flags    equ  $0002      ; location of the keyboard flag byte
RELEASE     equ  %00000001  ; keyboard flags
SHIFT       equ  %00000010  ; keyboard flags

kb_buffer   equ  $0200  ; 256-byte kb buffer 0200-02ff

aciac       equ  $e008      ; 6850 ACIA status/control register
aciad       equ  $e009      ; 6850 ACIA data register


; Use this line to run from RAM
;           org  $1000

; Use these lines to run from a 16K ROM
            org  $c000
            fcb $fee0-* dup($ff)


reset:      lda  #3         ; Reset ACIA
            sta  aciac
            lda  #$15       ; Control register setting
            sta  aciac      ; Initialize ACIA to 8 bits and no parity

            lds  #$7fff     ; On the 6502, the monitor didn't initialize the
                            ;  stack pointer, which was OK because it was
                            ;  guaranteed to be somewhere in page 1. Not so
                            ;  on the 6800!
                            ; Ideally, I'd take advantage of the stack
                            ;  starting right before the input buffer to
                            ;  save a few bytes, but I haven't yet figured
                            ;  out how to do it.
            LDA  #%10100111    ; KBD and DSP control register mask
            STA  KBDCR          ; Enable interrupts, set CA1, CB1 for
            
            lda  #$00
            sta  kb_flags
            sta  kb_wptr
            sta  kb_rptr

            bra  escape

; Get a line of input from the keyboard, echoing to display.
; Normally enter at escape or getline.

notcr:      cmpa #$df       ; "_"?  [NB back arrow]
            beq  backspace  ; Yes.
            cmpa #esc       ; ESC?
            beq  escape     ; Yes.
            leax 1,x        ; Advance text index.
            incb
            bpl  nextchar   ; Auto ESC if > 127.

escape:     lda  #$dc       ; "\".
            jsr  echo       ; Output it.

getline:    lda  #cr        ; CR.
            jsr  echo       ; Output it.
            lda  #lf
            jsr  echo       ; Output it.
            ldx  #in+1      ; Initialize text index.
            ldb  #1
backspace:  leax -1,x       ; Back up text index.
            decb
            bmi  getline    ; Beyond start of line, reinitialize.

nextchar:                   ; Wait for key press
            orcc  #$10      ; Set the I bit in the Condition Code Register (CC) to DISABLE IRQ
            lda  kb_rptr     ; keyboard read pointer
            cmpa  kb_wptr    ; keyboard write pointer
            andcc  #$EF      ; Clear the I bit in CC to ENABLE IRQ
            BEQ  nextchar    ; loop if no new characters
            ldx  kb_rptr     ; get the keyboard read pointer address
            ldy  kb_buffer   ; get the keyboard buffer address
            addr y,x         ; add the buffer address to the pointer store in x register
            lda  ,x           ; get the character being pointed to by the keyboard pointer

            ora  #$80       ; Convert to high ASCII.
            sta  ,x         ; Add to text buffer.
            bsr  echo       ; Display character.
            cmpa #cr        ; CR?
            bne  notcr      ; No.
            inc kb_rptr     ; increment the keyboard read pointer

; IRQ vector points here
keyboard_interrupt:
            pshs a
            tfr  x,a
            pshs a
            lda kb_flags
            anda #RELEASE   ; check if we're releasing a key
            beq read_key    ; otherwise, read the key

            lda kb_flags
            eora #RELEASE    ; flip the releasing bit
            sta kb_flags
            lda KBD         ; read key value that's being released
            cmpa #$12       ; left shift
            beq shift_up
            cmpa #$59       ; right shift
            beq shift_up
            jmp exit

shift_up:
            lda kb_flags
            eora #SHIFT  ; flip the shift bit
            sta kb_flags
            jmp exit

read_key:
            lda KBD
            cmpa #$f0        ; if releasing a key
            beq key_release ; set the releasing bit
            cmpa #$12        ; left shift
            beq shift_down
            cmpa #$59        ; right shift
            beq shift_down

            tfr a,x
            lda kb_flags
            anda #SHIFT
            bne shifted_key

            ldx #keymap            
            lda x           ; map to character code
            jmp push_key

shifted_key:
            ldx #keymap_shifted
            lda x   ; map to character code

push_key:
            ldx kb_wptr
            sta kb_buffer, x
            inc kb_wptr
            jmp exit

shift_down:
            lda kb_flags
            ora #SHIFT
            sta kb_flags
            jmp exit

key_release:
            lda kb_flags
            ora #RELEASE
            sta kb_flags

exit:
            puls a
            tfr a,x
            puls a
            rti

; Process an input line.

ret:        ldx  #in+256-1  ; Reset text index to in-1, +256 so that
                            ;  'inc inptr+1' will result in $0200.
            stx  inptr
            clra            ; For XAM mode. 0->B.

setblok:    asla            ; Leaves $56 if setting BLOCK XAM mode.
setmode:    sta  mode       ; $00 = XAM, $BA = STOR, $56 = BLOK XAM.
blskip:     inc  inptr+1    ; Advance text index.
nextitem:   ldx  inptr
            lda  ,x         ; Get character.
            cmpa #cr        ; CR?
            beq  getline    ; Yes, done this line.
            cmpa #$ae       ; "."?
            beq  setblok    ; Set BLOCK XAM mode.
            bls  blskip     ; Skip delimiter.
            cmpa #$ba       ; ":"?
            beq  setmode    ; Yes, set STOR mode.
            cmpa #$d2       ; "R"?
            beq  run        ; Yes, run user program.
            clr  l          ; $00->L.
            clr  h          ;  and H.
            stx  ysav       ; Save Y for comparison.

nexthex:    ldx  inptr
            lda  ,x         ; Get character for hex test.
            eora #$b0       ; Map digits to $0-9.
            cmpa #$09       ; Digit?
            bls  dig        ; Yes.
            adda #$89       ; Map letter "A"-"F" to $FA-FF.
            cmpa #$f9       ; Hex letter?
            bls  nothex     ; No, character not hex.

dig:        asla            ; Hex digit to MSD of A.
            asla
            asla
            asla

            ldb  #$04       ; Shift count.
hexshift:   asla            ; Hex digit left, MSB to carry.
            rol  l          ; Rotate into LSD.
            rol  h          ; Rotate into MSD's.
            decb            ; Done 4 shifts?
            bne  hexshift   ; No, loop.

            inc  inptr+1    ; Advance text index.
            bra  nexthex    ; Always taken. Check next character for hex.

nothex:     cmpx  ysav       ; Check if L, H empty (no hex digits).
            beq  escape     ; Yes, generate ESC sequence.
            tst  mode       ; Test MODE byte.
            bpl  notstor    ; B6=0 for STOR, 1 for XAM and BLOCK XAM

; STOR mode
            ldx  st
            lda  l          ; LSD's of hex data.
            sta  ,x         ; Store at current 'store index'.
            leax 1,x
            stx  st
tonextitem: bra  nextitem   ; Get next command item.

prbyte:     pshs a          ; Save A for LSD.
            lsra
            lsra
            lsra            ; MSD to LSD position.
            lsra
            bsr  prhex      ; Output hex digit.
            puls a          ; Restore A.
prhex:      anda #$0f       ; Mask LSD for hex print.
            ora  #$b0       ; Add "0".
            cmpa #$b9       ; Digit?
            bls  echo       ; Yes, output it.
            adda #$07       ; Add offset for letter.

echo:       ldb  aciac
            bitb #$02       ; bit (B2) cleared yet?
            beq  echo       ; No, wait for display.
            anda #$7F       ; Convert to low ASCII.
            sta  aciad      ; Output character.
            ora #$80        ; Convert back to high ASCII.
            rts             ; Return.

run:        ldx  xam
            jmp  ,x         ; Run at current XAM index.

notstor:    bne  xamnext    ; mode = $00 for XAM, $56 for BLOCK XAM.

            ldx  h          ; Copy hex data to
            stx  st         ;  'store index'.
            stx  xam        ; And to 'XAM index'.
            clra            ; set Z flag to force following branch.

nxtprnt:    bne  prdata     ; NE means no address to print.
            lda  #cr        ; CR.
            bsr  echo       ; Output it.
            lda  #lf        ; LF.
            bsr  echo       ; Output it.
            lda  xam        ; 'Examine index' high-order byte.
            bsr  prbyte     ; Output it in hex format.
            lda  xam+1      ; Low-order 'Examine index' byte.
            bsr  prbyte     ; Output it in hex format.
            lda  #$ba       ; ":".
            bsr  echo       ; Output it.

prdata:     lda  #$a0       ; Blank.
            bsr  echo       ; Output it.

            ldx  xam
            lda  ,x         ; Get data byte at 'examine index'.
            bsr  prbyte     ; Output it in hex format.

xamnext:    clr  mode       ; 0->MODE (XAM mode).
            ldx  xam        ; Compare 'examine index' to hex data.
            cmpx  h
            beq  tonextitem ; Not less, so more data to output.
            leax 1,x
            stx  xam
            lda  xam+1      ; Check low-order 'examine index' byte
            anda #$07       ; For MOD 8 = 0
            bra  nxtprnt    ; always taken

keymap:
                fcb "????????????? `?"
                fcb "?????q1???zsaw2?"
                fcb "?cxde43?? vftr5?"
                fcb "?nbhgy6???mju78?"
                fcb "?,kio09??./l;p-?"
                fcb "??'?[=????",$0A,"]?\??"
                fcb "?????????1?47???"
                fcb "0.2568",$1B,"??+3-*9??"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"

keymap_shifted:
                fcb "????????????? ~?"
                fcb "?????Q!???ZSAW@?"
                fcb "?CXDE#$?? VFTR%?"
                fcb "?NBHGY^???MJU&*?"
                fcb "?<KIO)(??>?L:P_?"
                fcb "??\"?{+?????}?|??"
                fcb "?????????1?47???"
                fcb "0.2568???+3-*9??"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
                fcb "????????????????"
; Below is only needed if you want to run from ROM as a standalone
; monitor.

;           org  $fff0      ; vector table

            fdb  $0000      ; Reserved
            fdb  $0000      ; SWI3
            fdb  $0000      ; SWI2
            fdb  $0000      ; FIRQ
            fdb  keyboard_interrupt      ; IRQ
            fdb  $0000      ; SWI
            fdb  $0000      ; NMI
            fdb  reset      ; RESET
