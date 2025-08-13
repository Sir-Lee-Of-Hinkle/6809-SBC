  org $8000
; RAM 0x0000 - 0x7FFF
; ROM 0x8000 - 0xDFFF
; PIA1 0xE000 - 0xE003
; PIA2 0xE004 - 0xE007
; ACIA 0xE008 - 0xE00F
; ROM 0xE010 - 0xFFFF
PORTB = $E000
PORTA = $E001
DDRB = $E002
DDRA = $E003

PORTC = $E004
PORTD = $E006
PORTCCTL = $E005
PORTDCTL = $E007
E  = %10000000
RW = %01000000
RS = %00100000

; ########### Program Start/Reset ##############
reset:
  ;lds #$7FFF      ; declare the system stack. it grows downward

; ########## Test Code #######
  
  ;lda #$55
  ;sta [$1000]
  
  ;lda #AA
  ;sta > #$5000
  
PIA1:
  lda #%11111111 ; Set all pins on port B to output
  sta DDRB

  lda #%00000000 ; Set all pins on port A to input
  sta DDRA
  
PIA2:
  ; --- Configure Port C ---
  ;LDA #$00
  ;STA PORTCCTL           ; Control register to Direction

  ;LDA #$00
  ;STA PORTC              ; Set all to input

  ;LDA #$04               ; #%00000111  CA1 low to high triggers IRQA
  ;STA PORTCCTL           ; Control register to Data (bit 0-2 = 1)
   
  ; --- Configure Port D ---
  ;LDA #$00
  ;STA PORTDCTL           ; Control register to Direction

  ;LDA #$FF
  ;STA PORTD              ; Set all to output

  ;LDA #$04
  ;STA PORTDCTL           ; Control register to Data (bit 2 = 1)


  lda #$55
  ;ta PORTB
  ;sta PORTD
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  
  lda #$AA
  sta PORTB
  ;sta PORTD
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  
  lda #$BB
  sta PORTB
  ;sta PORTD
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  
  

INIT:
  nop
  nop
  nop
  nop
  nop
  nop
  jmp INIT


;###########################################################
;######## Setup misc. Vectors
;###########################################################
  org $FFF6
  fdb $1212
  fdb $1212
  fdb $1212
  fdb $8000               ; place reset vector