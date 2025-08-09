  org $8000
; RAM 0x0000 - 0x7FFF
; ROM 0x8000 - 0xDFFF
; PIA1 0xE000 - 0xE003
; PIA2 0xE004 - 0xE007
; ACIA 0xE008 - 0xE00F
; ROM 0xE010 - 0xFFFF
PORTD = $E006
PORTDCTL = $E007
E  = %10000000

; ########### Program Start/Reset ##############
reset:
  lds #$7FFF      ; declare the system stack. it grows downward

; ########## Test Code #######
  
  lda #$55
  sta [$1000]
  
  
  ; --- Configure Port D ---
  LDA #$00
  STA PORTDCTL           ; Control register to Direction

  LDA #$FF
  STA PORTD              ; Set all to output

  LDA #$04
  STA PORTDCTL           ; Control register to Data (bit 2 = 1)


  lda #$55
  sta PORTD
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  
  lda #$AA
  sta PORTD
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
  sta PORTD
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
  bra INIT


;###########################################################
;######## Setup misc. Vectors
;###########################################################
  org $FFF6
  fdb $1212
  fdb $1212
  fdb $1212
  fdb $8000               ; place reset vector