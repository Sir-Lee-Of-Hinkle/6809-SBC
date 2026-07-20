PORTB = $7FF0
PORTA = $7FF1
DDRB = $7FF2
DDRA = $7FF3

E  = %10000000
RW = %01000000
RS = %00100000

  org $8000

reset:
; ################ Initialize the PIA ##############
  lda #%11111111 ; Set all pins on port B to output
  sta DDRB

  lda #%11100000 ; Set top 3 pins on port A to output
  sta DDRA

; ################ Initialize the LCD ##############
  lda #%00111000 ; Set 8-bit mode; 2-line display; 5x8 font
  sta PORTB
  lda #0         ; Clear RS/RW/E bits
  sta PORTA
  lda #E         ; Set E bit to send instruction
  sta PORTA
  lda #0         ; Clear RS/RW/E bits
  sta PORTA

 ; ##### Wait until LCD is ready 4.1 ms 
  lcdbusy1:
  ldb #RW         ; Set Read Mode
  stb PORTA
  orb #E          ; Set E bit to send instruction
  stb PORTA
  ldb PORTB       ; Read the data pins
  andb #%10000000
  bne lcdbusy1

  ldb #RW
  stb PORTA
  ldb #%11111111  ; Port B is output
  stb DDRB

   
  
  lda #%00001110 ; Display on; cursor on; blink off
  sta PORTB
  lda #0         ; Clear RS/RW/E bits
  sta PORTA
  lda #E         ; Set E bit to send instruction
  sta PORTA
  lda #0         ; Clear RS/RW/E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop

  lda #%00000110 ; Increment and shift cursor; don't shift display
  sta PORTB
  lda #0         ; Clear RS/RW/E bits
  sta PORTA
  lda #E         ; Set E bit to send instruction
  sta PORTA
  lda #0         ; Clear RS/RW/E bits
  sta PORTA


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
  nop
  nop
  nop
  nop
  nop
  
  lda #%00000001 ; Display Clear
  sta PORTB
  lda #0         ; Clear RS/RW/E bits
  sta PORTA
  lda #E         ; Set E bit to send instruction
  sta PORTA
  lda #0         ; Clear RS/RW/E bits
  sta PORTA
  
  
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
  nop
  
  lda #$48	;"H"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop


  lda #$65	;"e"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop


  lda #$6C	;"l"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop


  lda #$6C	;"l"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop


  lda #$6F	;"o"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop


  lda #$2C	;","
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop


  lda #$20	;" "
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

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
  nop
  nop
  nop
  nop
  nop
  nop


  lda #$57	;"w"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

  jsr lcd_wait



  lda #$6F	;"o"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

  jsr lcd_wait



  lda #$72	;"r"
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	  ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

  jsr lcd_wait



  lda #$6C	;"l"
  jsr lcd_write_char
  ;sta PORTB
  ;lda #RS         ; Set RS; Clear RW/E bits
  ;sta PORTA
  ;ora #E   	  ; Set E bit to send instruction
  ;sta PORTA
  ;lda #RS         ; Clear E bits
  ;sta PORTA

  jsr lcd_wait



  lda #$64	;"d"
  jsr lcd_write_char
  ;sta PORTB
  ;lda #RS         ; Set RS; Clear RW/E bits
  ;sta PORTA
  ;ora #E   	  ; Set E bit to send instruction
  ;sta PORTA
  ;lda #RS         ; Clear E bits
  ;sta PORTA

  jsr lcd_wait



  lda #$21	;"!"
  jsr lcd_write_char
  ;sta PORTB
  ;lda #RS         ; Set RS; Clear RW/E bits
  ;sta PORTA
  ;ora #E   	  ; Set E bit to send instruction
  ;sta PORTA
  ;lda #RS         ; Clear E bits
  ;sta PORTA

  lda #$55
  sta PORTA
  lda #$AA
  sta PORTB


loop:
  
  
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
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  
  
  
  jmp loop

;#########################################################
;############ LCD Subroutines
;#########################################################

lcd_write_char:
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  ora #E   	      ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA

  rts

lcd_wait:
  ldb #%00000000  ; Port B is input
  stb DDRB
lcdbusy:
  ldb #RW         ; Set Read Mode
  stb PORTA
  orb #E          ; Set E bit to send instruction
  stb PORTA
  ldb PORTB       ; Read the data pins
  andb #%10000000
  bne lcdbusy

  ldb #RW
  stb PORTA
  ldb #%11111111  ; Port B is output
  stb DDRB

  rts
 


;#########################################################
;############ General Subroutines
;#########################################################
  
  org $fffe
  .word $8000