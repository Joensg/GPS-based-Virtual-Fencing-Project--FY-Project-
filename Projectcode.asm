;/*
; * Projectcode.asm
; *
; *  Created: 9/14/2012 12:27:23 PM
; *  Last Modified: 4/25/2013 3:45:23 PM
; *   Author: Joel Sequeira
; */ 
;------------ Program for designing a GPS based Virtual Fencing System using Atmega32A 
;------------ Displaying the data on an LCD (LCD 16X2)
;Crystal Frequency 8 MHz

;Synopsis
;An LCD display to Processor interface

;LCD(16X2) to AVR Processor connections:
;    Pin Numbers(LCD): 1    2     3     4    5     6     7    8    9    10   11   12   13   14 
;      Pin Names(LCD): VSS  VDD   VEE   RS   R/W   E     D0   D1   D2   D3   D4   D5   D6   D7 
;Port Names(Atmega32):  -    -     -    PC5  PC6   PC7   PB0  PB1  PB2  PB3  PB4  PB5  PB6  PB7

.include "m32def.inc"                            ;include port pin definitions for Atmega32A

;Equating labels to RAM addresses(internal data SRAM)
.equ RAM_addr_1 = 0x0060
.equ RAM_addr_2 = 0x0070
.equ RAM_addr_3 = 0x0080
.equ RAM_addr_4 = 0x0090
.equ RAM_addr_5 = 0x00A0
.equ RAM_addr_6 = 0x00B0
.equ RAM_addr_7 = 0x00C0
;----------------------------------

;Equating labels to Register Files
.def temp = r16
.def temp_1 = r19
.def lcd_out = r17
.def key_in = r18
.def temp_key = r24
.def gps_in = r23
.def gps_count = r25
.def rreg_sec = r20
.def rreg_min = r21
.def rreg_deg = r22
;----------------------------------

;------ Upon wake up, go to main, avoid using memory space allocated to interrupt vector table
                   .device ATmega32            ;device used
                   .cseg                       ;code segment, translate following to code and write to code section of chip
                   .org 0x0000                 ;Start(origin) at location 0, reset vector
	               jmp RESET                   ;bypass interrupt vector table, jump to Reset Handler
;-------------------------------------------------

;-----------------------Interrupt Vectors
;------------USART : RXC USART interrupt Vector, Indicates Rx Complete
                   .org 0x001A                 ;Rx Complete default vertor location
                   jmp USART_RXC               ;USART RX Complete Handler
;-------------------------------------------------


;------------ the main program for initialization and other tasks
                   .org 0x002A                 ;a location after the interrupt vectors
  	        RESET: ldi temp, low(RAMEND)       ;Reset Handler
                   out SPL, temp               ;Initialize the Stack Pointer 
                   ldi temp, high(RAMEND)
                   out SPH, temp

;---- disabling JTAG interface
; In order to avoid unintentional disabling or enabling of the JTAG interface,
; a timed sequence must be followed when changing this bit.
; The application software must write this bit to the desired value
; twice within four cycles to change its value.
                   ldi   temp, 0x80
                   out   mcucsr, temp          ;JTAG
                   out   mcucsr, temp          ;disabled 
;-------------------------------------------------                         
			       sei                         ;set Global Interrupt Enable

;**********************************************************
;Main Part of the Program
;**********************************************************
             main: rcall LCD_init              ;call sub-routine to initialise LCD
			       ldi ZL, low(text_LUT1*2)
                   ldi ZH, high(text_LUT1*2)
				   rcall LCD_disp_text		   ;call sub-routine to display text1
                   rcall delay1                ;display for some time

                   rcall LCD_init
				   ldi ZL, low(text_LUT2*2)
                   ldi ZH, high(text_LUT2*2)
				   rcall LCD_disp_text		   ;call sub-routine to display text2
				   rcall delay1                ;display for some time

                   ldi XL, low(RAM_addr_6)
                   ldi XH, high(RAM_addr_6)    ;use register pair X as pointer
                   rcall keypad                ;take in input from user
                   mov temp, key_in
                   rcall ASCII_BCD_convrt
                   st X+, temp                 ;store indirect key pressed.
                   rcall delay1                ;display for some time
                   rcall delay1                ;display for some time
                   rcall keypad                ;take in input from user
                   mov temp, key_in
                   rcall ASCII_BCD_convrt
                   st X+, temp                 ;store indirect key pressed.
                   rcall delay1                ;display for some time
                   rcall delay1                ;display for some time
                   rcall keypad                ;take in input from user
                   mov temp, key_in
                   rcall ASCII_BCD_convrt
                   st X+, temp                 ;store indirect key pressed.
                   rcall delay1                ;display for some time
                   rcall delay1                ;display for some time

				   rcall LCD_init
				   ldi ZL, low(text_LUT3*2)
                   ldi ZH, high(text_LUT3*2)
				   rcall LCD_disp_text		   ;call sub-routine to display text3
				   rcall delay1                ;display for some time

                

                   rcall LCD_init              ;call sub-routine to initialise LCD
                   rcall GPS                   ;initialise and start GPS Communication
                   ldi XL, low(RAM_addr_1)
                   ldi XH, high(RAM_addr_1)
                   rcall LCD_disp_GPS		   ;call sub-routine to display GPS_GMT data
				   rcall delay1                ;display for some time
				   rcall LCD_init
                   ldi XL, low(RAM_addr_2)
                   ldi XH, high(RAM_addr_2)
                   rcall LCD_disp_GPS		   ;call sub-routine to display GPS_latitude
				   rcall delay1                ;display for some time
				   rcall LCD_init
                   ldi XL, low(RAM_addr_3)
                   ldi XH, high(RAM_addr_3)
                   rcall LCD_disp_GPS		   ;call sub-routine to display GPS_longitude,etc.
				   rcall delay1                ;display for some time
                   rcall LCD_init
                   ldi XL, low(RAM_addr_4)
                   ldi XH, high(RAM_addr_4)
                   rcall LCD_disp_GPS		   ;call sub-routine to display GPS_longitude,etc.
				   rcall delay1                ;display for some time
                   rcall LCD_init
                   ldi XL, low(RAM_addr_5)
                   ldi XH, high(RAM_addr_5)
                   rcall LCD_disp_GPS		   ;call sub-routine to display GPS_longitude,etc.
				   rcall delay1                ;display for some time
                   rcall delay_1sec            ;display for some time
                   rcall delay_1sec            ;display for some time
				   rcall LCD_init
				   
;----------------degrees
				   ldi XL, low(RAM_addr_2)     ;for accessing
                   ldi XH, high(RAM_addr_2)
				   clr temp
				   clr temp_1
				   ld temp, X                  ;load data memory, temp=data memory(x)
				   rcall ASCII_BCD_convrt
				   mov temp_1, temp            ;temp_1 holds MSB
				   inc XL					   ;point to next data code
				   clr temp
				   ld temp, X
				   rcall ASCII_BCD_convrt      ;temp holds LSB
				   rcall BCD_hex_convrt        ;hex value obtained in temp
				   
				   ldi XL, low(RAM_addr_7)     ;for storing
                   ldi XH, high(RAM_addr_7)
                   st X+, temp                 ;store hex value
				   mov rreg_deg, temp
				   
;----------------minutes
				   ldi XL, low(RAM_addr_2)     ;for accessing
                   ldi XH, high(RAM_addr_2)
				   clr temp
				   clr temp_1
				   inc XL
				   inc XL
				   ld temp, X                  ;load data memory, temp=data memory(x)
				   rcall ASCII_BCD_convrt
				   mov temp_1, temp            ;temp_1 holds MSB
				   inc XL					   ;point to next data code
				   clr temp
				   ld temp, X
				   rcall ASCII_BCD_convrt      ;temp holds LSB
				   rcall BCD_hex_convrt        ;hex value obtained in temp
				   
				   ldi XL, low(RAM_addr_7)     ;for storing
                   ldi XH, high(RAM_addr_7)
				   inc XL
                   st X+, temp                 ;store hex value
				   mov rreg_min, temp
				   
;----------------seconds
				   ldi XL, low(RAM_addr_2)     ;for accessing
                   ldi XH, high(RAM_addr_2)
				   clr temp
				   clr temp_1
				   inc XL
				   inc XL
				   inc XL
				   inc XL
				   ld temp, X                  ;load data memory, temp=data memory(x)
				   rcall ASCII_BCD_convrt
				   mov temp_1, temp            ;temp_1 holds MSB
				   inc XL					   ;point to next data code
				   clr temp
				   ld temp, X
				   rcall ASCII_BCD_convrt      ;temp holds LSB
				   rcall BCD_hex_convrt        ;hex value obtained in temp
				   
				   ldi XL, low(RAM_addr_7)     ;for storing
                   ldi XH, high(RAM_addr_7)
				   inc XL
				   inc XL
                   st X+, temp                 ;store hex value
				   mov rreg_sec, temp
;---------------------------  
				   
			   
				   
	         wait: jmp wait                    ;loop here endlessly
;---------------- End of main program --------------------------------------




;**********************************************************
;LCD (16X2) Specific Sub-Routines
;**********************************************************
;@@---For LCD, PA=data pins, PC5=RS, PC6=R/W, PC7=E pins
;--------------- LCD Initalisation
		 LCD_init: ldi ZL, low(command_LUT*2)
                   ldi ZH, high(command_LUT*2)
                   ldi temp, 0xE0              ;initialize Port C(7,6,5) as output
                   out DDRC, temp              ;DDRC=FF, Port C(7,6,5) configured as output
		       C1: clr lcd_out
		           lpm lcd_out, Z              ;load prog memory, lcd_out=prog memory(Z)
			       rcall cmd_wrt               ;call command subroutine
			       inc ZL					   ;point to next command code
                   clr lcd_out
				   lpm lcd_out, Z 			   ;to avoid sending null character as command
                   cpi lcd_out, 0x00           ;compare with immediate value(00)
			       brne C1					   ;keep sending commands until fully initialised
			       ret					       
;-------------------------------------------------

;--------------- LCD text display Routine
    LCD_disp_text: clr lcd_out
                   lpm lcd_out, Z              ;load prog memory, lcd_out=prog memory(Z)
			       rcall data_wrt              ;call data subroutine
			       rcall delay1                ;give LCD some time
			       inc ZL					   ;point to next data code
				   clr lcd_out
                   lpm lcd_out, Z 			   ;to avoid sending null character as data
                   cpi lcd_out, 0x00           ;compare with immediate value(00)
			       brne LCD_disp_text		   ;repeat until the last character                 
				   ret	   
;-------------------------------------------------

;--------------- LCD GPS data display Routine
     LCD_disp_GPS: clr lcd_out
                   ld lcd_out, X               ;load data memory, lcd_out=data memory(x)
			       rcall data_wrt              ;call data subroutine
			       rcall delay1                ;give LCD some time
			       inc XL					   ;point to next data code
				   clr lcd_out
                   ld lcd_out, X 			   ;to avoid sending character B as data
                   cpi lcd_out, 0x42           ;compare with immediate value(ASCII value of B)
			       brne LCD_disp_GPS		   ;repeat until the last character                 
				   ret	   
;-------------------------------------------------

;--------------- Command Write sub-routine
	      cmd_wrt: rcall ready                 ;is LCD ready?
	               cbi PORTC, 5                ;RS=0 for command
			       cbi PORTC, 6                ;R/W=0 to write to LCD
			       sbi PORTC, 7                ;E=1 for high pulse
                   ldi temp, 0xFF              ;initialize Port A as output
                   out DDRA, temp              ;DDRA=FF, Port A configured as output
	               out PORTA, lcd_out		   ;send command to LC
			       cbi PORTC, 7                ;E=0 for H-to-L pulse
			       ret
;-------------------------------------------------

;--------------- Data Write sub-routine
	     data_wrt: rcall ready                 ;is LCD ready?
			       SBi PORTC,5                 ;RS=1 for data
			       CBI PORTC,6                 ;R/W=0 to write to LCD
			       SBi PORTC,7                 ;E=1 for high pulse
                   ldi temp, 0xFF              ;initialize Port A as output
                   out DDRA, temp              ;DDRA=FF, Port A configured as output
	               out PORTA, lcd_out		   ;issue data to P1
			       CBI PORTC,7                 ;E=0 for H-to-L pulse
			       ret
;-------------------------------------------------

;--------------- Ready sub-routine	[Checks Busy Flag of LCD(D7 bit)]
		    ready: ldi temp, 0x00              ;initialize PA7 as input
                   out DDRA, temp              ;DDRA=00, PA7 configured as input
                   ldi temp, 0xFF               
                   out PORTA, temp             ;activate pull up
		           CBI PORTC,5                 ;RS=0 to access command reg
			       SBI PORTC,6                 ;R/W=1 to read command reg
		   ;read command reg and check busy flag(D7)
	     rdy_back: CBI PORTC,7                 ;E=0 for L-to-H pulse
			       sbi PORTC,7                 ;E=1 for L-to-H pulse
                   nop
                   in r0, PINA                 ;read Port A, store in R0
                   sbrc r0, 7                  ;skip next instruction if R0(7)=0
			       rjmp rdy_back               ;keep checking until busy flag=0
			       ret
;-------------------------------------------------
;---------------- End of LCD (16X2) Specific Sub-Routines --------------------------------------




;**********************************************************
;Keypad Specific Sub-Routines
;**********************************************************
;--------------- Program for keypad (4X4)
           keypad: ldi temp, 0xF0              ;initialize Port B as input(PB0-PB3), output(PB4-PB7)
                   out DDRB, temp              ;DDRB=F0, Port B configured as input-output            
       key_loop_1: ldi temp, 0x0F
                   out PORTB, temp             ;ground all rows
                   nop                         ;nop for synchronization
                   in r18, PINB                ;read all columns, ensure all keys open
                   andi key_in, 0x0F           ;mask un-used bits
                   cpi key_in, 0x0F            ;compare key_in with 0x0F
                   brne key_loop_1             ;check till all keys released

       key_loop_2: rcall delay_20ms			   ;call 20ms delay
                   in key_in, PINB             ;read all columns, check for any key press
                   andi key_in, 0x0F           ;mask un-used bits
                   cpi key_in, 0x0F            ;compare key_in with 0x0F
                   brne locate                 ;key pressed, await closure
                   rjmp key_loop_2             ;keep checking for key press

           locate: rcall delay_20ms			   ;wait for 20ms key debounce time
                   in key_in, PINB             ;check key closure
                   andi key_in, 0x0F           ;mask un-used bits
                   cpi key_in, 0x0F            ;compare key_in with 0x0F
                   brne loc_row                ;key pressed, find row
                   rjmp key_loop_2             ;if none, keep polling

		  loc_row: ldi temp, 0x04              ;counter for columns
                   ldi temp_key, 0x7F              
                   out PORTB, temp_key         ;ground row_0
                   nop                         ;nop for synchronization
                   in key_in, PINB             ;read all columns
                   andi key_in, 0x0F           ;mask un-used bits 
                   cpi key_in, 0x0F            ;compare key_in with 0x0F
                   brne loc_column_0           ;key in row_0, find column
                   lsr temp_key                ;logical shift right
                   out PORTB, temp_key         ;ground row_1
                   nop                         ;nop for synchronization
                   in key_in, PINB             ;read all columns
                   andi key_in, 0x0F           ;mask un-used bits
                   cpi key_in, 0x0F            ;compare key_in with 0x0F
                   brne loc_column_1           ;key in row_1, find column
                   lsr temp_key                ;logical shift right
                   out PORTB, temp_key         ;ground row_2
                   nop                         ;nop for synchronization
                   in key_in, PINB             ;read all columns
                   andi key_in, 0x0F           ;mask un-used bits
                   cpi key_in, 0x0F            ;compare key_in with 0x0F
                   brne loc_column_2           ;key in row_2, find column
                   lsr temp_key                ;logical shift right
                   out PORTB, temp_key         ;ground row_3
                   nop                         ;nop for synchronization
                   in key_in, PINB             ;read all columns
                   andi key_in, 0x0F           ;mask un-used bits
                   cpi key_in, 0x0F            ;compare key_in with 0x0F
                   brne loc_column_3           ;key in row_3, find column
                   jmp key_loop_2              ;if none, false input, repeat

	 loc_column_0: ldi ZL, low(row0_LUT*2)
                   ldi ZH, high(row0_LUT*2)    ;set data pointer to start of row_0
				   rjmp loc_column			   ;find column that the Key belongs to
     loc_column_1: ldi ZL, low(row1_LUT*2)
                   ldi ZH, high(row1_LUT*2)    ;set data pointer to start of row_1
				   rjmp loc_column			   ;find column that the Key belongs to
     loc_column_2: ldi ZL, low(row2_LUT*2)
                   ldi ZH, high(row2_LUT*2)    ;set data pointer to start of row_2
				   rjmp loc_column			   ;find column that the Key belongs to
     loc_column_3: ldi ZL, low(row3_LUT*2)
                   ldi ZH, high(row3_LUT*2)    ;set data pointer to start of row_3
       loc_column: clc                         ;clear carry flag
                   ror key_in                  ;rotate right through carry
                   brcc column_match           ;if CY=0, get the ASCII code from LUT
                   dec temp                    ;decrement counter
                   cpi temp, 0x00              ;compare with 0
                   brne key_cont
                   jmp key_loop_1
         key_cont: inc ZL
                   inc ZL                      ;point to next column address
                   rjmp loc_column             ;keep searching
	 column_match: clr key_in
                   lpm key_in, Z               ;load prog memory, key_in=prog memory(Z)
                   rcall LCD_disp_text         ;display pressed key	   
				   ret

;-------------------------------------------------  
;---------------- Keypad Specific Sub-Routines ---------------------




;**********************************************************
;GPS Specific Sub-Routines
;**********************************************************
;--------------- Program for GPS communication
              GPS: sbi UCSRB, RXCIE          ;set RX Complete interrupt enable
                ;setting the baud rate of 9600
                   ldi temp, 0x00              
                   out UBRRH, temp             
                   ldi temp, 0x33              
                   out UBRRL, temp           ;set UBRR to 51d , baud rate of 9600 for 8Mhz crystal freq.
                ;setting frame format of 8 data bits, 1 stop bit in async. mode
                   ldi temp, 0x86              
                   out UCSRC, temp           ;set UCSRC to 86, async mode, 8-bit character size, 1 stop bit
                ;setting Receiver enable
                   ldi gps_count, 0x00       ;count to keep track of GPS data bits received
                   sbi UCSRB, RXEN           ;enable USART Receiver

;----GPS communication started-----------------------
;----GPS_check_flag----------------------------
                   ldi temp_1, 0x00          ;reset temp_1 flag
   GPS_check_flag: cpi temp_1, 0x00
                   breq GPS_check_flag       ;wait for next byte to be received
                   cpi gps_in, 0x24          ;compare gps_in with ASCII value of $
                   brne GPS_check_flag       ;keep checking for start of message 

;----GPS_msg_type1-----------------------------
                   ldi temp_1, 0x00          ;reset temp_1 flag
  GPS_msg_type1_1: cpi temp_1, 0x00
                   breq GPS_msg_type1_1      ;wait for next byte to be received
                   cpi gps_in, 0x47          ;compare gps_in with ASCII value of G
                   brne GPS_check_flag       ;check for G

                   ldi temp_1, 0x00          ;reset temp_1 flag
  GPS_msg_type1_2: cpi temp_1, 0x00
                   breq GPS_msg_type1_2      ;wait for next byte to be received
                   cpi gps_in, 0x50          ;compare gps_in with ASCII value of P
                   brne GPS_check_flag       ;check for P

                   ldi temp_1, 0x00          ;reset temp_1 flag
  GPS_msg_type1_3: cpi temp_1, 0x00
                   breq GPS_msg_type1_3      ;wait for next byte to be received
                   cpi gps_in, 0x47          ;compare gps_in with ASCII value of G
                   brne GPS_check_flag       ;check for G

                   ldi temp_1, 0x00          ;reset temp_1 flag
  GPS_msg_type1_4: cpi temp_1, 0x00
                   breq GPS_msg_type1_4      ;wait for next byte to be received
                   cpi gps_in, 0x47          ;compare gps_in with ASCII value of G
                   brne GPS_check_flag       ;check for G

                   ldi temp_1, 0x00          ;reset temp_1 flag
  GPS_msg_type1_5: cpi temp_1, 0x00
                   breq GPS_msg_type1_5      ;wait for next byte to be received
                   cpi gps_in, 0x41          ;compare gps_in with ASCII value of A
                   brne GPS_check_flag       ;check for A

                   ldi temp_1, 0x00          ;reset temp_1 flag
      GPS_comma_1: cpi temp_1, 0x00
                   breq GPS_comma_1          ;wait for next byte to be received
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_check_flag       ;check for comma

;----GPS_GMT time------------------------------
                   ldi XL, low(RAM_addr_1)
                   ldi XH, high(RAM_addr_1)  ;use register pair X as pointer
                   ldi gps_count, 0x00       ;reset gps_count to 0
                   ldi temp_1, 0x00          ;reset temp_1 flag
     GPS_GMT_time: cpi temp_1, 0x00
                   breq GPS_GMT_time         ;wait for next byte to be received
                   st X+, gps_in             ;store indirect GMT time as (hr)(min)(sec).(ms)
                   inc gps_count             ;gps_count=1,..10
                   mov temp, gps_count       ;copy gps_count into temp
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi temp, 0x0A            ;check for gps_count=10
                   breq GPS_comma_2          ;all time bytes received, continue
                   jmp GPS_GMT_time          ;not equal indicates next byte received, store it

      GPS_comma_2: cpi temp_1, 0x00
                   breq GPS_comma_2          ;wait for next byte to be received
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_2          ;check for comma
                   ldi temp_1, 0x42          ;load character B
                   st X, temp_1              ;store it to indicate end of required data

;----GPS_latitude------------------------------
                   ldi XL, low(RAM_addr_2)
                   ldi XH, high(RAM_addr_2)  ;use register pair X as pointer
                   ldi gps_count, 0x00       ;reset gps_count to 0
                   ldi temp_1, 0x00          ;reset temp_1 flag
     GPS_latitude: cpi temp_1, 0x00
                   breq GPS_latitude         ;wait for next byte to be received
                   st X+, gps_in             ;store indirect Latitude as (degree)(minutes).(sec),N/S
                   inc gps_count             ;gps_count=1,..9
                   mov temp, gps_count       ;copy gps_count into temp
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi temp, 0x09            ;check for gps_count=9
                   breq GPS_comma_3          ;all latitude bytes received, continue
                   jmp GPS_latitude          ;not equal indicates next byte received, store it

      GPS_comma_3: cpi temp_1, 0x00
                   breq GPS_comma_3          ;wait for next byte to be received
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_3          ;check for comma

                   ldi temp_1, 0x00          ;reset temp_1 flag
           GPS_NS: cpi temp_1, 0x00
                   breq GPS_NS               ;wait for next byte to be received
                   st X+, gps_in             ;store N/S

                   ldi temp_1, 0x00          ;reset temp_1 flag
      GPS_comma_4: cpi temp_1, 0x00
                   breq GPS_comma_4          ;wait for next byte to be received
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_4          ;check for comma
                   ldi temp_1, 0x42          ;load character B
                   st X, temp_1              ;store it to indicate end of required data

;----GPS_longitude-----------------------------
                   ldi XL, low(RAM_addr_3)
                   ldi XH, high(RAM_addr_3)  ;use register pair X as pointer
                   ldi gps_count, 0x00       ;reset gps_count to 0
                   ldi temp_1, 0x00          ;reset temp_1 flag
    GPS_longitude: cpi temp_1, 0x00
                   breq GPS_longitude        ;wait for next byte to be received
                   st X+, gps_in             ;store indirect Longitude as (degree)(minutes).(sec),E/W
                   inc gps_count             ;gps_count=1,..10
                   mov temp, gps_count       ;copy gps_count into temp
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi temp, 0x0A            ;check for gps_count=10
                   breq GPS_comma_5          ;all longitude bytes received, continue
                   jmp GPS_longitude         ;not equal indicates next byte received, store it

      GPS_comma_5: cpi temp_1, 0x00
                   breq GPS_comma_5          ;wait for next byte to be received
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_5          ;check for comma

                   ldi temp_1, 0x00          ;reset temp_1 flag
           GPS_EW: cpi temp_1, 0x00
                   breq GPS_EW               ;wait for next byte to be received
                   st X+, gps_in             ;store E/W

                   ldi temp_1, 0x00          ;reset temp_1 flag
      GPS_comma_6: cpi temp_1, 0x00
                   breq GPS_comma_6          ;wait for next byte to be received
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_6          ;check for comma
                   ldi temp_1, 0x42          ;load character A
                   st X, temp_1              ;store it to indicate end of required data

;----GPS_valid_data----------------------------
                   ldi XL, low(RAM_addr_4)
                   ldi XH, high(RAM_addr_4)  ;use register pair X as pointer
                   ldi gps_count, 0x00       ;reset gps_count to 0
                   ldi temp_1, 0x00          ;reset temp_1 flag
   GPS_valid_data: cpi temp_1, 0x00
                   breq GPS_valid_data       ;wait for next byte to be received
                   st X+, gps_in             ;1= valid data

                   ldi temp_1, 0x00          ;reset temp_1 flag
      GPS_comma_7: cpi temp_1, 0x00
                   breq GPS_comma_7          ;wait for next byte to be received
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_7          ;check for comma

;----GPS_sat_viewed----------------------------
                   ldi temp_1, 0x00          ;reset temp_1 flag
   GPS_sat_viewed: cpi temp_1, 0x00
                   breq GPS_sat_viewed       ;wait for next byte to be received
                   st X+, gps_in             ;Number of satellites currently viewed
                   inc gps_count             ;gps_count=1,2
                   mov temp, gps_count       ;copy gps_count into temp
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi temp, 0x02            ;check for gps_count=3
                   breq GPS_comma_8          ;all sat_viewed bytes received, continue
                   jmp GPS_sat_viewed        ;not equal indicates next byte received, store it

      GPS_comma_8: cpi temp_1, 0x00
                   breq GPS_comma_8          ;wait for next byte to be received
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_8          ;check for comma

;----GPS_hdop----------------------------------
                   ldi gps_count, 0x00       ;reset gps_count to 0
                   ldi temp_1, 0x00          ;reset temp_1 flag
         GPS_hdop: cpi temp_1, 0x00
                   breq GPS_hdop             ;wait for next byte to be received
                   st X+, gps_in             ;HDOP
                   inc gps_count             ;gps_count=1,2,3
                   mov temp, gps_count       ;copy gps_count into temp
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi temp, 0x03            ;check for gps_count=3
                   breq GPS_comma_9          ;all hdop bytes received, continue
                   jmp GPS_hdop              ;not equal indicates next byte received, store it

      GPS_comma_9: cpi temp_1, 0x00
                   breq GPS_comma_9          ;wait for next byte to be received
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_9          ;check for comma
                   ldi temp_1, 0x42          ;load character B
                   st X, temp_1              ;store it to indicate end of required data

;----GPS_altitude------------------------------ 
                   ldi XL, low(RAM_addr_5)
                   ldi XH, high(RAM_addr_5)  ;use register pair X as pointer
                   ldi gps_count, 0x00       ;reset gps_count to 0
                   ldi temp_1, 0x00          ;reset temp_1 flag
     GPS_altitude: cpi temp_1, 0x00
                   breq GPS_altitude         ;wait for next byte to be received
                   st X+, gps_in             ;Altitude as (value).(value),M
           ;(Height above sea level in meter)
                   inc gps_count             ;gps_count=1,...4
                   mov temp, gps_count       ;copy gps_count into temp
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi temp, 0x04            ;check for gps_count=4
                   breq GPS_comma_10         ;all altitude bytes received, continue
                   jmp GPS_altitude          ;not equal indicates next byte received, store it

     GPS_comma_10: cpi temp_1, 0x00
                   breq GPS_comma_10         ;wait for next byte to be received
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi gps_in, 0x2C          ;compare gps_in with ASCII value of comma
                   brne GPS_comma_10         ;check for comma

                   ldi temp_1, 0x00          ;reset temp_1 flag
            GPS_M: cpi temp_1, 0x00
                   breq GPS_M                ;wait for next byte to be received
                   st X+, gps_in             ;store M
                   ldi temp_1, 0x42          ;load character B
                   st X, temp_1              ;store it to indicate end of required data

;----GPS_asterisk------------------------------
                   ldi temp_1, 0x00          ;reset temp_1 flag
     GPS_asterisk: cpi temp_1, 0x00
                   breq GPS_asterisk         ;wait for next byte to be received
                   ldi temp_1, 0x00          ;reset temp_1 flag
                   cpi gps_in, 0x2A          ;compare gps_in with ASCII value of asterisk
                   brne GPS_asterisk         ;wait for asterisk
                   cbi UCSRB, RXEN           ;disable USART Receiver
                   ret
;-------------------------------------------------  
;---------------- GPS Specific Sub-Routines ---------------------




;**********************************************************
;Interrupt Service Specific Routines
;**********************************************************
;------------USART : RXC USART interrupt Service Routine
        USART_RXC: in gps_in, UDR                 ;save contents of received data into gps_in
                   ldi temp_1, 0xFF               ;use temp_1 as flag to indicate next interrupt           
                   reti                           ;USART RX Complete Handler
;-------------------------------------------------


;---------------- End of Interrupt Service Specific Routines -------------------------------




;**********************************************************
;Data Format Conversions Specific Sub-Routines
;**********************************************************
;--------------- ASCII digits to BCD digits sub-routine
  ASCII_BCD_convrt: andi temp, 0x0F               ;mask upper nibble 
                    ret
;------------------------------------------------- 

;--------------- BCD digits to hex sub-routine
   BCD_hex_convrt: ldi key_in, 0x0A
                   mul temp_1, key_in            ;multiply by 10
				   mov temp_1, r0
				   adc temp, temp_1              ;add to LSB
				   
                   ret
;------------------------------------------------- 
;---------------- End of Data Format Conversions Specific Sub-Routines ---------------------




;**********************************************************
;Delay Specific Sub-Routines
;**********************************************************
;--------------- delay1 sub-routine: : A  51.201 ms delay1
		   delay1: ldi r19, 2
            here3: ldi r20, 200                ;R20=200
		    here2: ldi R21, 255                ;R21=255
		    here1: dec r21                     ;dec R21
                   cpi r21, 0
                   brne here1                  ;stay until R21 becomes 0
		           dec r20
                   cpi r20, 0
                   brne here2                  ;stay until R20 becomes 0
                   dec r19
                   cpi r19, 0
                   brne here3                  ;stay until R19 becomes 0
			       ret
;-------------------------------------------------


;--------------- delay2 sub-routine: : A 895 us delay2
          delay2: push r20
                  push r21
                  ldi r20, 20
          ld_hi:  ldi r21, 10
         loop_in: dec r21
                  cpi r21, 0
                  brne loop_in
                  dec r20
                  cpi r20, 0
                  brne ld_hi
                  pop r21
                  pop r20
                  ret
;-------------------------------------------------


;--------------- delay3 sub-routine: : A 107 us delay3
          delay3: push r20
                  ldi r20, 25
       more_call: dec r20
                  cpi r20, 0
                  brne more_call
                  pop r20
                  ret
;-------------------------------------------------


;--------------- delay4 sub-routine: : A 1.0075 ms delay4
         delay4: ldi r20, 250
       more_big: dec r20
                 cpi r20, 0
                 brne more_big
                 ret
;------------------------------------------------- 


;--------------- delay_20ms sub-routine: : A 20.100ms ms delay
     delay_20ms: ldi r21, 200
       d_20ms_1: ldi r20, 200
       d_20ms_2: dec r20
                 cpi r20, 0
                 brne d_20ms_2
                 dec r21
                 cpi r21, 0
                 brne d_20ms_1
                 ret
;-------------------------------------------------  

;--------------- delay_1sec sub-routine: : A  1 s delay
	   delay_1sec: ldi r19, 200
           d_1s_1: ldi r20, 50                 ;R20=50
		   d_1s_2: ldi R21, 255                ;R21=255
		   d_1s_3: dec r21                     ;dec R21
                   cpi r21, 0
                   brne d_1s_3                 ;stay until R21 becomes 0
		           dec r20
                   cpi r20, 0
                   brne d_1s_2                 ;stay until R20 becomes 0
                   dec r19
                   cpi r19, 0
                   brne d_1s_1                 ;stay until R19 becomes 0
			       ret
;-------------------------------------------------    

;---------------- End of delay Specific Sub-Routines --------------------------------------
 



;**********************************************************
;Look Up Tables (LUT's)
;**********************************************************
			       .org 0x0300                   ;store data at locations starting from 012Ch
;---------ASCII	Look-Up Table for each Row of the keypad(4x4)
         row0_LUT: .db "A", 0, "3", 0, "2", 0, "1", 0				;Row 0
         row1_LUT: .db "B", 0, "6", 0, "5", 0, "4", 0				;Row 1
         row2_LUT: .db "C", 0, "9", 0, "8", 0, "7", 0				;Row 2
         row3_LUT: .db "D", 0, "#", 0, "0", 0, "*", 0				;Row 3
;---------Look Up Tables for Command and Data for LCD (16X2)
      command_LUT: .db 0x38, 0x0E, 0x01, 0x06, 0x80, 0     ;commands and null
        text_LUT1: .db "GPS Fence Sys", 0
		text_LUT2: .db "Enter Fence: ", 0
		text_LUT3: .db "Success! ", 0 
;---------End of LUTs
