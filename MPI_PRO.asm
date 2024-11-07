ECHO EQU P1.0;DEFINITION OF ULTRASONIC RECIEVE
TRIG EQU P1.1;DEFINITION OF ULTRASONIC CONTROL
BUZZER EQU P2.5;DEFINITION OF BUZZER CONTROL,OLD ONE IS P1.2
SPK EQU P2.5;ANOTHER DEFINITION OF BUZZER TO ADAPT LCD INSTRUCT
LED1 EQU P2.0
LED2 EQU P2.1
LED3 EQU P2.2
checkled EQU P2.3
KEY1 EQU P3.1
RS BIT P2.6;old one is P2.4
RW BIT P2.5
EN BIT P2.7;OLD ONE IS P2.6
X  EQU 36H;ADDRESS OF LCD
TLOW   EQU  3cH 
THIGH  EQU  0b0H
CASE_CUR data 0;DEFINATION

HOUR   EQU  30H;TO BE SET
MIN    EQU  31H
SEC    EQU  32H
SEC0   EQU  33H
	
FREQ EQU 34H
	
ORG 0000H
	LJMP MAIN;TO MAIN FOUNCTION
ORG 0003H
	RETI
ORG 000BH
	LJMP T0INT
ORG 0013H
	RETI
ORG 001BH
	LJMP T1INT;TIM1 INTERRUPTION CALLBACK
ORG 0023H
	RETI
	
	ORG 0030H
MAIN:	CLR TRIG
		MOV TMOD,#11H;TIM0 AND TIM1 
		MOV TH0,#00H;FROM 00000000
		MOV TL0,#00H;FROM 00000000
		MOV IE, #8AH;ENABLE INTERRUPTION OF TIM0 AND TIM1
		MOV R3,#00H;SET A FLAG OF THE RANGE
		MOV  SP,#60H
		;LCD INIT
		CLR  EN
		CALL  SET_LCD
		CALL  INIT            
		CALL  INIT_TIMER      
		CALL   CONV 
		SETB LED1
		SETB LED2
		SETB LED3
		SETB checkled
		;LCD INIT END
		START:CALL MENU
		LCALL ULTRARANG;JUMP TO ULTRESONIC RANGING
		LCALL CHECKRANGE;JUMP TO CHECK THE RANGE
		SJMP START
		
		
ULTRARANG:LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
		  LCALL DELAY_1MS
          ;CLR F0;SET A FLAG
		  SETB TRIG;TRANSMIT SIGNAL
          NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP
		  NOP;FUNCTION OF DELAY 10US
		  CLR TRIG;STOP TRANSMITTING
		  JNB ECHO,$;CHECK IF RECIEVE THE SIGNAL
		  SETB TR0;TURN ON THE TIMER IF RECIEVE
		  CLR TF0;
		  JB ECHO,$;CHECK WHETHER SIGNAL RECEPTION IS OVER
		  CLR TR0;STOP RUNNING OF TIMER
		  ;JB F0,TOEXIT;TO EXIT
		  MOV R6,TH0;SAVE RANGE HIGH
		  MOV R7,TL0;SAVE RANGE LOW
		  ;MOV DPTR,#58;TO CALCULATE
		  MOV R5,#3AH;CHANGE REGISTER TO CALL THE FOUNCTION
		  MOV R4,#00H;CHANGE REGISTER TO CALL THE FOUNCTION
		  
		  LCALL UIDIV;CALL THE DIVISION FOUNCTION
		  MOV B,R6
		  MOV A,R7
		  MOV TL0,#00H
		  MOV TH0,#00H
		  RET
		  
		  
CHECKRANGE:CLR C;
		   MOV B,A;
		   MOV R1,A
           SUBB A,#10
           JC CASE1
		   CLR C;
		   MOV A,B;
		   MOV R1,A;
           SUBB A,#20
           JC CASE2
		   CLR C;
		   MOV A,B;
		   MOV r1,A;
           SUBB A,#30
           JC CASE3
		   JNC CASE0
		   RET
		   
		   CASE0:MOV A,CASE_CUR
				 CJNE A,#0,CASE0_SET
				 RET
				 CASE0_SET: SETB LED1
							SETB LED2
							SETB LED3
							SETB checkled
							MOV CASE_CUR,#0
							RET
		   CASE1:MOV A,CASE_CUR
				 CJNE A,#1,CASE1_SET
				 RET
				 CASE1_SET: CLR LED1
							SETB LED2
							SETB LED3
							SETB checkled
							MOV CASE_CUR,#1
							LCALL M_READY
							LCALL MUS1
							RET
		   CASE2:MOV A,CASE_CUR
				 CJNE A,#2,CASE2_SET
				 RET
				 CASE2_SET: SETB LED1
							CLR LED2
							SETB LED3
							SETB checkled
							MOV CASE_CUR,#2
							LCALL M_READY
							LCALL MUS2
							RET
		   CASE3:MOV A,CASE_CUR
				 CJNE A,#3,CASE3_SET
				 RET
				 CASE3_SET: SETB LED1
							SETB LED2
							CLR LED3
							SETB checkled
							MOV CASE_CUR,#3
							LCALL M_READY
							LCALL MUS3
							RET
							
			M_READY:MOV TMOD,#11H
					SETB EA;AS YOU SEE,NOW ULT DOES NOT WORK
					SETB ET1;LCD TIMER
					SETB ET0;BUZZER TIMER
					SETB TR1;ENABLE LCD TIMER
					RET
					
			MUS1:   SETB TR0
					MOV R4 ,#18
        MUS1LOOP:	INC FREQ
					LCALL DELAY_1MS
					INC FREQ
					JNB KEY1, STOP1 
					DJNZ R4, MUS1LOOP
					LJMP MUS1
					STOP1:LJMP STOP
				 
			MUS2:	SETB TR0
					MOV sec0,#20
		MUS2LOOP:	MOV FREQ,#0
					LCALL DELAY_100_MS
					JNB KEY1, STOP1
					MOV FREQ,#100
					LCALL DELAY_100_MS
					JNB KEY1, STOP1
					LJMP MUS2LOOP
					STOP2:LJMP STOP
				  
			MUS3: 	SETB TR0
		   MUS3LOOP:MOV R0,#0
		LOOP_music0:INC R0   
					MOV FREQ,R0
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					JNB KEY1, STOP
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					JNB KEY1, STOP
					CJNE R0, #100, LOOP_music0 
					LOOP_music1:
					MOV FREQ,r0
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					JNB KEY1, STOP
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					JNB KEY1, STOP  
					DJNZ R0, LOOP_music1
					LJMP MUS3
				  
			STOP:   LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS

					JB KEY1,STOP;SHAKING CHECK IN CASE IT IS A MISTAKE
					SETB LED1
					SETB LED2
					SETB LED3
					CLR EA
					CLR TR1
					CLR ET1
					CLR TR0
					CLR ET0
					CLR SPK	
					CLR checkled
					MOV  SEC0,#0;ABOUT LCD
					MOV  SEC,#0
					MOV  MIN,#0
					MOV  HOUR,#0
					LCALL MENU1
					LCALL CONV
					
					WAIT:	
					JB KEY1,$
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS

					JNB KEY1, WAIT
			;STOP ABOVE
			CHECK:	JNB KEY1,RESTART;TO RESTART
					AJMP CHECK
			RESTART:SETB checkled
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					JB KEY1,RESTART
					start_song:
					JB KEY1,$	
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					LCALL DELAY_1MS
					JNB KEY1, start_song
					LJMP START
		  
		  T1INT:CLR EA         ;INTERRUPTION OF TIMER1 WHICH WORKS FOR BUZZER
				MOV TH1,#0FEH;	
				MOV TL1,FREQ;   
				CPL BUZZER;      
				SETB EA
				RETI
		  
		  
DELAY_10US:	MOV R7,#10;SET COUNTER
			DJNZ R7,$;REPEAT TO GO THROUGH MACHINE CYCLE
			RET;BACK
			
DELAY_1MS:MOV R3,#250
	LOOP2:NOP
		  NOP
		  NOP
		  NOP
	  	  DJNZ R3,LOOP2
		  RET
		  
UIDIV:		CJNE     R4,#00H,DIV_H1			      
			CJNE     R6,#00H,DIV_H2			    
ALLDIV_L:	MOV      A,R7					
			MOV      B,R5
			DIV      AB
			MOV      R7,A
			MOV      R5,B
			RET				
DIV_H1:		CLR      A						
			XCH      A,R4		
			MOV      R0,A		
			MOV      B,#08H		
UIDIV_LOOP1:MOV      A,R7					
			ADD      A,R7					
			MOV      R7,A					
			MOV      A,R6							
			RLC      A									
			MOV      R6,A					
			MOV      A,R4										
			RLC      A									
			MOV      R4,A			
			
			MOV      A,R6									
			SUBB     A,R5														
			MOV      A,R4														
			SUBB     A,R0						
			JC       R4_HIGH			
R4_EQU_LOW:	MOV      R4,A													
			MOV      A,R6											
			SUBB     A,R5											
			MOV      R6,A											
			INC      R7										
R4_HIGH:	DJNZ     B,UIDIV_LOOP1			
			CLR      A
			XCH      A,R6				
			MOV      R5,A		
			RET      
DIV_H2:		MOV      A,R5
			MOV      R0,A		
			MOV      B,A	
			MOV      A,R6					
			DIV      AB			
			JB       OV,UIDIV_END
			MOV      R6,A					
			MOV      R5,B		
			MOV      B,#08H		
UIDIV_LOOP2:MOV      A,R7
			ADD      A,R7		
			MOV      R7,A		
			MOV      A,R5
			RLC      A
			MOV      R5,A		
			JC       UIDIV_NEXT
			SUBB     A,R0				
			JNC      RE_EQU_LOW			
RE_HIGH:	DJNZ     B,UIDIV_LOOP2			
			RET     
UIDIV_NEXT:	CLR      C
			SUBB     A,R0				
RE_EQU_LOW:	MOV      R5,A				
			INC      R7					
			DJNZ     B,UIDIV_LOOP2		
UIDIV_END:	RET

;----------------------------------------------------
;GREY AREA
T0INT:	 CLR EA           ;INTERRUPTION OF TIMER0 WHICH IS WORK FOR LCD
		 PUSH  ACC           
         MOV  TL0,#0b0h
         MOV  TH0,#3ch
		 INC sec0
		 MOV a,sec0
		 CJNE a,#30,tt1
		 MOV sec0,#0
         INC  SEC            ;SECOND
         MOV  A,SEC
         CJNE  A,#60,TT
         INC  MIN            ;MINUTE
         MOV  SEC,#0
         MOV  A,MIN
         CJNE  A,#60,TT
         INC  HOUR           ;HOUR
         MOV  MIN,#0
         MOV  A,HOUR
         CJNE  A,#24,TT
         MOV  SEC0,#0
         MOV  SEC,#0          ;CLC
         MOV  MIN,#0
         MOV  HOUR,#0
		 
 TT:     
		CALL   CONV
tt1:	POP  ACC
		SETB EA
         RETI

;----------------------------------------------------
;ABOUT LCD 
CONV:                  ;TO SET TIME DATA LIKE HOUR, MINUTE AND SECONG
          MOV  A,HOUR
          MOV  X,#8
          CALL  SHOW_DIG2
          INC  X
          MOV  A,#':'
          MOV  B,X 
          CALL  LCDP2 
          MOV  A,MIN     
          INC  X       
          CALL  SHOW_DIG2
          INC  X           
          MOV  A,#':'        
          MOV  B,X          
          CALL  LCDP2        
          MOV  A,SEC      
          INC  X           
          CALL  SHOW_DIG2   
          RET

SHOW_DIG2:            ;TO SHOW THE DIGIT ON LINE 2 OF LCD
          MOV  B,#10    
          DIV  AB     
          ADD  A,#30H      
          PUSH  B         
          MOV  B,X       
          CALL  LCDP2   
          POP  B         
          MOV  A,B    
          ADD  A,#30H     
          INC  X           
          MOV  B,X      
          CALL  LCDP2
          RET

LCDP2:                   ;TO SHOW THE CHARACTER ON LINE 2 OF LCD
         PUSH  ACC      
         MOV  A,B      
         ADD  A,#0C0H   
         CALL  WCOM     
         POP  ACC    
         CALL  WDATA    
         RET
		 
WDATA:                ;WRITE DATA TO LCD
          MOV  P0,A  
          SETB  RS
          CALL  EN1
          RET

EN1:                  ;LCD ENABLE
          CLR   RW
          SETB  EN    
          CALL  DE
          CLR  EN
          CALL  DE
          RET

DE:    	  MOV  R7,#250    ;TO DELAY 500US BY R7
          DJNZ  R7,$
          RET

WCOM:                     ;WRITE COMMAND TO LCD
          MOV  P0,A      
          call  enable
          RET

ENABLE:                       ;TO WRITE INSTRUCT
          CLR RS         
          CLR RW          
          SETB EN
          ACALL DELAY1          
          CLR EN
          RET

DELAY1:                    ;FIVE MINUTES TO DELAY
         MOV  R6,#25
  D2:    MOV  R7,#100
         DJNZ  R7,$
         DJNZ  R6,D2
         RET

MENU:                      ;LCD SHOW
         MOV  DPTR,#MMENU
         MOV  A,#1
         CALL  LCD_PRINT
         RET

MENU1:                      ;SHOW THE MENU OF LCD
         MOV  DPTR,#MMENU1
         MOV  A,#1
         CALL  LCD_PRINT
         RET

LCD_PRINT:       ;SHOW CHARACTER ON DIFFERENT LINE OG LCD

          CJNE  A,#1,LINE2
  LINE1:  MOV  A,#80H   
          CALL  WCOM     
          CALL  CLR_LINE  
          MOV  A,#80H    
          CALL  WCOM      
          JMP  FILL
  LINE2:  MOV  A,#0C0H    
          CALL  WCOM     
          CALL  CLR_LINE   
          MOV  A,#0C0H    
          CALL  WCOM
  FILL:   CLR  A          
          MOVC  A,@A+DPTR  
          CJNE  A,#0,LC1   
          RET
  LC1:    CALL  WDATA      
          INC  DPTR      
          JMP  FILL      
          RET

CLR_LINE:                  ;CLEAR OR DELETE ALL THE CHARACTER ON THIS LINE
          MOV  R0,#24
   CL1:   MOV  A,#' '
          CALL  WDATA
          DJNZ  R0,CL1
          RET


;INIT OF LCD AND ELSE
INIT_LCD:                  ;LCD INIT
          MOV  P0,#38H    
          call  enable
          call  delay1
          MOV  P0,#38H    
          call  enable
          call  delay1
          MOV  P0,#38H
          call  enable
          call  delay1
          CALL  INIT_LCD1
          RET

INIT_LCD1:                   ;INIT OF CONTROL INSTRUCT OF LCD
          MOV  A,#38H    
          CALL  WCOM       
          call  delay1
          MOV  A,#0CH     
          CALL  WCOM      
          call  delay1
          MOV  A,#01H     
          CALL  WCOM         
          call  delay1
          RET

SET_LCD:                     ;LCD INIT AND TEST
          CLR  EN
          CALL  INIT_LCD  
          MOV  R5,#10
          CALL  DELAY
          MOV  DPTR,#LMESS1 
          MOV  A,#1         
          CALL  LCD_PRINT
          MOV  DPTR,#LMESS2 
          MOV  A,#2        
          CALL  LCD_PRINT
          RET
		  
DELAY:                    ;10MS FOR DELAY
         MOV  R6,#50
  D1:    MOV  R7,#100
         DJNZ  R7,$
         DJNZ  R6,D1
         DJNZ  R5,DELAY
         RET
		 
DELAY_100_MS:
ddel: mov r7,#50
ddel1:mov r6,#200
ddel2:mov r5,#10
ddel3:djnz r5,ddel3
	djnz r6,ddel2
	djnz r7,ddel1

    RET

INIT_TIMER:                     ;TIMER INIT
         MOV  TMOD,#11H        
SETB EA
SETB ET0;?????0 ????
         MOV  TL0,#TLOW
         MOV  TH0,#THIGH
         RET

INIT:    CLR  A        ;TIME SHOW INIT
         MOV  SEC0,A
         MOV  SEC,A
         MOV  MIN,A
         MOV  HOUR,A
         CLR  TR0
         RET

;TO BE USED TO SHOW CHARACTER ON LCD
MMENU:  DB  "B21 SHU SCIE MPI",0
MMENU1:  DB  "B21 SHU SCIE MPI",0

;TEST BYTES
LMESS1:  DB  "                ",0  ;LCD LINE1
LMESS2:  DB  "TIME            ",0  ;LCD LINE2


END