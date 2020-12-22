E04F000F SUB R0, R15, R15 	; R0 = 0
E2801005 ADD R1, R0, #5      	; R1 = 5 calcular 5o fibo
EA000001 B FIBO

E280300C ADD R3, R0, #12    	; R3 = 12
E2437009 SUB R7, R3, #9    	; R7 = 3
E1874002 ORR R4, R7, R2    	; R4 = 3 OR 5 = 7
E0035004 AND R5, R3, R4    	; R5 = 12 AND 7 = 4
E0855004 ADD R5, R5, R4    	; R5 = 4 + 7 = 11
E0558007 SUBS R8, R5, R7    	; R8 <= 11 - 3 = 8, set Flags
0A00000C BEQ END        		; shouldn't be taken
E0538004 SUBS R8, R3, R4    	; R8 = 12 - 7  = 5
AA000000 BGE AROUND
E2805000 ADD 		  R5,  R0,       #0     				; should be skipped
E0578002 AROUND    SUBS 	 R7,  R8,            R2   					; R8 = 3 - 5 = -2, set Flags
B2857001 ADDLT   R5,  R7, #1  	; R7 = 11 + 1 = 12		
E0477002 SUB     R7,  R7,            R2    					; R7 = 12 - 5 = 7
E5837054 STR R7, [R3, #84]  								; mem[12+84] = 7	
E5902060 LDR R2, [R0, #96]  								; R2 = mem[96] = 7
E08FF000 ADD    R15, R15,            R0						; PC <- PC + 8 (skips next) 
E280200E ADD    R0,  R2,            #14    					; shouldn't happen   
EA000001 B END             									; always taken					
E280200D ADD    R0,  R2,            #13   					; shouldn't happen		
E280200A ADD    R2,  R0,            #10						; shouldn't happen	
E5802064 END      STR R2, [R0, #100] 	; mem[100] = 7 