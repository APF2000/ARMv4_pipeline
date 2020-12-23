R0=0; R1= N; R2=ant; R3=prox; R4=aux; R5=i; R6=Naux R14=return

E04F000F SUB R0, R15, R15 	; R0 = 0  0x00
E2801005 ADD R1, R0, #5      	; R1 = 5 calcular 5o fibo 0x04
EA000004 B FIBO ; 0x08
EA000000 BACK B BACK ; esquece 0x0C
00000000
00000000
00000000
00000000
E2802001 ADD R2, R0, #1 ; 0x20
E2803001 ADD R3, R0, #1 ; 0x24
E2805000 ADD R5, R0, #0 ; 0x28
E2816000 ADD R6, R1, #0 ; 0x2C
E2511002  SUBS R1, R1, #1 ; 0x30
AA000000 BGE LOOP ; 0x34
EA000006 B END ; 0x38
E0416005 LOOP SUB R6, R1, R5 ; Naux = N - i 0x3C
E2466001 SUBS R6, R6, #1 ; Naux = (N - 1) - i 0x40
AA000000 BGE CONTINUE ; 0x44
EA000004 B END ; 0x48
E0824003 CONTINUE ADD R4, R2, R3 ; aux = ant + prox 0x4C
E2832000 ADD R2, R3, #0 ; ant = prox ; 0x50
E2843000 ADD R3, R4, #0 ; prox = aux ; 0x54
E2855001 ADD R5, R5, #1 ; i++ ; 0x58
EA B LOOP ; 0x5C
END ADD R14, R5, #0 ; 0x60
B BACK ; 0x64

------------------------------
ADDI   = E28 R1op Rdest 00X
ADD    = E08 R1op Rdest 00 R2op
ADDS   = E09 R1op Rdest 00 R2op
ADDSI  = E29 R1op Rdest 00X
BGE    = AA pulo (pc at + pulo * 4 + 8)
BEQ    = 0A pulo (pc at + pulo * 4 + 8)
SUBI   = E24 R1op Rdest 00X
SUB    = E04 R1op Rdest 00 R2op
SUBS   = E04 R1op Rdest 00 R2op
B      = EA pulo (pc at + pulo * 4 + 8)

E04F000F SUB R0, R15, R15 	; R0 = 0
E2802005 ADD R2, R0, #5      	; R2 = 5
E280300C ADD R3, R0, #12    	; R3 = 12
E2437009 SUB R7, R3, #9    	; R7 = 3
E1874002 ORR R4, R7, R2    	; R4 = 3 OR 5 = 7
E0035004 AND R5, R3, R4    	; R5 = 12 AND 7 = 4
E0855004 ADD R5, R5, R4    	; R5 = 4 + 7 = 11
E0558007 SUBS R8, R5, R7    	; R8 <= 11 - 3 = 8, set Flags
0A00000C BEQ END        		; shouldn't be taken
E0538004 SUBS R8, R3, R4    	; R8 = 12 - 7  = 5
AA000000 BGE AROUND
------------------------------

def fibo(n):   
    ant, prox = 1, 1

    if(n < 1):
        return 1

    for i in range(n):
        aux = ant + prox
        ant = prox
        prox = aux

    return prox

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