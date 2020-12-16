// memfile.dat
// david_harris@hmc.edu and sarah_harris@hmc.edu 20 December 2013
// Test ARM processor
// ADD, SUB, AND, ORR, LDR, STR, B
// If successful, it should write the value 7 to address 100

// MAIN	      SUB    R15, R0,           R15 	          		; R0 = 0				
	1110 000 0010 0 1111 0000 0000 0000 1111 E04F000F 0x00
//  		 ADD          R2,  R0,      #5      				; R2 = 5             
	1110 001 0100 0 0000 0010 0000 0000 0101 E2802005 0x04
//  		 ADD  		  R3,  R0,      #12    					; R3 = 12 = 0xC        
	1110 001 0100 0 0000 0011 0000 0000 1100 E280300C 0x08
//  		 SUB     R3,  R7,            #9    					; R7 = 3             
	1110 001 0010 0 0011 0111 0000 0000 1001 E2437009 0x0c


//  		 ORR R4, R7, R2    	; R4 = 3 OR 5 = 7              	
    1110 000 1100 0 0111 0100 0000 0000 0010 E1874002 0x10
//  		 AND     R3,  R5             R4    					; R5 = 12 AND 7 = 4            	
    1110 000 0000 0 0011 0101 0000 0000 0100 E0035004 0x14
//  		 ADD 	 R5,  R5,            R4    					; R5 = 4 + 7 = 11              	
    1110 000 0100 0 0101 0101 0000 0000 0100 E0855004 0x18
//  		 SUBS    R5,  R8,            R7						; R8 <= 11 - 3 = 8, set Flags   	
    1110 000 0010 1 0101 1000 0000 0000 0111 E0558007 0x1c


//        BEQ END        										; shouldn't be taken            	
	0000 1010 0000  0000 0000 0000 0000 1100 0A00000C 0x20
//  		 SUBS    R3,  R8,            R4    					; R8 = 12 - 7  = 5             	
	1110 000 0010 1 0011 1000 0000 0000 0100 E0538004 0x24
//  	  BGE AROUND       										; should be taken               	
	1010 1010 0000  0000 0000 0000 0000 0000 AA000000 0x28


//  		 ADD 		  R5,  R0,       #0     				; should be skipped             	
	1110 001 0100 0 0000 0101 0000 0000 0000 E2805000 0x2c
// AROUND    SUBS 	 R7,  R8,            R2   					; R8 = 3 - 5 = -2, set Flags   	
	1110 000 0010 1 0111 1000 0000 0000 0010 E0578002 0x30
//           ADDLT   R5,  R7, #1  	; R7 = 11 + 1 = 12				
	1011 001 0100 0 0101 0111 0000 0000 0001 B2857001 0x34
//           SUB     R7,  R7,            R2    					; R7 = 12 - 5 = 7				
	1110 000 0010 0 0111 0111 0000 0000 0010 E0477002 0x38

//            STR R7, [R3, #84]  								; mem[12+84] = 7	
//			  STR    R3   R7	   5*16   4  	
	1110 010 1100 0 0011 0111 0000 0101 0100 E5837054 0x3c


//            LDR R2, [R0, #96]  								; R2 = mem[96] = 7
//			  LDR    R0   R2       6*16  0			
	1110 010 1100 1 0000 0010 0000 0110 0000 E5902060 0x40


//           ADD    R15, R15,            R0						; PC <- PC + 8 (skips next)     	
	1110 000 0100 0 1111 1111 0000 0000 0000 E08FF000 0x44
//           ADD    R0,  R2,            #14    					; shouldn't happen              	
	1110 001 0100 0 0000 0010 0000 0000 1110 E280200E 0x48

//          B END             									; always taken					
	1110 1010 0000 0000 0000 0000 0000 0001  EA000001 0x4c
//           ADD    R0,  R2,            #13   					; shouldn't happen				
	1110 001 0100 0 0000 0010 0000 0000 1001 E280200D 0x50
//           ADD    R2,  R0,            #10						; shouldn't happen				
	1110 001 0100 0 0000 0010 0000 0000 1010 E280200A 0x54

//  END      STR R2, [R0, #100] 	; mem[100] = 7            
//           STR     R0   R2       6*16  4        	
	1110 010 1100 0 0000 0010 0000 0101 0100 E5802064 0x58

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