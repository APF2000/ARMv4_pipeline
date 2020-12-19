onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -radix hexadecimal /testbench/clk
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/RA1D
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/RA2D
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r2
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r4
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ForwardAE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ForwardBE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ResultW
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ALUOutM
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ALUOutW
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/aluinst/a
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/aluinst/b
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/aluinst/Result
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/clk
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r0
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r1
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r2
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r3
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r4
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r5
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r6
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r7
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/db_r8
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/r15
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/ra1
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/ra2
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/rd1
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/rd2
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/wa3
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/wd3
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/rf/we3
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/clock
add wave -noupdate -color blue -radix hexadecimal /testbench/dut/i_arm/datap/hu/FlushD
add wave -noupdate -color blue -radix hexadecimal /testbench/dut/i_arm/datap/hu/FlushE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/ForwardAE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/ForwardBE
add wave -noupdate -color gold -radix hexadecimal /testbench/dut/i_arm/datap/hu/LDRStall
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/Match
add wave -noupdate -color gold -radix hexadecimal /testbench/dut/i_arm/datap/hu/BranchTakenE
add wave -noupdate -color gold -radix hexadecimal /testbench/dut/i_arm/datap/hu/PCWrPendingF
add wave -noupdate -color blue -radix hexadecimal /testbench/dut/i_arm/datap/hu/StallD
add wave -noupdate -color blue -radix hexadecimal /testbench/dut/i_arm/datap/hu/StallF
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/clock
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match_1E_M
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match_1E_W
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match_2E_M
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match_2E_W
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match_12D_E
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match_12D_Ea
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/Match_12D_Eb
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/PCSrcD
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/PCSrcE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/PCSrcM
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/PCWrPendingF
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/RA1D
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/RA1E
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/RA2D
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/RA2E
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/reset
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/WA3E
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/WA3M
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hl/WA3W
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/Match_1E_M
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/Match_1E_W
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/Match_2E_M
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/Match_2E_W
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/Match_12D_E
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/MemToRegE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/PCSrcW
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/RegWriteM
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/RegWriteW
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/hu/reset
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {125000 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 253
configure wave -valuecolwidth 71
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {387741 ps}
