onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -radix hexadecimal /testbench/clk
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/PC
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/PCNext1
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/PCNext2
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/PCPlus4F
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/PCPlus8D
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/instrD
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/instrF
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
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUControlD
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUControlE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUSrcD
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUSrcE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/ALUFlags
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/BranchTaken
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/Cond
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/CondEx
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/FlagsE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/FlagsLinha
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux2/d0
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux2/d1
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux2/s
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux2/y
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux4/d0
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux4/d1
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux4/d2
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux4/d3
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux4/s
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcBmux4/y
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ImmSrc
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ALUControlE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ALUFlags
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/ALUSrcE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/BranchTakenE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcAmux4/d0
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcAmux4/d1
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcAmux4/d2
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcAmux4/d3
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcAmux4/s
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/srcAmux4/y
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {125000 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 322
configure wave -valuecolwidth 100
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
WaveRestoreZoom {281484 ps} {379922 ps}
