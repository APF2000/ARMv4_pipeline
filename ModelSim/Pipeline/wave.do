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
add wave -noupdate /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUControlD
add wave -noupdate /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUControlE
add wave -noupdate /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUSrcD
add wave -noupdate /testbench/dut/i_arm/datap/inst_partial_ID_EX/ALUSrcE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/ALUFlags
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/BranchTaken
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/Cond
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/CondEx
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/FlagsE
add wave -noupdate -radix hexadecimal /testbench/dut/i_arm/datap/cl/FlagsLinha
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {95000 ps} 0}
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
WaveRestoreZoom {0 ps} {367765 ps}
