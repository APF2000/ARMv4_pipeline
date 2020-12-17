library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;
entity testbench is
end;
architecture test of testbench is
 component top
 port(clk, reset: in STD_LOGIC;
 WriteDataM, DataAdrM: out STD_LOGIC_VECTOR(31 downto 0);
 MemWriteM: out STD_LOGIC);
 end component;
 signal WriteData, DataAdr: STD_LOGIC_VECTOR(31 downto 0);
 signal clk, reset, MemWrite: STD_LOGIC;
begin
 -- instantiate device to be tested
 dut: top port map(clk, reset, WriteData, DataAdr, MemWrite);
 -- Generate clock with 10 ns period
 process begin
 clk <= '1';
 wait for 5 ns;
 clk <= '0';
 wait for 5 ns;
 end process;
 -- Generate reset for first two clock cycles
 process begin
 reset <= '1';
 wait for 22 ns;
 reset <= '0';
 wait;
 end process;
 -- check that 7 gets written to address 84
 -- at end of program
 process (clk) begin
 if (clk'event and clk = '0' and MemWrite = '1') then
 if (to_integer(DataAdr) = 100 and
 to_integer(WriteData) = 7) then
 report "NO ERRORS: Simulation succeeded" severity failure;
 elsif (DataAdr /= 96) then
 report "Simulation failed" severity failure;
 end if;
 end if;
 end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all; 

entity top is -- top-level design for testing
 port(clk, reset: in STD_LOGIC;
 WriteDataM, DataAdrM: buffer STD_LOGIC_VECTOR(31 downto 0);
 MemWriteM: buffer STD_LOGIC);
end;
architecture test of top is
 component arm
 port(clk, reset: in STD_LOGIC;
 PCF: out STD_LOGIC_VECTOR(31 downto 0);
 InstrF: in STD_LOGIC_VECTOR(31 downto 0);
 MemWriteM: out STD_LOGIC;
 ALUOutM, WriteDataM: out STD_LOGIC_VECTOR(31 downto 0);
 ReadDataM: in STD_LOGIC_VECTOR(31 downto 0));
 end component;
 component imem
 port(a: in STD_LOGIC_VECTOR(31 downto 0);
 rd: out STD_LOGIC_VECTOR(31 downto 0));
 end component;
 component dmem
 port(clk, we: in STD_LOGIC;
 a, wd: in STD_LOGIC_VECTOR(31 downto 0);
 rd: out STD_LOGIC_VECTOR(31 downto 0));
 end component;
 signal PCF, InstrF, ReadDataM: STD_LOGIC_VECTOR(31 downto 0);
begin
 -- instantiate processor and memories
 i_arm: arm port map(clk, reset, PCF, InstrF, MemWriteM, DataAdrM,
 WriteDataM, ReadDataM);
 i_imem: imem port map(PCF, InstrF);
 i_dmem: dmem port map(clk, MemWriteM, DataAdrM, WriteDataM, ReadDataM);
end;
library IEEE;
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity imem is -- instruction memory
 port(a: in STD_LOGIC_VECTOR(31 downto 0);
 rd: out STD_LOGIC_VECTOR(31 downto 0));
end;
architecture behave of imem is -- instruction memory
begin
 process is
 file mem_file: TEXT;
 variable L: line;
 variable ch: character;
 variable i, index, result: integer;
 type ramtype is array (63 downto 0) of
 STD_LOGIC_VECTOR(31 downto 0);
 variable mem: ramtype;
 begin
 -- initialize memory from file
 for i in 0 to 63 loop -- set all contents low
 mem(i) := (others => '0'); 

 end loop;
 index := 0;
 FILE_OPEN(mem_file, "memfile.dat", READ_MODE);
 while not endfile(mem_file) loop
 readline(mem_file, L);
 result := 0;
 for i in 1 to 8 loop
 read(L, ch);
 if '0' <= ch and ch <= '9' then
 result := character'pos(ch) - character'pos('0');
 elsif 'a' <= ch and ch <= 'f' then
 result := character'pos(ch) - character'pos('a')+10;
 elsif 'A' <= ch and ch <= 'F' then
 result := character'pos(ch) - character'pos('A')+10;
 else report "Format error on line " & integer'image(index)
 severity error;
 end if;
 mem(index)(35-i*4 downto 32-i*4) :=
 to_std_logic_vector(result,4);
 end loop;
 index := index + 1;
 end loop;
 -- read memory
 loop
 rd <= mem(to_integer(a(7 downto 2)));
 wait on a;
 end loop;
 end process;
end;
library IEEE;
use IEEE.STD_LOGIC_1164.all; use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity dmem is -- data memory
 port(clk, we: in STD_LOGIC;
 a, wd: in STD_LOGIC_VECTOR(31 downto 0);
 rd: out STD_LOGIC_VECTOR(31 downto 0));
end;
architecture behave of dmem is
begin
 process is
 type ramtype is array (63 downto 0) of
 STD_LOGIC_VECTOR(31 downto 0);
 variable mem: ramtype;
 begin -- read or write memory
 loop
 if clk'event and clk = '1' then
 if (we = '1') then
 mem(to_integer(a(7 downto 2))) := wd;
 end if;
 end if;
 rd <= mem(to_integer(a(7 downto 2))); 

 wait on clk, a;
 end loop;
 end process;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity arm is -- pipelined processor
 port(clk, reset: in STD_LOGIC;
 PCF: out STD_LOGIC_VECTOR(31 downto 0);
 InstrF: in STD_LOGIC_VECTOR(31 downto 0);
 MemWriteM: out STD_LOGIC;
 ALUOutM, WriteDataM: out STD_LOGIC_VECTOR(31 downto 0);
 ReadDataM: in STD_LOGIC_VECTOR(31 downto 0));
end;
architecture struct of arm is
 component controller
 port(clk, reset: in STD_LOGIC;
 InstrD: in STD_LOGIC_VECTOR(31 downto 12);
 ALUFlagsE: in STD_LOGIC_VECTOR(3 downto 0);
 RegSrcD, ImmSrcD: out STD_LOGIC_VECTOR(1 downto 0);
 ALUSrcE: out STD_LOGIC;
 BranchTakenE: out STD_LOGIC;
 ALUControlE: out STD_LOGIC_VECTOR(1 downto 0);
 MemWriteM: out STD_LOGIC;
 MemtoRegW: out STD_LOGIC;
 PCSrcW: out STD_LOGIC;
 RegWriteW: out STD_LOGIC;
 -- hazard interface
 RegWriteM: out STD_LOGIC;
 MemtoRegE: out STD_LOGIC;
 PCWrPendingF: out STD_LOGIC;
 FlushE: in STD_LOGIC);
 end component;
 component datapath
 port(clk, reset: in STD_LOGIC;
 RegSrcD, ImmSrcD: in STD_LOGIC_VECTOR(1 downto 0);
 ALUSrcE: in STD_LOGIC;
 BranchTakenE: in STD_LOGIC;
 ALUControlE: in STD_LOGIC_VECTOR(1 downto 0);
 MemtoRegW: in STD_LOGIC;
 PCSrcW: in STD_LOGIC;
 RegWriteW: in STD_LOGIC;
 PCF: out STD_LOGIC_VECTOR(31 downto 0);
 InstrF: in STD_LOGIC_VECTOR(31 downto 0);
 InstrD: out STD_LOGIC_VECTOR(31 downto 0);
 ALUOutM: out STD_LOGIC_VECTOR(31 downto 0);
 WriteDataM: out STD_LOGIC_VECTOR(31 downto 0);
 ReadDataM: in STD_LOGIC_VECTOR(31 downto 0);
 ALUFlagsE: out STD_LOGIC_VECTOR(3 downto 0);
 -- hazard logic
 Match_1E_M: out STD_LOGIC;
 Match_1E_W: out STD_LOGIC;
 Match_2E_M: out STD_LOGIC; 

 Match_2E_W: out STD_LOGIC;
 Match_12D_E: out STD_LOGIC;
 ForwardAE: in STD_LOGIC_VECTOR(1 downto 0);
 ForwardBE: in STD_LOGIC_VECTOR(1 downto 0);
 StallF: in STD_LOGIC;
 StallD: in STD_LOGIC;
 FlushD: in STD_LOGIC);
 end component;
 component hazard
 port(clk, reset: in STD_LOGIC;
 Match_1E_M: in STD_LOGIC;
 Match_1E_W: in STD_LOGIC;
 Match_2E_M: in STD_LOGIC;
 Match_2E_W: in STD_LOGIC;
 Match_12D_E: in STD_LOGIC;
 RegWriteM: in STD_LOGIC;
 RegWriteW: in STD_LOGIC;
 BranchTakenE: in STD_LOGIC;
 MemtoRegE: in STD_LOGIC;
 PCWrPendingF: in STD_LOGIC;
 PCSrcW: in STD_LOGIC;
 ForwardAE: out STD_LOGIC_VECTOR(1 downto 0);
 ForwardBE: out STD_LOGIC_VECTOR(1 downto 0);
 StallF, StallD: out STD_LOGIC;
 FlushD, FlushE: out STD_LOGIC);
 end component;
 signal RegSrcD, ImmSrcD, ALUControlE: STD_LOGIC_VECTOR(1 downto 0);
 signal ALUSrcE, BranchTakenE, MemtoRegW, PCSrcW, RegWriteW: STD_LOGIC;
 signal ALUFlagsE: STD_LOGIC_VECTOR(3 downto 0);
 signal InstrD: STD_LOGIC_VECTOR(31 downto 0);
 signal RegWriteM, MemtoRegE, PCWrPendingF: STD_LOGIC;
 signal ForwardAE, ForwardBE: STD_LOGIC_VECTOR(1 downto 0);
 signal StallF, StallD, FlushD, FlushE: STD_LOGIC;
 signal Match_1E_M, Match_1E_W, Match_2E_M, Match_2E_W, Match_12D_E:
STD_LOGIC;

begin
 c: controller port map(clk, reset, InstrD(31 downto 12), ALUFlagsE,
 RegSrcD, ImmSrcD,
 ALUSrcE, BranchTakenE, ALUControlE,
 MemWriteM,
 MemtoRegW, PCSrcW, RegWriteW,
 RegWriteM, MemtoRegE, PCWrPendingF,
 FlushE);
 dp: datapath port map(clk, reset,
 RegSrcD, ImmSrcD,
 ALUSrcE, BranchTakenE, ALUControlE,
 MemtoRegW, PCSrcW, RegWriteW,
 PCF, InstrF, InstrD,
 ALUOutM, WriteDataM, ReadDataM,
 ALUFlagsE,
 Match_1E_M, Match_1E_W, Match_2E_M,
 Match_2E_W, Match_12D_E, 

 ForwardAE, ForwardBE, StallF, StallD, FlushD);
 h: hazard port map(clk, reset, Match_1E_M, Match_1E_W,
 Match_2E_M, Match_2E_W, Match_12D_E,
 RegWriteM, RegWriteW, BranchTakenE, MemtoRegE,
 PCWrPendingF, PCSrcW,
 ForwardAE, ForwardBE,
 StallF, StallD, FlushD, FlushE);
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity controller is -- pipelined control decoder
 port(clk, reset: in STD_LOGIC;
 InstrD: in STD_LOGIC_VECTOR(31 downto 12);
 ALUFlagsE: in STD_LOGIC_VECTOR(3 downto 0);
 RegSrcD, ImmSrcD: out STD_LOGIC_VECTOR(1 downto 0);
 ALUSrcE: out STD_LOGIC;
 BranchTakenE: out STD_LOGIC;
 ALUControlE: out STD_LOGIC_VECTOR(1 downto 0);
 MemWriteM: out STD_LOGIC;
 MemtoRegW: out STD_LOGIC;
 PCSrcW: out STD_LOGIC;
 RegWriteW: out STD_LOGIC;
 -- hazard interface
 RegWriteM: out STD_LOGIC;
 MemtoRegE: out STD_LOGIC;
 PCWrPendingF: out STD_LOGIC;
 FlushE: in STD_LOGIC);
end;
architecture synth of controller is
 component flopr generic(width: integer);
 port(clk, reset: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
 end component;
 component floprc generic(width: integer);
 port(clk, reset, clear: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
 end component;
 component conditional
 port(Cond: in STD_LOGIC_VECTOR(3 downto 0);
 Flags: in STD_LOGIC_VECTOR(3 downto 0);
 ALUFlags: in STD_LOGIC_VECTOR(3 downto 0);
 FlagsWrite: in STD_LOGIC_VECTOR(1 downto 0);
 CondEx: out STD_LOGIC;
 FlagsNext: out STD_LOGIC_VECTOR(3 downto 0));
 end component;
 signal controlsD: STD_LOGIC_VECTOR(9 downto 0);
 signal CondExE, ALUOpD: STD_LOGIC;
 signal ALUControlD: STD_LOGIC_VECTOR(1 downto 0);
 signal ALUSrcD: STD_LOGIC;
 signal MemtoRegD, MemtoRegM: STD_LOGIC;
 signal RegWriteD, RegWriteE, RegWriteGatedE: STD_LOGIC;
 signal MemWriteD, MemWriteE, MemWriteGatedE: STD_LOGIC; 

 signal BranchD, BranchE: STD_LOGIC;
 signal FlagWriteD, FlagWriteE: STD_LOGIC_VECTOR(1 downto 0);
 signal PCSrcD, PCSrcE, PCSrcM: STD_LOGIC;
 signal FlagsE, FlagsNextE, CondE: STD_LOGIC_VECTOR(3 downto 0);
 signal Funct: STD_LOGIC_VECTOR(5 downto 0);
 signal Rd: STD_LOGIC_VECTOR(3 downto 0);
 signal PCSrcGatedE: STD_LOGIC;
 signal FlushedValsEnext, FlushedValsE: STD_LOGIC_VECTOR(6 downto 0);
 signal ValsEnext, ValsE: STD_LOGIC_VECTOR(2 downto 0);
 signal ValsMnext, ValsM: STD_LOGIC_VECTOR(3 downto 0);
 signal ValsWnext, ValsW: STD_LOGIC_VECTOR(2 downto 0);
begin
 -- Decode stage
 -- Main Decoder
 process(all) begin
 case InstrD(27 downto 26) is
 when "00" => controlsD <= "0000101001" when InstrD(25) -- DP imm
 else "0000001001"; -- DP reg
 when "01" => controlsD <= "0001111000" when InstrD(20) -- LDR
 else "1001110100"; -- STR
 when "10" => controlsD <= "0110100010"; -- B
 when others => controlsD <= "----------"; --unimplemented
 end case;
 end process;
 (RegSrcD, ImmSrcD, ALUSrcD, MemtoRegD,
 RegWriteD, MemWriteD, BranchD, ALUOpD) <= controlsD;
 -- ALU Decoder
 Funct <= InstrD(25 downto 20);
 Rd <= InstrD(15 downto 12);
 process(all) begin
 if (ALUOpD) then
 case Funct(4 downto 1) is
 when "0100" => ALUControlD <= "00"; -- ADD
 when "0010" => ALUControlD <= "01"; -- SUB
 when "0000" => ALUControlD <= "10"; -- AND
 when "1100" => ALUControlD <= "11"; -- ORR
 when others => ALUControlD <= "--"; -- unimplemented
 end case;
 FlagWriteD(1) <= Funct(0);
 FlagWriteD(0) <= Funct(0) and (not ALUControlD(1));
 else
 ALUControlD <= "00";
 FlagWriteD <= "00";
 end if;
 end process;

 PCSrcD <= ((and Rd) and RegWriteD) or BranchD;
-- Execute stage
 FlushedValsEnext <= (FlagWriteD, BranchD, MemWriteD, RegWriteD,
 PCSrcD, MemtoRegD); 

 ValsEnext <= (ALUSrcD, ALUControlD);
 flushedregsE: floprc generic map (7)
 port map(clk, reset, FlushE, FlushedValsEnext, FlushedValsE);
 regsE: flopr generic map (3)
 port map(clk, reset, ValsEnext, ValsE);
 condregE: flopr generic map (4)
 port map(clk, reset, InstrD(31 downto 28), CondE);
 flagsreg: flopr generic map (4)
 port map(clk, reset, FlagsNextE, FlagsE);
 (FlagWriteE, BranchE, MemWriteE, RegWriteE, PCSrcE, MemtoRegE) <=
FlushedValsE;
 (ALUSrcE, ALUControlE) <= ValsE;
 -- write and Branch controls are conditional
 Cond: conditional port map(CondE, FlagsE, ALUFlagsE, FlagWriteE,
CondExE, FlagsNextE);
 BranchTakenE <= BranchE and CondExE;
 RegWriteGatedE <= RegWriteE and CondExE;
 MemWriteGatedE <= MemWriteE and CondExE;
 PCSrcGatedE <= PCSrcE and CondExE;
 -- Memory stage
 ValsMnext <= (MemWriteGatedE, MemtoRegE, RegWriteGatedE, PCSrcGatedE);
 regsM: flopr generic map (4)
 port map(clk, reset, ValsMnext, ValsM);
 (MemWriteM, MemtoRegM, RegWriteM, PCSrcM) <= ValsM;
 -- Writeback stage
 ValsWnext <= (MemtoRegM, RegWriteM, PCSrcM);
 regsW: flopr generic map (3)
 port map(clk, reset, ValsWnext, ValsW);
 (MemtoRegW, RegWriteW, PCSrcW) <= ValsW;
 -- Hazard Prediction
 PCWrPendingF <= PCSrcD or PCSrcE or PCSrcM;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity conditional is
 port(Cond: in STD_LOGIC_VECTOR(3 downto 0);
 Flags: in STD_LOGIC_VECTOR(3 downto 0);
 ALUFlags: in STD_LOGIC_VECTOR(3 downto 0);
 FlagsWrite: in STD_LOGIC_VECTOR(1 downto 0);
 CondEx: out STD_LOGIC;
 FlagsNext: out STD_LOGIC_VECTOR(3 downto 0));
end;
architecture behave of conditional is
 signal neg, zero, carry, overflow, ge: STD_LOGIC;
begin
 (neg, zero, carry, overflow) <= Flags;
 ge <= (neg xnor overflow);
 

 process(all) begin -- Condition checking
 case Cond is
 when "0000" => CondEx <= zero;
 when "0001" => CondEx <= not zero;
 when "0010" => CondEx <= carry;
 when "0011" => CondEx <= not carry;
 when "0100" => CondEx <= neg;
 when "0101" => CondEx <= not neg;
 when "0110" => CondEx <= overflow;
 when "0111" => CondEx <= not overflow;
 when "1000" => CondEx <= carry and (not zero);
 when "1001" => CondEx <= not(carry and (not zero));
 when "1010" => CondEx <= ge;
 when "1011" => CondEx <= not ge;
 when "1100" => CondEx <= (not zero) and ge;
 when "1101" => CondEx <= not ((not zero) and ge);
 when "1110" => CondEx <= '1';
 when others => CondEx <= '-';
 end case;
 end process;
 FlagsNext(3 downto 2) <= ALUFlags(3 downto 2) when (FlagsWrite(1) and
CondEx) else Flags(3 downto 2);
 FlagsNext(1 downto 0) <= ALUFlags(1 downto 0) when (FlagsWrite(0) and
CondEx) else Flags(1 downto 0);
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity datapath is
 port(clk, reset: in STD_LOGIC;
 RegSrcD, ImmSrcD: in STD_LOGIC_VECTOR(1 downto 0);
 ALUSrcE: in STD_LOGIC;
 BranchTakenE: in STD_LOGIC;
 ALUControlE: in STD_LOGIC_VECTOR(1 downto 0);
 MemtoRegW: in STD_LOGIC;
 PCSrcW: in STD_LOGIC;
 RegWriteW: in STD_LOGIC;
 PCF: out STD_LOGIC_VECTOR(31 downto 0);
 InstrF: in STD_LOGIC_VECTOR(31 downto 0);
 InstrD: out STD_LOGIC_VECTOR(31 downto 0);
 ALUOutM: out STD_LOGIC_VECTOR(31 downto 0);
 WriteDataM: out STD_LOGIC_VECTOR(31 downto 0);
 ReadDataM: in STD_LOGIC_VECTOR(31 downto 0);
 ALUFlagsE: out STD_LOGIC_VECTOR(3 downto 0);
 -- hazard logic
 Match_1E_M: out STD_LOGIC;
 Match_1E_W: out STD_LOGIC;
 Match_2E_M: out STD_LOGIC;
 Match_2E_W: out STD_LOGIC;
 Match_12D_E: out STD_LOGIC;
 ForwardAE: in STD_LOGIC_VECTOR(1 downto 0);
 ForwardBE: in STD_LOGIC_VECTOR(1 downto 0);
 StallF: in STD_LOGIC;
 StallD: in STD_LOGIC; 

 FlushD: in STD_LOGIC);
end;
architecture struct of datapath is
 component alu
 port(a, b: in STD_LOGIC_VECTOR(31 downto 0);
 ALUControl: in STD_LOGIC_VECTOR(1 downto 0);
 Result: buffer STD_LOGIC_VECTOR(31 downto 0);
 ALUFlags: out STD_LOGIC_VECTOR(3 downto 0));
 end component;
 component regfile
 port(clk: in STD_LOGIC;
 we3: in STD_LOGIC;
 ra1, ra2, wa3: in STD_LOGIC_VECTOR(3 downto 0);
 wd3, r15: in STD_LOGIC_VECTOR(31 downto 0);
 rd1, rd2: out STD_LOGIC_VECTOR(31 downto 0));
 end component;
 component adder
 port(a, b: in STD_LOGIC_VECTOR(31 downto 0);
 y: out STD_LOGIC_VECTOR(31 downto 0));
 end component;
 component extend
 port(Instr: in STD_LOGIC_VECTOR(23 downto 0);
 ImmSrc: in STD_LOGIC_VECTOR(1 downto 0);
 ExtImm: out STD_LOGIC_VECTOR(31 downto 0));
 end component;
 component flopr generic(width: integer);
 port(clk, reset: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
 end component;
 component flopenrc generic(width: integer);
 port(clk, reset, en, clear: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
 end component;
 component flopenr generic(width: integer);
 port(clk, reset, en: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
 end component;
 component mux2 generic(width: integer);
 port(d0, d1: in STD_LOGIC_VECTOR(width-1 downto 0);
 s: in STD_LOGIC;
 y: out STD_LOGIC_VECTOR(width-1 downto 0));
 end component;
 component mux3 generic(width: integer);
 port(d0, d1, d2: in STD_LOGIC_VECTOR(width-1 downto 0);
 s: in STD_LOGIC_VECTOR(1 downto 0);
 y: out STD_LOGIC_VECTOR(width-1 downto 0));
 end component;
 component eqcmp generic(width: integer);
 port(a, b: in STD_LOGIC_VECTOR(width-1 downto 0);
 y: out STD_LOGIC);
 end component; 

 signal PCPlus4F, PCnext1F, PCnextF: STD_LOGIC_VECTOR(31 downto 0);
 signal ExtImmD, rd1D, rd2D, PCPlus8D: STD_LOGIC_VECTOR(31 downto 0);
 signal rd1E, rd2E, ExtImmE, SrcAE: STD_LOGIC_VECTOR(31 downto 0);
 signal SrcBE, WriteDataE, ALUResultE: STD_LOGIC_VECTOR(31 downto 0);
 signal ReadDataW, ALUOutW, ResultW: STD_LOGIC_VECTOR(31 downto 0);
 signal RA1D, RA2D, RA1E, RA2E: STD_LOGIC_VECTOR(3 downto 0);
 signal WA3E, WA3M, WA3W: STD_LOGIC_VECTOR(3 downto 0);
 signal Match_1D_E, Match_2D_E: STD_LOGIC;
 signal notStallF: STD_LOGIC;
begin
 -- Fetch stage
 notStallF <= (not StallF);
 pcnextmux: mux2 generic map (32)
 port map(PCPlus4F, ResultW, PCSrcW, PCnext1F);
 branchmux: mux2 generic map (32)
 port map(PCnext1F, ALUResultE, BranchTakenE, PCnextF);
 pcreg: flopenr generic map (32)
 port map(clk, reset, notStallF, PCnextF, PCF);
 pcadd: adder generic map (32)
 port map(PCF, 32D"4", PCPlus4F);

 -- Decode Stage
 PCPlus8D <= PCPlus4F; -- skip register
 instrreg: flopenrc generic map (32)
 port map(clk, reset, (not StallD), FlushD, InstrF, InstrD);
 ra1mux: mux2 generic map (4)
 port map(InstrD(19 downto 16), 4D"15", RegSrcD(0), RA1D);
 ra2mux: mux2 generic map (4)
 port map(InstrD(3 downto 0), InstrD(15 downto 12), RegSrcD(1), RA2D);
 rf: regfile
 port map(clk, RegWriteW, RA1D, RA2D,
 WA3W, ResultW, PCPlus8D,
 rd1D, rd2D);
 ext: extend
 port map(InstrD(23 downto 0), ImmSrcD, ExtImmD);

 -- Execute Stage
 rd1reg: flopr generic map (32)
 port map(clk, reset, rd1D, rd1E);
 rd2reg: flopr generic map (32)
 port map(clk, reset, rd2D, rd2E);
 immreg: flopr generic map (32)
 port map(clk, reset, ExtImmD, ExtImmE);
 wa3ereg: flopr generic map (4)
 port map(clk, reset, InstrD(15 downto 12), WA3E);
 ra1reg: flopr generic map (4)
 port map(clk, reset, RA1D, RA1E);
 ra2reg: flopr generic map (4)
 port map(clk, reset, RA2D, RA2E);
 byp1mux: mux3 generic map (32)
 port map(rd1E, ResultW, ALUOutM, ForwardAE, SrcAE);
 byp2mux: mux3 generic map (32)
 port map(rd2E, ResultW, ALUOutM, ForwardBE, WriteDataE); 

 srcbmux: mux2 generic map (32)
 port map(WriteDataE, ExtImmE, ALUSrcE, SrcBE);
 i_alu: alu
 port map(SrcAE, SrcBE, ALUControlE, ALUResultE, ALUFlagsE);

 -- Memory Stage
 aluresreg: flopr generic map (32)
 port map(clk, reset, ALUResultE, ALUOutM);
 wdreg: flopr generic map (32)
 port map(clk, reset, WriteDataE, WriteDataM);
 wa3mreg: flopr generic map (4)
 port map(clk, reset, WA3E, WA3M);

 -- Writeback Stage
 aluoutreg: flopr generic map (32)
 port map(clk, reset, ALUOutM, ALUOutW);
 rdreg: flopr generic map (32)
 port map(clk, reset, ReadDataM, ReadDataW);
 wa3wreg: flopr generic map (4)
 port map(clk, reset, WA3M, WA3W);
 resmux: mux2 generic map (32)
 port map(ALUOutW, ReadDataW, MemtoRegW, ResultW);

 -- hazard comparison
 m0: eqcmp generic map (4)
 port map(WA3M, RA1E, Match_1E_M);
 m1: eqcmp generic map (4)
 port map(WA3W, RA1E, Match_1E_W);
 m2: eqcmp generic map (4)
 port map(WA3M, RA2E, Match_2E_M);
 m3: eqcmp generic map (4)
 port map(WA3W, RA2E, Match_2E_W);
 m4a: eqcmp generic map (4)
 port map(WA3E, RA1D, Match_1D_E);
 m4b: eqcmp generic map (4)
 port map(WA3E, RA2D, Match_2D_E);
 Match_12D_E <= Match_1D_E or Match_2D_E;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity hazard is
 port(clk, reset: in STD_LOGIC;
 Match_1E_M: in STD_LOGIC;
 Match_1E_W: in STD_LOGIC;
 Match_2E_M: in STD_LOGIC;
 Match_2E_W: in STD_LOGIC;
 Match_12D_E: in STD_LOGIC;
 RegWriteM: in STD_LOGIC;
 RegWriteW: in STD_LOGIC;
 BranchTakenE: in STD_LOGIC;
 MemtoRegE: in STD_LOGIC;
 PCWrPendingF: in STD_LOGIC;
 PCSrcW: in STD_LOGIC;
 ForwardAE: out STD_LOGIC_VECTOR(1 downto 0); 

 ForwardBE: out STD_LOGIC_VECTOR(1 downto 0);
 StallF, StallD: out STD_LOGIC;
 FlushD, FlushE: out STD_LOGIC);
end;
architecture behave of hazard is
 signal ldrStallD: STD_LOGIC;
begin
 ForwardAE(1) <= '1' when (Match_1E_M and RegWriteM) else '0';
 ForwardAE(0) <= '1' when (Match_1E_W and RegWriteW and (not
ForwardAE(1))) else '0';
 ForwardBE(1) <= '1' when (Match_2E_M and RegWriteM) else '0';
 ForwardBE(0) <= '1' when (Match_2E_W and RegWriteW and (not
ForwardBE(1))) else '0';
 ldrStallD <= Match_12D_E and MemtoRegE;

 StallD <= ldrStallD;
 StallF <= ldrStallD or PCWrPendingF;
 FlushE <= ldrStallD or BranchTakenE;
 FlushD <= PCWrPendingF or PCSrcW or BranchTakenE;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity regfile is -- three-port register file
 port(clk: in STD_LOGIC;
 we3: in STD_LOGIC;
 ra1, ra2, wa3: in STD_LOGIC_VECTOR(3 downto 0);
 wd3, r15: in STD_LOGIC_VECTOR(31 downto 0);
 rd1, rd2: out STD_LOGIC_VECTOR(31 downto 0));
end;
architecture behave of regfile is
 type ramtype is array (31 downto 0) of
 STD_LOGIC_VECTOR(31 downto 0);
 signal mem: ramtype;

 signal db_r0 : std_logic_vector(31 downto 0);
 signal db_r1 : std_logic_vector(31 downto 0);
 signal db_r2 : std_logic_vector(31 downto 0);
 signal db_r3 : std_logic_vector(31 downto 0);
 signal db_r4 : std_logic_vector(31 downto 0);
 signal db_r5 : std_logic_vector(31 downto 0);
 signal db_r6 : std_logic_vector(31 downto 0);
 signal db_r7 : std_logic_vector(31 downto 0);
 signal db_r8 : std_logic_vector(31 downto 0);
 signal db_r9 : std_logic_vector(31 downto 0);
 signal db_r10 : std_logic_vector(31 downto 0);
 signal db_r11 : std_logic_vector(31 downto 0);
 signal db_r12 : std_logic_vector(31 downto 0);
 signal db_r13 : std_logic_vector(31 downto 0);
 signal db_r14 : std_logic_vector(31 downto 0);
 signal db_r15 : std_logic_vector(31 downto 0);
begin
 process(clk) begin
 if falling_edge(clk) then -- write rf on negative edge of clock
 if we3 = '1' then mem(to_integer(wa3)) <= wd3;
 end if;
 end if;
 end process;
 process(all) begin
 if (to_integer(ra1) = 15) then rd1 <= r15;
 else rd1 <= mem(to_integer(ra1));
 end if;
 if (to_integer(ra2) = 15) then rd2 <= r15;
 else rd2 <= mem(to_integer(ra2));
 end if;
 end process;

 db_r0 <= mem(0);
 db_r1 <= mem(1);
 db_r2 <= mem(2);
 db_r3 <= mem(3);
 db_r4 <= mem(4);
 db_r5 <= mem(5);
 db_r6 <= mem(6);
 db_r7 <= mem(7);
 db_r8 <= mem(8);
 db_r9 <= mem(9);
 db_r10 <= mem(10);
 db_r11 <= mem(11);
 db_r12 <= mem(12);
 db_r13 <= mem(13);
 db_r14 <= mem(14);
 db_r15 <= mem(15);
end; 

library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity adder is -- adder
 port(a, b: in STD_LOGIC_VECTOR(31 downto 0);
 y: out STD_LOGIC_VECTOR(31 downto 0));
end;
architecture behave of adder is
begin
 y <= a + b;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity extend is
 port(Instr: in STD_LOGIC_VECTOR(23 downto 0);
 ImmSrc: in STD_LOGIC_VECTOR(1 downto 0);
 ExtImm: out STD_LOGIC_VECTOR(31 downto 0));
end;
architecture behave of extend is
begin
 process(all) begin
 case ImmSrc is
 when "00" => ExtImm <= (X"000000", Instr(7 downto 0));
 when "01" => ExtImm <= (X"00000", Instr(11 downto 0));
 when "10" => ExtImm <= (Instr(23), Instr(23), Instr(23),
 Instr(23), Instr(23), Instr(23), Instr(23 downto 0), "00");
 when others => ExtImm <= X"--------";
 end case;
 end process;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity flopenr is -- flip-flop with enable and asynchronous reset
 generic(width: integer);
 port(clk, reset, en: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
end;
architecture asynchronous of flopenr is
begin
 process(clk, reset) begin
 if reset then q <= (others => '0');
 elsif rising_edge(clk) then
 if en then
 q <= d;
 end if;
 end if;
 end process;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all; 

entity flopr is -- flip-flop with asynchronous reset
 generic(width: integer);
 port(clk, reset: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
end;
architecture asynchronous of flopr is
begin
 process(clk, reset) begin
 if reset then q <= (others => '0');
 elsif rising_edge(clk) then
 q <= d;
 end if;
 end process;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity floprc is -- flip-flop with asynchronous reset
 -- and synchronous clear
 generic(width: integer);
 port(clk, reset, clear: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
end;
architecture asynchronous of floprc is
begin
 process(clk, reset) begin
 if reset then q <= (others => '0');
 elsif rising_edge(clk) then
 if clear then q <= (others => '0');
 else q <= d;
 end if;
 end if;
 end process;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity flopenrc is -- flip-flop with enable and asynchronous reset,synchronous clear
 generic(width: integer);
 port(clk, reset, en, clear: in STD_LOGIC;
 d: in STD_LOGIC_VECTOR(width-1 downto 0);
 q: out STD_LOGIC_VECTOR(width-1 downto 0));
end;
architecture asynchronous of flopenrc is
begin
 process(clk, reset) begin
 if reset then q <= (others => '0');
 elsif rising_edge(clk) then
 if en then
 if clear then 

 q <= (others => '0');
 else
 q <= d;
 end if;
 end if;
 end if;
 end process;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity mux2 is -- two-input multiplexer
 generic(width: integer);
 port(d0, d1: in STD_LOGIC_VECTOR(width-1 downto 0);
 s: in STD_LOGIC;
 y: out STD_LOGIC_VECTOR(width-1 downto 0));
end;
architecture behave of mux2 is
begin
 y <= d1 when s else d0;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity mux3 is -- three-input multiplexer
 generic(width: integer);
 port(d0, d1, d2: in STD_LOGIC_VECTOR(width-1 downto 0);
 s: in STD_LOGIC_VECTOR(1 downto 0);
 y: out STD_LOGIC_VECTOR(width-1 downto 0));
end;
architecture behave of mux3 is
begin
 process(all) begin
 case s is
 when "00" => y <= d0;
 when "01" => y <= d1;
 when "10" => y <= d2;
 when others => y <= d0;
 end case;
 end process;
end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
entity eqcmp is -- equality comparator
 generic(width: integer);
 port(a, b: in STD_LOGIC_VECTOR(width-1 downto 0);
 y: out STD_LOGIC);
end;
architecture behave of eqcmp is
begin
 y <= '1'when a = b else '0'; 

end;
library IEEE; use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity alu is
 port(a, b: in STD_LOGIC_VECTOR(31 downto 0);
 ALUControl: in STD_LOGIC_VECTOR(1 downto 0);
 Result: buffer STD_LOGIC_VECTOR(31 downto 0);
 ALUFlags: out STD_LOGIC_VECTOR(3 downto 0));
end;
architecture behave of alu is
 signal condinvb: STD_LOGIC_VECTOR(31 downto 0);
 signal sum: STD_LOGIC_VECTOR(32 downto 0);
 signal neg, zero, carry, overflow: STD_LOGIC;
begin
 condinvb <= not b when ALUControl(0) else b;
 sum <= ('0', a) + ('0', condinvb) + ALUControl(0);
 process(all) begin
 case? ALUControl(1 downto 0) is
 when "0-" => result <= sum(31 downto 0);
 when "10" => result <= a and b;
 when "11" => result <= a or b;
 when others => result <= (others => '-');
 end case?;
 end process;
 neg <= Result(31);
 zero <= '1' when (Result = 0) else '0';
 carry <= (not ALUControl(1)) and sum(32);
 overflow <= (not ALUControl(1)) and
 (not (a(31) xor b(31) xor ALUControl(0))) and
 (a(31) xor sum(31));
 ALUFlags <= (neg, zero, carry, overflow);
end; 