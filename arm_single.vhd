---------------------------------------------------------------
-- arm_single.vhd
-- David_Harris@hmc.edu, Sarah.Harris@unlv.edu 6 March 2014
-- Single-cycle implementation of a subset of ARMv4
--
-- Compile in ModelSim at the command line with the command
-- vcom -2008 arm_single.vhd
-- Expect plenty of simulation warnings of metavalues detected
-- run 210
-- Expect at time 205 ns a message of
-- Failure: NO ERRORS: Simulation succeeded
-- when the value 7 is written to address 100 (0x64)
---------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity testbench is
end;

architecture test OF testbench is
  component top
    port (
      clk, reset : in std_logic;
      WriteData, DatAadr : out std_logic_vector(31 downto 0);
      MemWrite : out std_logic);
  end component;
  signal WriteData, DataAdr : std_logic_vector(31 downto 0);
  signal clk, reset, MemWrite : std_logic;
begin

  -- instantiate device to be tested
  dut : top port map(clk, reset, WriteData, DataAdr, MemWrite);

  -- Generate clock with 10 ns period
  PROCESS begin
    clk <= '1';
    WAIT FOR 5 ns;
    clk <= '0';
    WAIT FOR 5 ns;
  end PROCESS;

  -- Generate reset for first two clock cycles
  PROCESS begin
    reset <= '1';
    WAIT FOR 22 ns;
    reset <= '0';
    WAIT;
  end PROCESS;

  -- check that 7 gets written to address 84 
  -- at end of program
  PROCESS (clk) begin
    IF (clk'event AND clk = '0' AND MemWrite = '1') THEN
      IF (to_integer(DataAdr) = 100 AND
        to_integer(WriteData) = 7) THEN
        REport "NO ERRORS: Simulation succeeded" SEVERITY failure;
      ELSIF (DataAdr /= 96) THEN
        REport "Simulation failed" SEVERITY failure;
      end IF;
    end IF;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity top is -- top-level design for testing
  port (
    clk, reset : in std_logic;
    WriteData, DataAdr : BUFFER std_logic_vector(31 downto 0);
    MemWrite : BUFFER std_logic);
end;

architecture test OF top is
  component arm
    port (
      clk, reset : in std_logic;
      PC : out std_logic_vector(31 downto 0);
      instr : in std_logic_vector(31 downto 0);
      MemWrite : out std_logic;
      ALUResult, WriteData : out std_logic_vector(31 downto 0);
      ReadData : in std_logic_vector(31 downto 0));
  end component;
  component imem
    port (
      a : in std_logic_vector(31 downto 0);
      rd : out std_logic_vector(31 downto 0));
  end component;
  component dmem
    port (
      clk, we : in std_logic;
      a, wd : in std_logic_vector(31 downto 0);
      rd : out std_logic_vector(31 downto 0));
  end component;
  signal PC, instr,
  ReadData : std_logic_vector(31 downto 0);
begin
  -- instantiate processor and memories
  i_arm : arm port map(
    clk, reset, PC, instr, MemWrite, DataAdr,
    WriteData, ReadData);
  i_imem : imem port map(PC, instr);
  i_dmem : dmem port map(
    clk, MemWrite, DataAdr,
    WriteData, ReadData);
end;

-------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity partial_IF_ID is
  port (
    clock, reset : in std_logic;
    instrF : in std_logic_vector(31 downto 0);
    stallD, flushD : in std_logic;

    instrD : in std_logic_vector(31 downto 0);
  );
end entity;

architecture arch of partial_IF_ID is

  signal s_instr : std_logic_vector(31 downto 0);
  signal s_stall, s_flush : std_logic;

begin

  instrD <= instrF;
  
end architecture;

--------------------------------------------------------------------------

entity partial_ID_EX is
  port (
    clock, reset : in std_logic;
    PCSrcD, RegWriteD : in std_logic;
    MemtoRegD, MemWriteD : in std_logic;
    ALUControlD, FlagWriteD : in std_logic_vector(1 downto 0);
    BranchD, ALUSrcD : in std_logic;
    RD1D, RD2D, extendD : in std_logic_vector(31 downto 0)
    WA3D : in std_logic_vector(3 downto 0);
    CondD: in std_logic_vector(3 downto 0);
    FlagsD : out std_logic_vector();--[ver tamanho]
    FLushE : in std_logic;

    PCSrcE, RegWriteE : out std_logic;
    MemtoRegE, MemWriteE : out std_logic;
    ALUControlE, FlagWriteE : out std_logic_vector(1 downto 0);
    BranchE, ALUSrcE : out std_logic
    RD1E, RD2E, extendE : in std_logic_vector(31 downto 0)
    WA3E : in std_logic_vector(3 downto 0)
    CondE : in std_logic_vector(3 downto 0);
    FlagsE : out std_logic_vector();--[ver tamanho]
  );
end entity;

architecture arch OF partial_ID_EX is

    signal s_PCSrc, s_RegWrite : std_logic;
    signal s_MemtoReg, s_MemWrite : std_logic;
    signal s_ALUControl, s_FlagWrite : std_logic_vector(1 downto 0);
    signal s_Branch, s_ALUSrc : std_logic;
    signal s_R1D, s_R2D, s_extend : std_logic_vector(31 downto 0)
    signal s_WA3D : std_logic_vector(3 downto 0);
    signal s_Cond: std_logic_vector(3 downto 0);
    --signal s_Flags : std_logic_vector();--[ver tamanho]

begin
    
   s_PCSrc <= PCSrcD;
   s_RegWrite <= RegWriteD;
   s_MemtoReg <= MemtoRegD;
   s_MemWrite <= MemWriteD;
   s_ALUControl <= ALUControlD;
   s_FlagWrite <= FlagWriteD;
   s_Branch <= BranchD;
   s_ALUSrc <= ALUSrcD
   s_R1D <= RD1D;
   s_R2D <= RD2D;
   s_extend <= extendD;
   s_WA3D <= WA3D;
   s_Cond <= CondD;
   s_Flags <=FlagsD;

   PCSrcE <= s_PCSrc
   RegWriteE <= s_RegWrite
   MemtoRegE <= s_MemtoReg
   MemWriteE <= s_MemWrite
   ALUControlE <= s_ALUControl
   FlagWriteE <= s_FlagWrite 
   BranchE <= s_Branch 
   ALUSrcE <= s_ALUSrc 
   RD1E <= s_R1D
   RD2E <= s_R2D
   extendE <= s_extend 
   WA3E <= s_WA3D
   CondE <= s_Cond
   FlagsE <= s_Flags 

end architecture;

------------------------------------------------------------------

entity partial_EX_MEM is
  port (
    clock, reset : in std_logic;

    PCSrcE, RegWriteE, MemtoRegE, MemWriteE : in std_logic; -- Sinais combinatorios
    ALUResultE, WriteDataE : in std_logic_vector(31 downto 0);
    WA3E : in std_logic_vector(3 downto 0)
  
    PCSrcM, RegWriteM, MemtoRegM, MemWriteM : in std_logic; -- Sinais combinatorios
    ALUResultm, WriteDataM : in std_logic_vector(31 downto 0);
    WA3M : in std_logic_vector(3 downto 0)
  );
end entity;

architecture arch of partial_EX_MEM is

  signal s_PCSrc, s_RegWrite, s_MemtoReg, s_MemWrite : std_logic; -- Sinais combinatorios
  signal s_ALUResult, s_WriteData, : std_logic_vector(31 downto 0);
  signal s_WA3 : std_logic_vector(3 downto 0);

begin

  s_PCSrc <= PCSrcE;
  s_RegWrite <= RegWriteE; 
  s_MemtoReg <= MemtoRegE; 
  s_MemWrite <= MemWriteE;
  s_ALUResult <= ALUResultE;
  s_WriteData <= WriteDataE;
  s_WA3 <= WA3E;
  
  PCSrcM <= s_PCSrc;
  RegWriteM <=  s_RegWrite; 
  MemtoRegM <=  s_MemtoReg; 
  MemWriteM <= s_MemWrite;
  ALUResultM <= s_ALUResult;
  WriteDataM <= s_WriteData;
  WA3M <= s_WA3;

end architecture;

--------------------------------------------------------

entity partial_MEM_WB is
  port (
    clock : in std_logic;
    
    PCSrcM : in std_logic;
    RegWriteM : in std_logic;
    MemtoRegM : in std_logic;
    ALUOutM   : in std_logic_vector(31 downto 0);
    WA3M      : in std_logic_vector(31 downto 0);
    RD      : in std_logic_vector(31 downto 0);

    PCSrcW : out std_logic;
    RegWriteW : out std_logic;
    MemtoRegW : out std_logic;
    ReadDataW : out std_logic;
    ALUOutW   : out std_logic_vector(31 downto 0);
    WA3W      : out std_logic_vector(31 downto 0);
  );
end entity;

architecture arch OF partial_MEM_WB is

  --signal s_clock : std_logic;
  signal s_PCSrc : std_logic;
  signal s_RegWrite : std_logic;
  signal s_MemtoReg : std_logic;
  signal s_PCSrc : std_logic;
  signal s_RegWrite : std_logic;
  signal s_MemtoReg : std_logic;

begin

s_PCSrc <= PCSrcM; 
s_RegWrite <= RegWriteM;
s_MemtoReg <= MemtoRegM;

PCSrcW <= s_PCSrc;
RegWriteW <= s_RegWrite;
MemtoRegW <= s_MemtoReg;

end architecture;


entity hazard_unit is
  port (
    clock : in std_logic;
    reset : in std_logic;
    RA1E : in std_logic_vector(31 downto 0);
    RA2E : in std_logic_vector(31 downto 0);
    WA3M : in std_logic_vector(31 downto 0);
    RegWriteM : in std_logic;
    RegWriteW : in std_logic;
    MemToRegE : in std_logic;
    StallF : out std_logic;
    StallD : out std_logic;
    FlushD : out std_logic;
    FlushE : out std_logic;
    ForwardAE : out std_logic_vector(1 downto 0);
    ForwardBE : out std_logic_vector(1 downto 0);
  );
end entity;

library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity dmem is -- data memory
  port (
    clk, we : in std_logic;
    a, wd : in std_logic_vector(31 downto 0);
    rd : out std_logic_vector(31 downto 0));
end;

architecture behave OF dmem is
begin
  PROCESS is
    TYPE ramtype is ARRAY (63 downto 0) OF
    std_logic_vector(31 downto 0);
    VARIABLE mem : ramtype;
  begin -- read or write memory
    LOOP
      IF clk'event AND clk = '1' THEN
        IF (we = '1') THEN
          mem(to_integer(a(7 downto 2))) := wd;
        end IF;
      end IF;
      rd <= mem(to_integer(a(7 downto 2)));
      WAIT ON clk, a;
    end LOOP;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity imem is -- instruction memory
  port (
    a : in std_logic_vector(31 downto 0);
    rd : out std_logic_vector(31 downto 0));
end;
architecture behave OF imem is -- instruction memory
begin
  PROCESS is
    FILE mem_file : TEXT;
    VARIABLE L : line;
    VARIABLE ch : CHARACTER;
    VARIABLE i, index, result : inTEGER;
    TYPE ramtype is ARRAY (63 downto 0) OF
    std_logic_vector(31 downto 0);
    VARIABLE mem : ramtype;
  begin
    -- initialize memory from file
    FOR i in 0 TO 63 LOOP -- set all contents low
      mem(i) := (OTHERS => '0');
    end LOOP;
    index := 0;
    FILE_OPEN(mem_file, "memfile.dat", READ_MODE);
    WHILE NOT endfile(mem_file) LOOP
      readline(mem_file, L);
      result := 0;
      FOR i in 1 TO 8 LOOP
        read(L, ch);
        IF '0' <= ch AND ch <= '9' THEN
          result := CHARACTER'pos(ch) - CHARACTER'pos('0');
        ELSIF 'a' <= ch AND ch <= 'f' THEN
          result := CHARACTER'pos(ch) - CHARACTER'pos('a') + 10;
        ELSIF 'A' <= ch AND ch <= 'F' THEN
          result := CHARACTER'pos(ch) - CHARACTER'pos('A') + 10;
        ELSE
          REport "Format error on line " & inTEGER'image(index)
            SEVERITY error;
        end IF;
        mem(index)(35 - i * 4 downto 32 - i * 4) :=
        to_std_logic_vector(result, 4);
      end LOOP;
      index := index + 1;
    end LOOP;

    -- read memory
    LOOP
      rd <= mem(to_integer(a(7 downto 2)));
      WAIT ON a;
    end LOOP;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity arm is -- single cycle processor
  port (
    clk, reset : in std_logic;
    PC : out std_logic_vector(31 downto 0);
    instr : in std_logic_vector(31 downto 0);
    MemWrite : out std_logic;
    ALUResult, WriteData : out std_logic_vector(31 downto 0);
    ReadData : in std_logic_vector(31 downto 0));
end;

architecture struct OF arm is
  
component partial_IF_ID is
  port (
    clock, reset : in std_logic;
    instrF : in std_logic_vector(31 downto 0);
    stallD, flushD : in std_logic;

    instrD : in std_logic_vector(31 downto 0);
  );
end component;

--------------------------------------------------------------------------

component partial_ID_EX is
  port (
    clock, reset : in std_logic;
    PCSrcD, RegWriteD : in std_logic;
    MemtoRegD, MemWriteD : in std_logic;
    ALUControlD, FlagWriteD : in std_logic_vector(1 downto 0);
    BranchD, ALUSrcD : in std_logic;
    RD1D, RD2D, extendD : in std_logic_vector(31 downto 0)
    WA3D : in std_logic_vector(3 downto 0);
    CondD: in std_logic_vector(3 downto 0);
    FlagsD : out std_logic_vector();--[ver tamanho]
    FLushE : in std_logic;

    PCSrcE, RegWriteE : out std_logic;
    MemtoRegE, MemWriteE : out std_logic;
    ALUControlE, FlagWriteE : out std_logic_vector(1 downto 0);
    BranchE, ALUSrcE : out std_logic
    RD1E, RD2E, extendE : in std_logic_vector(31 downto 0)
    WA3E : in std_logic_vector(3 downto 0)
    CondE : in std_logic_vector(3 downto 0);
    FlagsE : out std_logic_vector();--[ver tamanho]
  );
end component;

------------------------------------------------------------------

component partial_EX_MEM is
  port (
    clock, reset : in std_logic;

    PCSrcE, RegWriteE, MemtoRegE, MemWriteE : in std_logic; -- Sinais combinatorios
    ALUResultE, WriteDataE : in std_logic_vector(31 downto 0);
    WA3E : in std_logic_vector(3 downto 0)
  
    PCSrcM, RegWriteM, MemtoRegM, MemWriteM : in std_logic; -- Sinais combinatorios
    ALUResultm, WriteDataM : in std_logic_vector(31 downto 0);
    WA3M : in std_logic_vector(3 downto 0)
  );
end component;


--------------------------------------------------------

component partial_MEM_WB is
  port (
    clock : in std_logic;
    
    PCSrcM : in std_logic;
    RegWriteM : in std_logic;
    MemtoRegM : in std_logic;
    ALUOutM   : in std_logic_vector(31 downto 0);
    WA3M      : in std_logic_vector(31 downto 0);
    RD      : in std_logic_vector(31 downto 0);

    PCSrcW : out std_logic;
    RegWriteW : out std_logic;
    MemtoRegW : out std_logic;
    ReadDataW : out std_logic;
    ALUOutW   : out std_logic_vector(31 downto 0);
    WA3W      : out std_logic_vector(31 downto 0);
  );
end component;

---------------------------------------------------------

component hazard_unit is
  port (
    clock : in std_logic;
    reset : in std_logic;
    RA1E : in std_logic_vector(31 downto 0);
    RA2E : in std_logic_vector(31 downto 0);
    WA3M : in std_logic_vector(31 downto 0);
    RegWriteM : in std_logic;
    RegWriteW : in std_logic;
    MemToRegE : in std_logic;
    StallF : out std_logic;
    StallD : out std_logic;
    FlushD : out std_logic;
    FlushE : out std_logic;
    ForwardAE : out std_logic_vector(1 downto 0);
    ForwardBE : out std_logic_vector(1 downto 0);
  );
end component;

-------------------------------------------------------

component controller
    port (
      clk, reset : in std_logic;
      instr : in std_logic_vector(31 downto 12);
      --ALUFlags : in std_logic_vector(3 downto 0);

      RegSrc : out std_logic_vector(1 downto 0);
      RegWrite : out std_logic;
      ImmSrc : out std_logic_vector(1 downto 0);
      ALUSrc : out std_logic;
      ALUControl : out std_logic_vector(1 downto 0);
      MemWrite : out std_logic;
      MemtoReg : out std_logic;
      PCSrc : out std_logic);
    
      -- Sinais a mais pra poder controlar o fluxo das instrucoes
      FlagWrite, Branch : out std_logic
  end component;

-------------------------------------------

  component datapath
    port (
      clk, reset : in std_logic;
      RegSrc : in std_logic_vector(1 downto 0);
      RegWrite : in std_logic;
      ImmSrc : in std_logic_vector(1 downto 0);
      ALUSrc : in std_logic;
      ALUControl : in std_logic_vector(1 downto 0);
      MemtoReg : in std_logic;
      PCSrc : in std_logic;
      ALUFlags : out std_logic_vector(3 downto 0);
      PC : BUFFER std_logic_vector(31 downto 0);
      instr : in std_logic_vector(31 downto 0);
      ALUResult, WriteData : BUFFER std_logic_vector(31 downto 0);
      ReadData : in std_logic_vector(31 downto 0));
  end component; 
  
--------------------------------------------------

component cond_unit
port (
  clk, reset : in std_logic;
  Cond : in std_logic_vector(3 downto 0);
  ALUFlags : in std_logic_vector(3 downto 0);
  FlagW : in std_logic_vector(1 downto 0);
  PCS, RegW, MemW : in std_logic;

  PCSrc, RegWrite : out std_logic;
  MemWrite : out std_logic);
end component;

  signal RegWrite, ALUSrc, MemtoReg, PCSrc : std_logic;
  signal RegSrc, ImmSrc, ALUControl : std_logic_vector(1 downto 0);
  signal ALUFlags : std_logic_vector(3 downto 0);
  -- CUIDADO COM A LINHA ACIMA, ELA ESTA AZUL CLARO


  signal FlagWriteE : std_logic;
  signal PCS, RegW, MemW : std_logic;
  signal condE : std_logic_vector(3 downto 0);

  --signal RegWriteD, MemWriteD, PCSrcD : std_logic;
  --signal RegWriteE, MemWriteE, PCSrcE : std_logic;

  -- Fetch
  signal instrF std_logic_vector(31 downto 0);

  -- Decode
  signal stallD, flushD std_logic;
  signal instrD std_logic_vector(31 downto 0);
  signal PCSrcD, RegWriteD std_logic;
  signal MemtoRegD, MemWriteD std_logic;
  signal ALUControlD, FlagWriteD std_logic_vector(1 downto 0);
  signal BranchD, ALUSrcD std_logic;
  signal RD1D, RD2D, extendD std_logic_vector(31 downto 0)
  signal WA3D std_logic_vector(3 downto 0);
  signal CondD: in std_logic_vector(3 downto 0);
  --signal FlagsD std_logic_vector(????);--[ver tamanho]
  signal FLushE std_logic;

  -- Execute
  signal PCSrcE, RegWriteE std_logic;
  signal MemtoRegE, MemWriteE std_logic;
  signal ALUControlE, FlagWriteE std_logic_vector(1 downto 0);
  signal BranchE, ALUSrcE std_logic
  signal RD1E, RD2E, extendE std_logic_vector(31 downto 0)
  signal WA3E std_logic_vector(3 downto 0)
  signal CondE std_logic_vector(3 downto 0);
  signal FlagsE std_logic_vector();--[ver tamanho]

  -- Memory
  signal PCSrcM, RegWriteM, MemtoRegM, MemWriteM std_logic; -- Sinais combinatorios
  signal ALUResultm, WriteDataM std_logic_vector(31 downto 0);
  signal WA3M std_logic_vector(3 downto 0)

  signal ALUOutM std_logic_vector(31 downto 0);
  signal ReadDataM std_logic_vector(31 downto 0);

  -- Write back
  signal PCSrcW std_logic;
  signal RegWriteW std_logic;
  signal MemtoRegW std_logic;
  signal ReadDataW std_logic;
  signal ALUOutW std_logic_vector(31 downto 0);
  signal WA3W std_logic_vector(3 downto 0);


begin
  cont : controller port map(
    clk, reset, instr(31 downto 12),
    --ALUFlags, 
    
    RegSrc, RegWriteD,--RegWrite, 
    ImmSrc,
    ALUSrc, ALUControl, MemWriteD,--MemWrite,
    MemtoReg, PCSrcD--PCSrc
  );

  dp : datapath port map(
    clk, reset, RegSrc, RegWrite, ImmSrc,
    ALUSrc, ALUControl, MemtoReg, PCSrc,
    ALUFlags, PC, instr, ALUResult,
    WriteData, ReadData
  );

  cl : cond_unit port map(
    clk, reset, condE,--instr(31 downto 28),
    ALUFlags, FlagWriteE,
    PCSrcE, RegWriteE, MemWriteE, -- entradas transplantadas
    PCSrc, RegWrite, MemWrite
  );
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity controller is -- single cycle control decoder
  port (
    clk, reset : in std_logic;
    instr : in std_logic_vector(31 downto 12);
    --ALUFlags : in std_logic_vector(3 downto 0);

    RegSrc : out std_logic_vector(1 downto 0);

    RegWrite : out std_logic;
    ImmSrc : out std_logic_vector(1 downto 0);
    ALUSrc : out std_logic;
    ALUControl : out std_logic_vector(1 downto 0);
    MemWrite : out std_logic;
    MemtoReg : out std_logic;
    PCSrc : out std_logic;
    
    -- Sinais a mais pra poder controlar o fluxo das instrucoes
    FlagWrite, Branch : out std_logic
  );
end;

architecture struct OF controller is
  component decoder
    port (
      Op : in std_logic_vector(1 downto 0);
      Funct : in std_logic_vector(5 downto 0);
      Rd : in std_logic_vector(3 downto 0);

      FlagW : out std_logic_vector(1 downto 0);

      PCS, RegW, MemW : out std_logic;
      MemtoReg, ALUSrc : out std_logic;
      ImmSrc, RegSrc : out std_logic_vector(1 downto 0);
      ALUControl : out std_logic_vector(1 downto 0));
  end component;

  signal FlagW : std_logic_vector(1 downto 0);
  signal PCS, RegW, MemW : std_logic;

begin
  dec : decoder port map(
    instr(27 downto 26), instr(25 downto 20),
    instr(15 downto 12), FlagW, PCS,
    RegW, MemW, MemtoReg, ALUSrc, ImmSrc,
    RegSrc, ALUControl);

   PCSrc <= PCS; 
   RegWrite <= RegW; 
   MemWrite <= MemW; 
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity decoder is -- main control decoder
  port (
    Op : in std_logic_vector(1 downto 0);
    Funct : in std_logic_vector(5 downto 0);
    Rd : in std_logic_vector(3 downto 0);
    FlagW : out std_logic_vector(1 downto 0);
    PCS, RegW, MemW : out std_logic;
    MemtoReg, ALUSrc : out std_logic;
    ImmSrc, RegSrc : out std_logic_vector(1 downto 0);
    ALUControl : out std_logic_vector(1 downto 0));
end;

architecture behave OF decoder is
  signal controls : std_logic_vector(9 downto 0);
  signal ALUOp, Branch : std_logic;
  signal op2 : std_logic_vector(3 downto 0);
begin
  op2 <= (Op, Funct(5), Funct(0));
  PROCESS (all) begin -- Main Decoder
    CASE ? (op2) is
      WHEN "000-" => controls <= "0000001001";
      WHEN "001-" => controls <= "0000101001";
      WHEN "01-0" => controls <= "1001110100";
      WHEN "01-1" => controls <= "0001111000";
      WHEN "10--" => controls <= "0110100010";
      WHEN OTHERS => controls <= "----------";
    end CASE?;
  end PROCESS;

  (RegSrc, ImmSrc, ALUSrc, MemtoReg, RegW, MemW,
  Branch, ALUOp) <= controls;

  PROCESS (all) begin -- ALU Decoder
    IF (ALUOp) THEN
      CASE Funct(4 downto 1) is
        WHEN "0100" => ALUControl <= "00"; -- ADD
        WHEN "0010" => ALUControl <= "01"; -- SUB
        WHEN "0000" => ALUControl <= "10"; -- AND
        WHEN "1100" => ALUControl <= "11"; -- ORR
        WHEN OTHERS => ALUControl <= "--"; -- unimplemented
      end CASE;
      FlagW(1) <= Funct(0);
      FlagW(0) <= Funct(0) AND (NOT ALUControl(1));
    ELSE
      ALUControl <= "00";
      FlagW <= "00";
    end IF;
  end PROCESS;

  PCS <= ((AND Rd) AND RegW) OR Branch;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity cond_unit is -- Conditional logic
  port (
    clk, reset : in std_logic;

    Cond : in std_logic_vector(3 downto 0);
    ALUFlags : in std_logic_vector(3 downto 0);
    FlagW : in std_logic_vector(1 downto 0);
    PCS, RegW, MemW : in std_logic;

    PCSrc, RegWrite : out std_logic;
    MemWrite : out std_logic);
end;

architecture behave OF cond_unit is
  component condcheck
    port (
      Cond : in std_logic_vector(3 downto 0);
      Flags : in std_logic_vector(3 downto 0);
      CondEx : out std_logic);
  end component;
  component flopenr GENERIC (width : inTEGER);
    port (
      clk, reset, en : in std_logic;
      d : in std_logic_vector(width - 1 downto 0);
      q : out std_logic_vector(width - 1 downto 0));
  end component;
  signal FlagWrite : std_logic_vector(1 downto 0);
  signal Flags : std_logic_vector(3 downto 0);
  signal CondEx : std_logic;
begin
  flagreg1 : flopenr GENERIC map(2)
  port map(
    clk, reset, FlagWrite(1),
    ALUFlags(3 downto 2), Flags(3 downto 2));

  flagreg0 : flopenr GENERIC map(2)
  port map(
    clk, reset, FlagWrite(0),
    ALUFlags(1 downto 0), Flags(1 downto 0));
  cc : condcheck port map(Cond, Flags, CondEx);

  FlagWrite <= FlagW AND (CondEx, CondEx);
  RegWrite <= RegW AND CondEx;
  MemWrite <= MemW AND CondEx;
  PCSrc <= PCS AND CondEx;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity condcheck is
  port (
    Cond : in std_logic_vector(3 downto 0);
    Flags : in std_logic_vector(3 downto 0);
    CondEx : out std_logic);
end;

architecture behave OF condcheck is
  signal neg, zero, carry, overflow, ge : std_logic;
begin
  (neg, zero, carry, overflow) <= Flags;
  ge <= (neg XNOR overflow);

  PROCESS (all) begin -- Condition checking
    CASE Cond is
      WHEN "0000" => CondEx <= zero;
      WHEN "0001" => CondEx <= NOT zero;
      WHEN "0010" => CondEx <= carry;
      WHEN "0011" => CondEx <= NOT carry;
      WHEN "0100" => CondEx <= neg;
      WHEN "0101" => CondEx <= NOT neg;
      WHEN "0110" => CondEx <= overflow;
      WHEN "0111" => CondEx <= NOT overflow;
      WHEN "1000" => CondEx <= carry AND (NOT zero);
      WHEN "1001" => CondEx <= NOT(carry AND (NOT zero));
      WHEN "1010" => CondEx <= ge;
      WHEN "1011" => CondEx <= NOT ge;
      WHEN "1100" => CondEx <= (NOT zero) AND ge;
      WHEN "1101" => CondEx <= NOT ((NOT zero) AND ge);
      WHEN "1110" => CondEx <= '1';
      WHEN OTHERS => CondEx <= '-';
    end CASE;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity datapath is
  port (
    clk, reset : in std_logic;
    RegSrc : in std_logic_vector(1 downto 0);
    RegWrite : in std_logic;
    ImmSrc : in std_logic_vector(1 downto 0);
    ALUSrc : in std_logic;
    ALUControl : in std_logic_vector(1 downto 0);
    MemtoReg : in std_logic;
    PCSrc : in std_logic;
    ALUFlags : out std_logic_vector(3 downto 0);
    PC : BUFFER std_logic_vector(31 downto 0);
    instr : in std_logic_vector(31 downto 0);
    ALUResult, WriteData : BUFFER std_logic_vector(31 downto 0);
    ReadData : in std_logic_vector(31 downto 0));
end;

architecture struct OF datapath is
  component alu
    port (
      a, b : in std_logic_vector(31 downto 0);
      ALUControl : in std_logic_vector(1 downto 0);
      Result : BUFFER std_logic_vector(31 downto 0);
      ALUFlags : out std_logic_vector(3 downto 0));
  end component;
  component regfile
    port (
      clk : in std_logic;
      we3 : in std_logic;
      ra1, ra2, wa3 : in std_logic_vector(3 downto 0);
      wd3, r15 : in std_logic_vector(31 downto 0);
      rd1, rd2 : out std_logic_vector(31 downto 0));
  end component;
  component adder
    port (
      a, b : in std_logic_vector(31 downto 0);
      y : out std_logic_vector(31 downto 0));
  end component;
  component extend
    port (
      instr : in std_logic_vector(23 downto 0);
      ImmSrc : in std_logic_vector(1 downto 0);
      ExtImm : out std_logic_vector(31 downto 0));
  end component;
  component flopr GENERIC (width : inTEGER);
    port (
      clk, reset : in std_logic;
      d : in std_logic_vector(width - 1 downto 0);
      q : out std_logic_vector(width - 1 downto 0));
  end component;
  component mux2 GENERIC (width : inTEGER);
    port (
      d0, d1 : in std_logic_vector(width - 1 downto 0);
      s : in std_logic;
      y : out std_logic_vector(width - 1 downto 0));
  end component;
  signal PCNext, PCPlus4, PCPlus8 : std_logic_vector(31 downto 0);
  signal ExtImm, Result : std_logic_vector(31 downto 0);
  signal SrcA, SrcB : std_logic_vector(31 downto 0);
  signal RA1, RA2 : std_logic_vector(3 downto 0);
begin
  -- next PC logic
  pcmux : mux2 GENERIC map(32)
  port map(PCPlus4, Result, PCSrc, PCNext);
  pcreg : flopr GENERIC map(32) port map(clk, reset, PCNext, PC);
  pcadd1 : adder port map(PC, X"00000004", PCPlus4);
  pcadd2 : adder port map(PCPlus4, X"00000004", PCPlus8);

  -- register file logic
  ra1mux : mux2 GENERIC map(4)
  port map(instr(19 downto 16), "1111", RegSrc(0), RA1);
  ra2mux : mux2 GENERIC map(
    4) port map(instr(3 downto 0),
    instr(15 downto 12), RegSrc(1), RA2);
  rf : regfile port map(
    clk, RegWrite, RA1, RA2,
    instr(15 downto 12), Result,
    PCPlus8, SrcA, WriteData);
  resmux : mux2 GENERIC map(32)
  port map(ALUResult, ReadData, MemtoReg, Result);
  ext : extend port map(instr(23 downto 0), ImmSrc, ExtImm);

  -- ALU logic
  srcbmux : mux2 GENERIC map(32)
  port map(WriteData, ExtImm, ALUSrc, SrcB);
  i_alu : alu port map(SrcA, SrcB, ALUControl, ALUResult, ALUFlags);
end;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity regfile is -- three-port register file
  port (
    clk : in std_logic;
    we3 : in std_logic;
    ra1, ra2, wa3 : in std_logic_vector(3 downto 0);
    wd3, r15 : in std_logic_vector(31 downto 0);
    rd1, rd2 : out std_logic_vector(31 downto 0));
end;

architecture behave OF regfile is
  TYPE ramtype is ARRAY (31 downto 0) OF
  std_logic_vector(31 downto 0);
  signal mem : ramtype;
begin
  PROCESS (clk) begin
    IF rising_edge(clk) THEN
      IF we3 = '1' THEN
        mem(to_integer(wa3)) <= wd3;
      end IF;
    end IF;
  end PROCESS;
  PROCESS (all) begin
    IF (to_integer(ra1) = 15) THEN
      rd1 <= r15;
    ELSE
      rd1 <= mem(to_integer(ra1));
    end IF;
    IF (to_integer(ra2) = 15) THEN
      rd2 <= r15;
    ELSE
      rd2 <= mem(to_integer(ra2));
    end IF;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity adder is -- adder
  port (
    a, b : in std_logic_vector(31 downto 0);
    y : out std_logic_vector(31 downto 0));
end;

architecture behave OF adder is
begin
  y <= a + b;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity extend is
  port (
    instr : in std_logic_vector(23 downto 0);
    ImmSrc : in std_logic_vector(1 downto 0);
    ExtImm : out std_logic_vector(31 downto 0));
end;

architecture behave OF extend is
begin
  PROCESS (all) begin
    CASE ImmSrc is
      WHEN "00" => ExtImm <= (X"000000", instr(7 downto 0));
      WHEN "01" => ExtImm <= (X"00000", instr(11 downto 0));
      WHEN "10" => ExtImm <= (instr(23), instr(23), instr(23),
        instr(23), instr(23), instr(23), instr(23 downto 0), "00");
      WHEN OTHERS => ExtImm <= X"--------";
    end CASE;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity flopenr is -- flip-flop with enable and asynchronous reset
  GENERIC (width : inTEGER);
  port (
    clk, reset, en : in std_logic;
    d : in std_logic_vector(width - 1 downto 0);
    q : out std_logic_vector(width - 1 downto 0));
end;

architecture asynchronous OF flopenr is
begin
  PROCESS (clk, reset) begin
    IF reset THEN
      q <= (OTHERS => '0');
    ELSIF rising_edge(clk) THEN
      IF en THEN
        q <= d;
      end IF;
    end IF;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity flopr is -- flip-flop with asynchronous reset
  GENERIC (width : inTEGER);
  port (
    clk, reset : in std_logic;
    d : in std_logic_vector(width - 1 downto 0);
    q : out std_logic_vector(width - 1 downto 0));
end;

architecture asynchronous OF flopr is
begin
  PROCESS (clk, reset) begin
    IF reset THEN
      q <= (OTHERS => '0');
    ELSIF rising_edge(clk) THEN
      q <= d;
    end IF;
  end PROCESS;
end;

library IEEE;
use IEEE.std_logic_1164.all;
entity mux2 is -- two-input multiplexer
  GENERIC (width : inTEGER);
  port (
    d0, d1 : in std_logic_vector(width - 1 downto 0);
    s : in std_logic;
    y : out std_logic_vector(width - 1 downto 0));
end;

architecture behave OF mux2 is
begin
  y <= d1 WHEN s ELSE
    d0;
end;