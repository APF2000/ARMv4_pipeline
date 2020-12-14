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
      WriteData, DataAdr : out std_logic_vector(31 downto 0);
      MemWrite : out std_logic
    );
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
  PROCESS (clk, MemWrite, DataAdr, WriteData) begin
    IF (clk'event AND clk = '0' AND MemWrite = '1') THEN
      IF (to_integer(DataAdr) = 100 AND
        to_integer(WriteData) = 7) THEN
        REport "NO ERRORS: Simulation succeeded" SEVERITY failure;
      ELSIF (DataAdr /= 96) THEN
        REport "Simulation failed" SEVERITY failure;
      end IF;
    end IF;
  end PROCESS;
end architecture;

-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- TB TOP 2 [VERIFICAR] PRECISA DE UM MEMFILE.DAT DIFERENTE
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity tb_top_2 is
end;

architecture tb of tb_top_2 is
  component top
    port (
      clk, reset : in std_logic;
      WriteData, DatAadr : out std_logic_vector(31 downto 0);
      MemWrite : out std_logic
    );
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
  PROCESS (clk, MemWrite, DataAdr, WriteData) begin
    IF (clk'event AND clk = '0' AND MemWrite = '1') THEN
      IF (to_integer(DataAdr) = 100 AND
        to_integer(WriteData) = 7) THEN
        REport "NO ERRORS: Simulation succeeded" SEVERITY failure;
      ELSIF (DataAdr /= 96) THEN
        REport "Simulation failed" SEVERITY failure;
      end IF;
    end IF;
  end PROCESS;
end architecture;

-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- TOP
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity top is -- top-level design for testing
  port (
    clk, reset : in std_logic;
    WriteData, DataAdr : BUFFER std_logic_vector(31 downto 0);
    MemWrite : BUFFER std_logic
  );
end;

architecture test OF top is
  component arm
    port (
      clk, reset : in std_logic;
      PC : out std_logic_vector(31 downto 0);
      instr : in std_logic_vector(31 downto 0);
      MemWrite : out std_logic;
      ALUResult, WriteData : out std_logic_vector(31 downto 0);
      ReadData : in std_logic_vector(31 downto 0)
    );
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
  i_arm : arm
  port map
  (
    clk => clk,
    reset => reset,
    PC => PC,
    instr => instr,
    MemWrite => MemWrite,
    ALUResult => DataAdr,
    WriteData => WriteData,
    ReadData => ReadData
  );

  i_imem : imem port map(
    a => PC,
    rd => instr
  );

  i_dmem : dmem port map(
    clk => clk,
    we => MemWrite,
    a => DataAdr,
    wd => WriteData,
    rd => ReadData
  );
end;

-------------------------------------------------------------------------
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- PARTIAL_IF_ID
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity partial_IF_ID is
  port (
    clock, reset : in std_logic;
    instrF : in std_logic_vector(31 downto 0);
    stallD, flushD : in std_logic;

    instrD : out std_logic_vector(31 downto 0)
  );
end entity;

architecture arch of partial_IF_ID is

  component flopenr -- flip-flop with enable and asynchronous reset
  generic (width : integer);
  port (
    clk, reset, en : in std_logic;
    d : in std_logic_vector(width - 1 downto 0);
    q : out std_logic_vector(width - 1 downto 0));
  end component;

  signal s_instr : std_logic_vector(31 downto 0);
  signal s_not_stall, s_flush : std_logic;

begin
  flnr_IF_ID_0 : flopenr
  generic map(width => 32)
  port map
  (
    clk => clock,
    reset => s_flush,
    en => s_not_stall,
    d => instrF,
    q => instrD
  );

  s_not_stall <= not stallD;
  s_flush <= flushD or reset;

end architecture;

--------------------------------------------------------------------------
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- PARTIAL_ID_EX
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity partial_ID_EX is
  port (
    clock, reset : in std_logic;

    PCSrcD, RegWriteD : in std_logic;
    MemtoRegD, MemWriteD : in std_logic;
    ALUControlD, FlagWriteD : in std_logic_vector(1 downto 0);
    BranchD, ALUSrcD : in std_logic;
    RD1D, RD2D, ExtImmD : in std_logic_vector(31 downto 0);
    WA3D : in std_logic_vector(3 downto 0);
    CondD: in std_logic_vector(3 downto 0);
    FlagsD : in std_logic_vector(3 downto 0);--[nao precisa mais ver tamanho]

    FLushE : in std_logic;
    RA1D, RA2D : in std_logic_vector(3 downto 0);
    
    RA1E, RA2E : out std_logic_vector(3 downto 0);

    PCSrcE, RegWriteE : out std_logic;
    MemtoRegE, MemWriteE : out std_logic;
    ALUControlE, FlagWriteE : out std_logic_vector(1 downto 0);
    BranchE, ALUSrcE : out std_logic;
    RD1E, RD2E, ExtImmE : out std_logic_vector(31 downto 0);
    WA3E : out std_logic_vector(3 downto 0);
    CondE : out std_logic_vector(3 downto 0);
    FlagsE : out std_logic_vector(3 downto 0) -- [nao precisa mais ver o tamanho]
  );
end entity;

architecture arch OF partial_ID_EX is

  component flopenr -- flip-flop with enable and asynchronous reset
  generic (width : integer);
  port (
    clk, reset, en : in std_logic;
    d : in std_logic_vector(width - 1 downto 0);
    q : out std_logic_vector(width - 1 downto 0));
  end component;

  signal s_in: std_logic_vector(125 downto 0);
  signal s_out: std_logic_vector(125 downto 0);
  signal s_flush : std_logic;

begin

  flnr_ID_EX : flopenr
  generic map(width => 126)
  port map
  (
    clk => clock,
    reset => s_flush,
    en => '1',
    d => s_in,
    q => s_out
  );
  s_flush <= flushE or reset;
  
  s_in (125 downto 94) <= RD1D;
  s_in (93 downto 62) <= RD2D;
  s_in (61 downto 30) <= ExtImmD;
  s_in (29 downto 26) <= WA3D;
  s_in (25 downto 22) <= CondD;
  s_in (21 downto 18) <= FlagsD;
  s_in (17 downto 14) <= RA1D;
  s_in (13 downto 10) <= RA2D;
  s_in (9 downto 8) <= ALUControlD;
  s_in (7 downto 6) <= FlagWriteD;
  s_in (5) <= PCSrcD;
  s_in (4) <= RegWriteD;
  s_in (3) <= MemtoRegD;
  s_in (2) <= MemWriteD;
  s_in (1) <= BranchD;
  s_in (0) <= ALUSrcD;

   
  RD1E <= s_out (125 downto 94);
  RD2E <= s_out (93 downto 62);
  ExtImmE <= s_out (61 downto 30);
  WA3E <= s_out (29 downto 26);
  CondE <= s_out (25 downto 22);
  FlagsE <= s_out (21 downto 18);
  RA1E <= s_out (17 downto 14);
  RA2E <= s_out (13 downto 10);
  ALUControlE <= s_out (9 downto 8);
  FlagWriteE <= s_out (7 downto 6);
  PCSrcE <= s_out (5);
  RegWriteE <= s_out (4);
  MemtoRegE <= s_out (3);
  MemWriteE <= s_out (2);
  BranchE <= s_out (1);
  ALUSrcE <= s_out (0);


end architecture;

------------------------------------------------------------------
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- PARTIAL_EX_MEM
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity partial_EX_MEM is
  port (
    clock, reset : in std_logic;

    PCSrcE, RegWriteE, MemtoRegE, MemWriteE : in std_logic; -- Sinais combinatorios
    ALUResultE, WriteDataE : in std_logic_vector(31 downto 0);
    WA3E : in std_logic_vector(3 downto 0);

    PCSrcM, RegWriteM, MemtoRegM, MemWriteM : out std_logic; -- Sinais combinatorios
    ALUResultM, WriteDataM : out std_logic_vector(31 downto 0);
    WA3M : out std_logic_vector(3 downto 0)
  );
end entity;

architecture arch of partial_EX_MEM is

  component flopenr -- flip-flop with enable and asynchronous reset
  generic (width : integer);
  port (
    clk, reset, en : in std_logic;
    d : in std_logic_vector(width - 1 downto 0);
    q : out std_logic_vector(width - 1 downto 0));
  end component;

  signal s_in: std_logic_vector(71 downto 0);
  signal s_out: std_logic_vector(71 downto 0);

begin

  flnr_EX_MEM : flopenr
  generic map(width => 72)
  port map
  (
    clk => clock,
    reset => reset,
    en => '1',
    d => s_in,
    q => s_out
  );

  s_in (71 downto 40) <= ALUResultE;
  s_in (39 downto 8) <= WriteDataE;
  s_in (7 downto 4) <= WA3E; 
  s_in (3) <= PCSrcE;
  s_in (2) <= RegWriteE;
  s_in (1) <= MemtoRegE;
  s_in (0) <= MemWriteE;

  ALUResultM <= s_out  (71 downto 40);
  WriteDataM <= s_out  (39 downto 8);
  WA3M <= s_out  (7 downto 4); 
  PCSrcM <= s_out  (3);
  RegWriteM <= s_out  (2);
  MemtoRegM <= s_out  (1);
  MemWriteM <= s_out  (0);

end architecture;

--------------------------------------------------------
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- PARTIAL_MEM_WB
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity partial_MEM_WB is
  port (
    clock : in std_logic;
    reset : in std_logic;

    PCSrcM : in std_logic;
    RegWriteM : in std_logic;
    MemtoRegM : in std_logic;
    ALUOutM   : in std_logic_vector(31 downto 0);
    WA3M      : in std_logic_vector(3 downto 0);
    RD      : in std_logic_vector(31 downto 0);

    PCSrcW : out std_logic;
    RegWriteW : out std_logic;
    MemtoRegW : out std_logic;
    ReadDataW : out std_logic_vector(31 downto 0);
    ALUOutW   : out std_logic_vector(31 downto 0);
    WA3W      : out std_logic_vector(3 downto 0)
  );
end entity;

architecture arch OF partial_MEM_WB is
  
  component flopenr -- flip-flop with enable and asynchronous reset
  generic (width : integer);
  port (
    clk, reset, en : in std_logic;
    d : in std_logic_vector(width - 1 downto 0);
    q : out std_logic_vector(width - 1 downto 0));
  end component;


  signal s_in: std_logic_vector(70 downto 0);
  signal s_out: std_logic_vector(70 downto 0);

begin

  flnr_MEM_WB : flopenr
  generic map(width => 71)
  port map
  (
    clk => clock,
    reset => reset,
    en => '1',
    d => s_in,
    q => s_out
  );

  s_in (70 downto 39) <= ALUOutM;
  s_in (38 downto 7) <= RD;
  s_in (6 downto 3) <= WA3M; 
  s_in (2) <= PCSrcM;
  s_in (1) <= RegWriteM;
  s_in (0) <= MemtoRegM;

  ALUOutW <= s_out  (70 downto 39);
  ReadDataW <= s_out  (38 downto 7);
  WA3W <= s_out  (6 downto 3); 
  PCSrcW <= s_out  (2);
  RegWriteW <= s_out  (1);
  MemtoRegW <= s_out  (0); 

end architecture;

-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- TESTBENCH HAZARD_UNIT
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- library IEEE;
-- use IEEE.std_logic_1164.all;
-- use STD.TEXTIO.all;
-- use IEEE.NUMERIC_STD_UNSIGNED.all;


-- entity tb_hazard_unit is
-- end entity;

-- architecture arch of tb_hazard_unit is
-- 	component hazard_unit is
-- 		 port (
-- 			 clock : in std_logic;
-- 			 reset : in std_logic;
-- 			 Match : in std_logic_vector(4 downto 0);

-- 			 PCWrPendingF : in std_LOGIC;
-- 			 PCSrcW : in std_logic;
-- 			 BranchTakenE : in std_LOGIC;

-- 			RegWriteM : in std_logic;
-- 			RegWriteW : in std_logic;
-- 			MemToRegE : in std_logic;

-- 			 StallF : out std_logic;
-- 			 StallD : out std_logic;

-- 			 FlushD : out std_logic;
-- 			 FlushE : out std_logic;
-- 			 ForwardAE : out std_logic_vector(1 downto 0);
-- 			 ForwardBE : out std_logic_vector(1 downto 0)
-- 		);
--   end component;

--   component hazard_logic is
-- 		port (
-- 		 -- ENTRADAS
-- 		 clock : in std_logic;
-- 		 reset : in std_logic;
-- 		 RA1D : in std_logic_vector(3 downto 0);
-- 		 RA2D : in std_logic_vector(3 downto 0);
-- 		 RA1E : in std_logic_vector(3 downto 0);
-- 		 RA2E : in std_logic_vector(3 downto 0);
-- 		 WA3E : in std_logic_vector(3 downto 0);
-- 		 WA3M : in std_logic_vector(3 downto 0);
-- 		 WA3W : in std_logic_vector(3 downto 0);
-- 		 PCSrcD : in std_logic;
-- 		 PCSrcE : in std_logic;
-- 		 PCSrcM : in std_logic;
-- 		 -- SAIDAS
-- 		 -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
-- 		 Match : out std_logic_vector(4 downto 0);
-- 		 PCWrPendingF: out std_logic
-- 	  );
-- 	end component;

--   signal clock : std_logic;
--   signal reset : std_logic;
--   signal RegWriteM : std_logic;
--   signal RegWriteW : std_logic;
--   signal MemToRegE : std_logic;
--   signal StallF : std_logic;
--   signal StallD : std_logic;
--   signal FlushD : std_logic;
--   signal FlushE : std_logic;
--   signal ForwardAE : std_logic_vector(1 downto 0);
--   signal ForwardBE : std_logic_vector(1 downto 0);

--   signal RA1D : std_logic_vector(3 downto 0);
--   signal RA2D : std_logic_vector(3 downto 0);
--   signal RA1E : std_logic_vector(3 downto 0);
--   signal RA2E : std_logic_vector(3 downto 0);
--   signal WA3E : std_logic_vector(3 downto 0);
--   signal WA3M : std_logic_vector(3 downto 0);
--   signal WA3W : std_logic_vector(3 downto 0);
--   signal PCSrcD : std_logic;
--   signal PCSrcE : std_logic;
--   signal PCSrcM : std_logic;
--   -- SAIDAS
--   -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
--   signal Match : std_logic_vector(4 downto 0);
--   signal PCWrPendingF: std_logic;

--   signal s_simulando : std_logic := '0';
  
--   signal PCSrcW : std_logic;
--   signal BranchTakenE : std_LOGIC;

-- begin

--   -- instantiate device to be tested
-- 	h_uni : hazard_unit
--   		 port map (
-- 			 clock => clock,
-- 			 reset => reset,

-- 			 RegWriteM => RegWriteM,
-- 			 RegWriteW => RegWriteW,
-- 			 MemToRegE => MemToRegE,

-- 			 StallF => StallF,
-- 			 StallD => StallD,
-- 			 FlushD => FlushD,
-- 			 FlushE => FlushE,

-- 			 ForwardAE => ForwardAE,
-- 			 ForwardBE => ForwardBE,

-- 			 Match => Match,

-- 			 PCWrPendingF => PCWrPendingF,
-- 			 PCSrcW => PCSrcW,
-- 			 BranchTakenE => BranchTakenE
-- 		);

-- 	h_logic : hazard_logic
-- 	port map (
-- 		 -- ENTRADAS
-- 		 clock => clock,
-- 		 reset => reset,
-- 		 RA1D => RA1D,
-- 		 RA2D => RA2D,
-- 		 RA1E => RA1E,
-- 		 RA2E => RA2E,
-- 		 WA3E => WA3E,
-- 		 WA3M => WA3M,
-- 		 WA3W => WA3W,
-- 		 PCSrcD => PCSrcD,
-- 		 PCSrcE => PCSrcE,
-- 		 PCSrcM => PCSrcM,
-- 		 -- SAIDAS
-- 		 -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
-- 		 Match => Match,
-- 		 PCWrPendingF => PCWrPendingF
-- 	 );

--   -- Generate clock with 10 ns period
--   PROCESS begin
--     clock <= '1';
--     WAIT FOR 5 ns;
--     clock <= '0';
--     WAIT FOR 5 ns;
--   end PROCESS;


--   -- check that 7 gets written to address 84
--   -- at end of program
--   verif : PROCESS
--   (clock,   reset,   RA1E,   RA2E,   WA3M,
--     RegWriteM,   RegWriteW,   MemToRegE, s_simulando, PCWrPendingF,
-- 	 PCSrcW, BranchTakenE, Match)
--   begin
--     IF (clock'event AND clock = '0') THEN
--       IF (s_simulando = '1') THEN
-- 			elsif( (Match(2) and RegWriteM) = '1') then assert ( ForwardAE = "10" ) report "ForwardA errado" severity error;
-- 			elsif( (Match(3) and RegWriteM) = '1') then assert ( ForwardAE = "01" ) report "ForwardA errado" severity error;
-- 			else assert ( ForwardAE = "00" ) report "ForwardA errado" severity error; end if;

-- 			elsif( (Match(2) and RegWriteM) = '1') then assert ( ForwardBE = "10" ) report "ForwardB errado" severity error;
-- 			elsif( (Match(3) and RegWriteM) = '1') then assert ( ForwardBE = "01" ) report "ForwardB errado" severity error;
-- 			else assert ( ForwardBE = "00" ) report "ForwardB errado" severity error; end if;


-- 			assert (PCWrPendingF = (PCSrcD or PCSrcE or PCSrcM) ) report "PCWr errado" severity error;
-- 			assert (FlushD = (PCWrPendingF or PCSrcW or BranchTakenE) ) report "FlushD errado" severity error;
-- --			StallD = LDRstall;
-- --			StallF = LDRstall + PCWrPendingF;
-- --			FlushE = LDRstall + BranchTakenE;

--         --REport "NO ERRORS: Simulation succeeded" SEVERITY note;
--       --ELSIF () THEN
--       --end IF;
--     end IF;
--   end PROCESS;

-- 	main_sim : PROCESS
--   (all)--clock,   reset,   RA1E,   RA2E,   WA3M,
--     --RegWriteM,   RegWriteW,   MemToRegE)
-- 	begin
-- 	 reset <= '1';
--     WAIT FOR 22 ns;
--     reset <= '0';
--     WAIT;

-- 	 s_simulando <= '1';

-- 	 ------------------------
-- 	 --  Teste1 : ID: ADD, EX: ADD, MEM: ADD
-- 	 ------------------------
-- 	 RA1E <= X"A";
-- 	 RA2E <= X"3"; -- conflito com wa3w

-- 	 RA1D <= X"C";
-- 	 RA2D <= X"2"; -- conflito com wa3m

-- 	RegWriteM <= '1';
-- 	RegWriteW <= '1';
-- 	MemToRegE <= '1';

-- 	PCSrcW <= '0'; -- Sem branch
-- 	PCSrcD <= '0';
-- 	PCSrcE <= '0';
-- 	PCSrcM <= '0';
-- 	BranchTakenE <= '0';

-- 	WA3E <= x"1"; -- Endereco de write back da instr

-- 	WA3M <= x"2"; -- Hazard
-- 	WA3W <= x"3"; -- Hazard

-- 	assert ( false ) report "Teste 1" severity note;

-- 	assert ( PCWrPendingF = '0' ) report "PCWr errado" severity error;

-- 	-- Esperar terminar instr e apagar o que estava para ir pro EX
-- 	assert ( StallF = '1' ) report "StallF errado" severity error;
-- 	assert ( StallD = '1' ) report "StallD errado" severity error;

-- 	assert ( FlushD = '0' ) report "FlushD errado" severity error;
-- 	assert ( FlushE = '1' ) report "FlushE errado" severity error;


-- 	 ------------------------------------------
-- 	 --  Teste2 IF: SUM, ID: SUM, EX: LDR, MEM: SUM
-- 	 ------------------------------------------
-- 	 RA1E <= X"A";
-- 	 RA2E <= X"2"; -- conflito com wa3m

-- 	 RA1D <= X"C";
-- 	 RA2D <= X"1"; -- conflito com wa3e : livro pag 436

-- 	RegWriteM <= '1';
-- 	RegWriteW <= '0';
-- 	MemToRegE <= '1';

-- 	PCSrcW <= '0'; -- Sem branch
-- 	PCSrcD <= '0';
-- 	PCSrcE <= '0';
-- 	PCSrcM <= '0';
-- 	BranchTakenE <= '0';

-- 	WA3E <= x"1"; -- Endereco de write back da instr

-- 	WA3M <= x"2"; -- Hazard
-- 	WA3W <= x"3"; -- Hazard

-- 	assert ( false ) report "Teste 2" severity error;

-- 	assert ( PCWrPendingF = '0' ) report "PCWr errado" severity error;

-- 	assert ( StallF = '1' ) report "StallF errado" severity error;
-- 	assert ( StallD = '1' ) report "StallD errado" severity error;

-- 	assert ( FlushD = '0' ) report "FlushD errado" severity error; -- [VERIFICAR] zero ou um ??
-- 	assert ( FlushE = '1' ) report "FlushE errado" severity error;


-- 	---------------------------------------------------------------------

-- 	 s_simulando <= '0';

-- 	end process;

-- end architecture;*/


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- HAZARD_UNIT
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity hazard_unit is
  port (
    -- ENTRADAS
    clock : in std_logic;
    reset : in std_logic;
    -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
    Match : in std_logic_vector(4 downto 0);
    PCWrPendingF: in std_logic;

    RegWriteM : in std_logic;
    RegWriteW : in std_logic;
    MemToRegE : in std_logic;

    PCSrcW : in std_logic;
    BranchTakenE : in std_logic;
    -- SAIDAS
    StallF : out std_logic;
    StallD : out std_logic;
    FlushD : out std_logic;
    FlushE : out std_logic;
    ForwardAE : out std_logic_vector(1 downto 0);
    ForwardBE : out std_logic_vector(1 downto 0)
  );
end entity;

architecture arch_hazard_unit OF hazard_unit is

  signal Match_1E_M : std_logic;
  signal Match_1E_W : std_logic;
  signal Match_2E_M : std_logic;
  signal Match_2E_W : std_logic;
  signal Match_12D_E: std_logic;
  signal LDRStall: std_logic;

begin
  Match_1E_M <= Match(0);
  Match_1E_W <= Match(1);
  Match_2E_M <= Match(2);
  Match_2E_W <= Match(3);
  Match_12D_E <= Match(4);

  -- Dar um stall quando instrucao LDR e o Reg de escrita em Execution e o mesmo que um dos operandos em Decode
  LDRStall <= Match_12D_E and MemToRegE;

  -- Saidas
  StallD <= LDRStall;
  StallF <= LDRStall or PCWrPendingF;
  FlushE <= LDRStall or BranchTakenE;
  FlushD <= PCWrPendingF or PCSrcW or BranchTakenE;

  ForwardAE(1) <= '1' when ( (Match_1E_M and RegWriteM) = '1')
                      else '0';
  ForwardAE(0) <= '1' when ( (Match_1E_W and RegWriteW and (not ForwardAE(1)))= '1' )
                      else '0';
  ForwardBE(1) <= '1' when ((Match_2E_M and RegWriteM)='1')
                      else '0';
  ForwardBE(0) <= '1' when ( (Match_2E_W and RegWriteW and (not ForwardBE(1))) = '1')
                      else '0';
end;

-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- HAZARD_LOGIC
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
use STD.TEXTIO.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity hazard_logic is
  port (
    -- ENTRADAS
    clock : in std_logic;
    reset : in std_logic;
    RA1D : in std_logic_vector(3 downto 0);
    RA2D : in std_logic_vector(3 downto 0);
    RA1E : in std_logic_vector(3 downto 0);
    RA2E : in std_logic_vector(3 downto 0);
    WA3E : in std_logic_vector(3 downto 0);
    WA3M : in std_logic_vector(3 downto 0);
    WA3W : in std_logic_vector(3 downto 0);
    PCSrcD : in std_logic;
    PCSrcE : in std_logic;
    PCSrcM : in std_logic;
    -- SAIDAS
    -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
    Match : out std_logic_vector(4 downto 0);
    PCWrPendingF: out std_logic
  );
end entity;

architecture arch_hazard_logic OF hazard_logic is

  signal Match_1E_M : std_logic;
  signal Match_1E_W : std_logic;
  signal Match_2E_M : std_logic;
  signal Match_2E_W : std_logic;

  signal Match_12D_E: std_logic;
  signal Match_12D_Ea: std_logic;
  signal Match_12D_Eb: std_logic;


begin
  -- Comparar se Reg 1 de Execution e o mesmo que o reg de escrita em Memory
  Match_1E_M <= '1'when RA1E = WA3M else '0';
  -- Comparar se Reg 1 de Execution e o mesmo que o reg de escrita em WriteBack
  Match_1E_W <= '1'when RA1E = WA3W else '0';
  -- Comparar se Reg 2 de Execution e o mesmo que o reg de escrita em Memory
  Match_2E_M <= '1'when RA2E = WA3M else '0';
  -- Comparar se Reg 2 de Execution e o mesmo que o reg de escrita em WriteBack
  Match_2E_W <= '1'when RA2E = WA3W else '0';

  -- Comparar se Reg 1 de Decode e o mesmo que o reg de escrita em Execution
  Match_12D_Ea <= '1'when RA1D = WA3E else '0';
  -- Comparar se Reg 2 de Decode e o mesmo que o reg de escrita em Execution
  Match_12D_Eb <= '1'when RA2D = WA3E else '0';
  -- Formar Match_12D_E
  Match_12D_E <= Match_12D_Ea or Match_12D_Eb;

  -- Quando uma escrita de branch no PC estaria ocorrendo nos estagios Decode ou Execution ou Memory
  PCWrPendingF <= PCSrcD or PCSrcE or PCSrcM;

  Match(4) <= Match_12D_E;
  Match(3) <= Match_2E_W;
  Match(2) <= Match_2E_M;
  Match(1) <= Match_1E_W;
  Match(0) <= Match_1E_M;
end;


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- DMEM
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- IMEM
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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
    VARIABLE i, index, result : integer;
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
          REport "Format error on line " & integer'image(index)
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

-- [VERIFICAR] TALVEZ NAO SEJA NECESSARIO ESTE TB
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- TESTBENCH ARM
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity tb_arm is -- single cycle processor
end;

architecture tb of tb_arm is

	component arm is
	  port (
		 clk, reset : in std_logic;
		 PC : out std_logic_vector(31 downto 0);
		 instr : in std_logic_vector(31 downto 0);

		 MemWrite : out std_logic;
		 ALUResult, WriteData : out std_logic_vector(31 downto 0);
		 ReadData : in std_logic_vector(31 downto 0)
	  );
	 end component;

	 signal clk, reset :  std_logic;
	 signal PC :  std_logic_vector(31 downto 0);
	 signal instr :  std_logic_vector(31 downto 0);

	 signal MemWrite :  std_logic;
	 signal ALUResult, WriteData :  std_logic_vector(31 downto 0);
   signal ReadData :  std_logic_vector(31 downto 0);
   
   signal DataAdr : std_logic_vector(31 downto 0);

  begin

	  dut_arm : arm
		  port map
		  (
			 clk => clk,
			 reset => reset,
			 PC => PC,
			 instr => instr,
			 MemWrite => MemWrite,
			 ALUResult => DataAdr,
			 WriteData => WriteData,
			 ReadData => ReadData
		  );

		  -- Generate clock with 10 ns period
		  PROCESS begin
			 clk <= '1';
			 WAIT FOR 5 ns;
			 clk <= '0';
			 WAIT FOR 5 ns;
		  end PROCESS;

   PROCESS begin
    reset <= '1';
    WAIT FOR 22 ns;
    reset <= '0';
    WAIT;
  end PROCESS;



  PROCESS begin--(clk, reset, PC, instr, MemWrite,  ALUResult, WriteData, ReadData ) begin
    IF (clk'event AND clk = '0' AND MemWrite = '1') THEN

        --REport "NO ERRORS: Simulation succeeded" SEVERITY failure;
    ELSE
    end IF;
  end PROCESS;

  ------------------------
  -- Instrucoes:
  --
  --
  PROCESS --(clk, reset, PC, instr, MemWrite,  ALUResult, WriteData, ReadData )
  begin
		wait until reset = '0';



  end PROCESS;

 end architecture;

-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- ARM
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity arm is -- single cycle processor
  port (
    clk, reset : in std_logic;
    PC : out std_logic_vector(31 downto 0);
    instr : in std_logic_vector(31 downto 0);

    MemWrite : out std_logic;
    ALUResult, WriteData : out std_logic_vector(31 downto 0);
    ReadData : in std_logic_vector(31 downto 0)
  );
end;

architecture struct OF arm is

component controller
    port (
      clk, reset : in std_logic;
      instr : in std_logic_vector(27 downto 12);
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
      FlagWrite : out std_logic_vector(1 downto 0);
      Branch : out std_logic
    );
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
      instrIn : in std_logic_vector(31 downto 0);
      instrOut : out std_logic_vector(27 downto 12);
      ALUResult, WriteData : BUFFER std_logic_vector(31 downto 0);
      ReadData : in std_logic_vector(31 downto 0);
      MemWriteIn : in std_logic;
      MemWriteOut : out std_logic;
      ALUOut: out std_logic_vector(31 downto 0);
      -- [VERIFICAR] TEM QUE ESTAR CONECTADOS QUANDO TIVER O PIPELINE
      FlagWrite : in std_logic_vector(1 downto 0); --[esta certo sim, confia]confirmar se tamanho esta certo e adicionar na top level entity
      Branch : in std_logic --adicionar na top level entity
    );
  end component;

  -- [MUDAR PIPELINE] NEM TODOS OS SINAIS ABAIXO SAO NECESSARIOS AQUI
  -- PODE TIRAR AS REDUNDANCIAS DEPOIS

 -- Fetch
 signal instrF : std_logic_vector(31 downto 0);

 -- Decode
 signal stallD, flushD : std_logic;
 signal instrD : std_logic_vector(31 downto 0);
 signal PCSrcD, RegWriteD : std_logic;
 signal MemtoRegD : std_logic;--, MemWriteD : std_logic;
 signal ALUControlD, FlagWriteD : std_logic_vector(1 downto 0);
 signal BranchD, ALUSrcD : std_logic;
 signal RD1D, RD2D, ExtImmD : std_logic_vector(31 downto 0);
 signal WA3D : std_logic_vector(3 downto 0);
 signal CondD: std_logic_vector(3 downto 0);
 signal Flags : std_logic_vector(3 downto 0);--[nao precisa mais ver tamanho]
 signal FLushE : std_logic;
 signal MemWriteD : std_logic;
 signal ImmSrcD : std_logic_vector(1 downto 0);
 signal RegSrcD :  std_logic_vector(1 downto 0);
 signal ALUFlags : std_logic_vector(3 downto 0);

 -- Memory
 signal MemWriteM : std_logic; -- Sinais combinatorios
 signal ALUResultM, WriteDataM : std_logic_vector(31 downto 0);

 -- Datapath
 signal s_PC : std_logic_vector(31 downto 0);
 signal Branch : std_logic; --adicionar na top level entity


begin

  -- [VERIFICAR] SINAIS QUE SAEM PRA MEMORIA DE DADOS (RAM)

  PC <= s_PC;
  MemWrite <= MemWriteM;
  WriteData <= WriteDataM;
  ALUResult <= ALUResultM;

  instrF <= instr;

  cont : controller port map(
    clk => clk,
    reset => reset,
    instr => instrD(27 downto 12),
    --ALUFlags,
    PCSrc => PCSrcD,--PCSrc
    RegWrite => RegWriteD,--RegWrite,
    MemToReg => MemtoRegD,
    MemWrite => MemWriteD,--MemWrite,
    ALUControl => ALUControlD,
    ALUSrc => ALUSrcD,
    ImmSrc => ImmSrcD,

    RegSrc => RegSrcD,

    FlagWrite => FlagWriteD, -- [MUDAR PIPELINE] ADICIONAR FLAGWRITE D
    Branch => Branch -- [MUDAR PIPELINE] TEM QUE ENTRAR NO DATAPATH PRA DEFINIR SE E BRANCH OU NAO 
    -- [MUDAR PIPELINE] ADICIONAR BRANCH D
  );

  datap : datapath
  port map (
    clk => clk,
    reset => reset,

    PCSrc => PCSrcD,--PCSrc
    RegWrite => RegWriteD,--RegWrite,
    MemToReg => MemtoRegD,
    MemWriteIn => MemWriteD,--MemWrite,
    ALUControl => ALUControlD,
    
    -- [MUDAR PIPELINE] ADICIONAR BRANCH D
    Branch => Branch,
    ALUSrc => ALUSrcD,

    -- [MUDAR PIPELINE] ADICIONAR FLAGWRITE D [MUDADO]
    FlagWrite => FlagWriteD,
    ImmSrc => ImmSrcD,
    RegSrc => RegSrcD,

    ALUFlags => ALUFlags,

    PC => s_PC,
    instrIn => instrF,
    instrOut => instrD(27 downto 12),

    ReadData => ReadData, -- [VERIFICAR] VEM DA RAM, DATA MEMORY [e uma entrada]

    MemWriteOut => MemWriteM,
    WriteData => WriteDataM,
    --ALUResult => ALUResultM [VERIFICAR] ESSE SINAL NAO SERVE PRA NADA
    ALUOut => ALUResultM
  );
end architecture;


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- CONTROLLER
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity controller is -- single cycle control decoder
  port (
    clk, reset : in std_logic;
    instr : in std_logic_vector(27 downto 12);
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
    FlagWrite : out std_logic_vector(1 downto 0);-- [VERIFICAR] E BIT OU E VETOR??
    Branch : out std_logic
  );
end entity;

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
      ALUControl : out std_logic_vector(1 downto 0);

      Branch : out std_logic      
    );
  end component;

  signal FlagW : std_logic_vector(1 downto 0);
  signal PCS, RegW, MemW : std_logic;

begin
  dec : decoder port map(
    Op => instr(27 downto 26),
    Funct => instr(25 downto 20),
    Rd => instr(15 downto 12),

    FlagW => FlagW,
    PCS => PCS,
    RegW => RegW,
    MemW => MemW,
    MemtoReg => MemToReg,
    ALUSrc => ALUSrc,
    ImmSrc => ImmSrc,
    RegSrc => RegSrc,
    ALUControl => ALUControl,

    Branch => Branch
  );

   PCSrc <= PCS;
   RegWrite <= RegW;
   MemWrite <= MemW;
   FlagWrite <= FlagW; -- [VERIFICAR] QUANDO FIZ ISSO, ENTENDI QUE O FLAGW TINHA QUE SER
   
   -- REPASSADO PARA A CONDLOGIC (ASSIM COMO NO MONOCICLO), POR ISSO TEM QUE SE CONECTAR COM A SAIDA
end;


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- DECODER
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&componn&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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
    Branch: out std_logic;
    ALUControl : out std_logic_vector(1 downto 0));
end;

architecture behave OF decoder is
  signal controls : std_logic_vector(9 downto 0);
  signal ALUOp, s_Branch : std_logic;
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
  s_Branch, ALUOp) <= controls;

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

  PCS <= ((AND Rd) AND RegW) OR s_Branch;
  Branch <= s_Branch;
end;


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- COND_UNIT
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity cond_unit is -- Conditional logic
  port (
    clk, reset : in std_logic;

    Cond : in std_logic_vector(3 downto 0);
    ALUFlags : in std_logic_vector(3 downto 0);
    FlagW : in std_logic_vector(1 downto 0);
    PCS, RegW, MemW : in std_logic;
    FlagsE: in std_logic_vector(3 downto 0);
    Branch : in std_logic;

    Flags: out std_logic_vector(3 downto 0);
    PCSrc, RegWrite : out std_logic;
    MemWrite : out std_logic;
    BranchTaken : out std_logic
  );
end;

architecture behave OF cond_unit is
  component condcheck
    port (
      Cond : in std_logic_vector(3 downto 0);
      Flags : in std_logic_vector(3 downto 0);
      CondEx : out std_logic);
  end component;
  component flopenr generic (width : integer);
    port (
      clk, reset, en : in std_logic;
      d : in std_logic_vector(width - 1 downto 0);
      q : out std_logic_vector(width - 1 downto 0));
  end component;

  signal FlagWrite : std_logic_vector(1 downto 0);
  --signal Flags : std_logic_vector(3 downto 0);
  signal CondEx : std_logic;

begin
  flagreg1 : flopenr generic map(2)
  port map(
    clk => clk,
    reset => reset,
    en => FlagWrite(1),
    d => ALUFlags(3 downto 2),
    q => Flags(3 downto 2)
  );

  flagreg0 : flopenr
  generic map(width => 2)
  port map(
    clk => clk,
    reset => reset,
    en => FlagWrite(0),
    d => ALUFlags(1 downto 0),
    q => Flags(1 downto 0)
  );

  cc : condcheck port map(
    Cond => Cond,
    Flags => FlagsE,
    CondEx => CondEx
  );

  PCSrc <= PCS AND CondEx;
  RegWrite <= RegW AND CondEx;
  MemWrite <= MemW AND CondEx;
  FlagWrite <= FlagW AND (CondEx, CondEx);

  BranchTaken <= Branch and CondEx;

end;


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- CONDCHECK
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- DATAPATH
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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
    instrIn : in std_logic_vector(31 downto 0);
    instrOut : out std_logic_vector(27 downto 12);

    ALUResult, WriteData : BUFFER std_logic_vector(31 downto 0);
    ReadData : in std_logic_vector(31 downto 0);

    MemWriteIn : in std_logic;
    MemWriteOut : out std_logic;

    ALUOut: out std_logic_vector(31 downto 0);
    FlagWrite : in std_logic_vector(1 downto 0); --[esta certo sim, confia]confirmar se tamanho esta certo e adicionar na top level entity
    Branch : in std_logic --adicionar na top level entity
  );
end;

architecture struct OF datapath is

  component hazard_logic is
    port (
      -- ENTRADAS
      clock : in std_logic;
      reset : in std_logic;
      RA1D : in std_logic_vector(3 downto 0);
      RA2D : in std_logic_vector(3 downto 0);
      RA1E : in std_logic_vector(3 downto 0);
      RA2E : in std_logic_vector(3 downto 0);
      WA3E : in std_logic_vector(3 downto 0);
      WA3M : in std_logic_vector(3 downto 0);
      WA3W : in std_logic_vector(3 downto 0);
      PCSrcD : in std_logic;
      PCSrcE : in std_logic;
      PCSrcM : in std_logic;
      -- SAIDAS
      -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
      Match : out std_logic_vector(4 downto 0);
      PCWrPendingF: out std_logic
    );
  end component;

  --------------------------------------------------------------------------

  component hazard_unit is
    port (
      -- ENTRADAS
      clock : in std_logic;
      reset : in std_logic;
      -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
      Match : in std_logic_vector(4 downto 0);
      PCWrPendingF: in std_logic;

      RegWriteM : in std_logic;
      RegWriteW : in std_logic;
      MemToRegE : in std_logic;

      PCSrcW : in std_logic;
      BranchTakenE : in std_logic;
      -- SAIDAS
      StallF : out std_logic;
      StallD : out std_logic;
      FlushD : out std_logic;
      FlushE : out std_logic;
      ForwardAE : out std_logic_vector(1 downto 0);
      ForwardBE : out std_logic_vector(1 downto 0)
    );
  end component;

 --------------------------------------------------------------------------

  component partial_IF_ID is
  port (
    clock, reset : in std_logic;
    instrF : in std_logic_vector(31 downto 0);
    stallD, flushD : in std_logic;

    instrD : out std_logic_vector(31 downto 0)
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
    RD1D, RD2D, ExtImmD : in std_logic_vector(31 downto 0);
    WA3D : in std_logic_vector(3 downto 0);
    CondD: in std_logic_vector(3 downto 0);
    FlagsD : in std_logic_vector(3 downto 0);--[nao precisa mais ver tamanho]
    FLushE : in std_logic;
    RA1D, RA2D : in std_logic_vector(3 downto 0);
    
    RA1E, RA2E : out std_logic_vector(3 downto 0);
    PCSrcE, RegWriteE : out std_logic;
    MemtoRegE, MemWriteE : out std_logic;
    ALUControlE, FlagWriteE : out std_logic_vector(1 downto 0);
    BranchE, ALUSrcE : out std_logic;
    RD1E, RD2E, ExtImmE : out std_logic_vector(31 downto 0);
    WA3E : out std_logic_vector(3 downto 0);
    CondE : out std_logic_vector(3 downto 0);
    FlagsE : out std_logic_vector(3 downto 0)--[nao precisa mais ver tamanho]
  );
end component;

------------------------------------------------------------------

component partial_EX_MEM is
  port (
    clock, reset : in std_logic;

    PCSrcE, RegWriteE, MemtoRegE, MemWriteE : in std_logic; -- Sinais combinatorios
    ALUResultE, WriteDataE : in std_logic_vector(31 downto 0);
    WA3E : in std_logic_vector(3 downto 0);

    PCSrcM, RegWriteM, MemtoRegM, MemWriteM : out std_logic; -- Sinais combinatorios
    ALUResultM, WriteDataM : out std_logic_vector(31 downto 0);
    WA3M : out std_logic_vector(3 downto 0)
  );
end component;


--------------------------------------------------------

component partial_MEM_WB is
  port (
    clock : in std_logic;
    reset : in std_logic;

    PCSrcM : in std_logic;
    RegWriteM : in std_logic;
    MemtoRegM : in std_logic;
    ALUOutM   : in std_logic_vector(31 downto 0);
    WA3M      : in std_logic_vector(3 downto 0);
    RD      : in std_logic_vector(31 downto 0);

    PCSrcW : out std_logic;
    RegWriteW : out std_logic;
    MemtoRegW : out std_logic;
    ReadDataW : out std_logic_vector(31 downto 0);
    ALUOutW   : out std_logic_vector(31 downto 0);
    WA3W      : out std_logic_vector(3 downto 0)
  );
end component;

---------------------------------------------------------
  component cond_unit
  port (
      clk, reset : in std_logic;

      Cond : in std_logic_vector(3 downto 0);
      ALUFlags : in std_logic_vector(3 downto 0);
      FlagW : in std_logic_vector(1 downto 0);
      PCS, RegW, MemW : in std_logic;
      FlagsE: in std_logic_vector(3 downto 0);
      Branch : in std_logic;

      Flags: out std_logic_vector(3 downto 0);
      PCSrc, RegWrite : out std_logic;
      MemWrite : out std_logic;
      BranchTaken : out std_logic
  );
  end component;
-----------------------------------------------------------

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
      rd1, rd2 : out std_logic_vector(31 downto 0);
      db_r0, db_r1, db_r2, db_r3, db_r4, db_r5, db_r6, db_r7, db_r8, db_r9, db_r10, db_r11, db_r12, db_r13, db_r14, db_r15 : out std_logic_vector(31 downto 0)
      );
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
  component flopr 
  generic (width : integer);
    port (
      clk, reset : in std_logic;
      d : in std_logic_vector(width - 1 downto 0);
      q : out std_logic_vector(width - 1 downto 0));
  end component;
  component mux2 
  generic (width : integer);
    port (
      d0, d1 : in std_logic_vector(width - 1 downto 0);
      s : in std_logic;
      y : out std_logic_vector(width - 1 downto 0));
  end component;
  component mux4 
  generic (width : integer);
    port (
      d0, d1, d2, d3 : in std_logic_vector(width - 1 downto 0);
      s : in std_logic_vector(1 downto 0);
      y : out std_logic_vector(width - 1 downto 0)
    );
  end component;
  component flopenr -- flip-flop with enable and asynchronous reset
  generic (width : integer);
  port (
    clk, reset, en : in std_logic;
    d : in std_logic_vector(width - 1 downto 0);
    q : out std_logic_vector(width - 1 downto 0));
  end component;

  signal PCNext1, PCNext2 : std_logic_vector(31 downto 0);
  --signal ExtImm : std_logic_vector(31 downto 0);
  signal SrcAE, SrcBE, SrcB : std_logic_vector(31 downto 0);
  signal RA1D, RA2D : std_logic_vector(3 downto 0);
  signal RA1E, RA2E : std_logic_vector(3 downto 0);

  --WriteData : out std_logic_vector(31 downto 0);
 --ReadData : in std_logic_vector(31 downto 0));

 --PC : out std_logic_vector(31 downto 0);
 --instr : in std_logic_vector(31 downto 0);

 --signal RegWrite : std_logic;--, ALUSrc,
 --signal MemtoReg, PCSrc : std_logic;
 --signal RegSrc, ImmSrc, ALUControl : std_logic_vector(1 downto 0);
 --signal ALUFlags : std_logic_vector(3 downto 0); [VERIFICAR] ALUFLAGS TEM QUE SER UMA SAIDA PRA CONDUNIT


 -- CUIDADO COM A LINHA ACIMA, ELA ESTA AZUL CLARO


 --signal FlagWriteE : std_logic;
 --signal PCS, 
 signal RegW, MemW : std_logic; --[VERIFICAR] PODE SER QUE TENHA TIRADO SINAL A MAIS
 --signal condE : std_logic_vector(3 downto 0);

 --signal RegWriteD, MemWriteD, PCSrcD : std_logic;
 --signal RegWriteE, MemWriteE, PCSrcE : std_logic;

 -- Fetch
 signal instrF : std_logic_vector(31 downto 0);
 signal PCPlus4F : std_logic_vector(31 downto 0);

 -- Decode
 --signal stallD, flushD : std_logic;
 signal instrD : std_logic_vector(31 downto 0);
 signal PCSrcD, RegWriteD : std_logic;
 signal MemtoRegD : std_logic;--, MemWriteD : std_logic;
 signal ALUControlD, FlagWriteD : std_logic_vector(1 downto 0);
 signal BranchD, ALUSrcD : std_logic;
 signal RD1D, RD2D, ExtImmD : std_logic_vector(31 downto 0);
 signal WA3D : std_logic_vector(3 downto 0);
 signal CondD: std_logic_vector(3 downto 0);
 signal Flags : std_logic_vector(3 downto 0);--[nao precisa mais ver tamanho]
 --signal FLushE : std_logic;
 signal PCPlus8D : std_logic_vector(31 downto 0);

 -- Execute
 signal PCSrcE1, PCSrcE2, RegWriteE1, RegWriteE2 : std_logic;
 signal MemtoRegE, MemWriteE1, MemWriteE2 : std_logic;
 signal ALUControlE, FlagWriteE : std_logic_vector(1 downto 0);
 
 -- [MUDAR PIPELINE] PRECISA LIGAR O ALURESULT NO SEGUNDO MUX DO PC
 signal ALUResultE : std_logic_vector(31 downto 0);
 signal WriteDataE : std_logic_vector(31 downto 0);
 signal BranchE, ALUSrcE : std_logic;
 signal RD1E, RD2E, ExtImmE : std_logic_vector(31 downto 0);
 signal WA3E : std_logic_vector(3 downto 0);
 signal CondE : std_logic_vector(3 downto 0);
 signal FlagsE : std_logic_vector(3 downto 0);--[nao precisa mais ver tamanho]

 -- Memory
 signal PCSrcM, RegWriteM, MemtoRegM, MemWriteM : std_logic; -- Sinais combinatorios
 signal ALUResultM, WriteDataM : std_logic_vector(31 downto 0);
 signal WA3M : std_logic_vector(3 downto 0);

 signal ALUOutM : std_logic_vector(31 downto 0); -- [MUDAR PIPELINE] BIFURCAR PARA O MUX DO SrcAE
 signal ReadDataM : std_logic_vector(31 downto 0);

 -- Write back
 signal PCSrcW : std_logic;
 signal RegWriteW : std_logic;
 signal MemtoRegW : std_logic;
 signal ReadDataW : std_logic_vector(31 downto 0);
 signal ALUOutW : std_logic_vector(31 downto 0);
 signal WA3W : std_logic_vector(3 downto 0);
 signal ResultW : std_logic_vector(31 downto 0);

 -- Datapath
 signal s_PC : std_logic_vector(31 downto 0);

 -- Hazards
 signal Match : std_logic_vector(4 downto 0);
 signal PCWrPendingF : std_logic;

 signal StallF, not_StallF : std_logic;
 signal StallD : std_logic;
 signal FlushD : std_logic;
 signal FlushE : std_logic;

 signal ForwardAE : std_logic_vector(1 downto 0);
 signal ForwardBE : std_logic_vector(1 downto 0);

 signal BranchTakenE : std_logic; -- [VERIFICAR] Ligar a hazard unit e a mux do PC

 --Registradores de pipeline
 -------ID-EX
 -- [VERIFICAR] PODE SER QUE DE CACA TIRAR ESSE s_WA3D
 --signal s_RD1D, s_RD2D, s_extendD : std_logic_vector(31 downto 0);
 --signal s_WA3D : in std_logic_vector(3 downto 0);

begin

-- Entradas e saidas desta entidade (estao abaixo)
    --PCSrc <= PCSrcW;
    --RegWrite <= RegWriteW; [VERIFICAR] REGWRITEW TEM QUE IR PRO REGFILE
    --MemWrite <= MemWriteM;
    instrF <= instrIn;

    --[VERIFICAR] LA EM CIMA DIZ QUE ESTE SINAL N SERVE PRA NADA --ALUResult <= ALUResultM; -- [VERIFICAR]?? SSE SINAL E NECESSARIO MESMO    
    ALUOut <= ALUOutM;
    ALUResultM <= ALUOutM; -- [VERIFICAR] E ISSO?

    WriteData <= WriteDataM;
    ReadDataM <= ReadData;
    MemWriteOut <= MemWriteM; --saida do datapath
    PCSrcD <= PCSrc;

    PC <= s_PC;

 --------------------------------------------------
  CondD <= instrD(31 downto 28);
  instrOut <= instrD(27 downto 12);

  WriteDataE <= RD2E; -- [MUDAR PIPELINE] RECEBE O QUE SAI DO MUX ANTES DO SrcBE
  WA3D <= instrD(15 downto 12);

  PCPlus8D <= PCPlus4F;

  -- next PC logic
  pcmux1 : mux2 -- Mais a esquerda na imagem
  generic map(width => 32)
  port map(
    d0 => PCPlus4F, --[MUDAR PRO PIPELINE] TEM QUE ADICIONAR UM OUTRO MUX2 ANTES DO PC+4
    d1 => ResultW,
    s => PCSrcW,
    y => PCNext1
  );

  pcmux2 : mux2 -- mais a direita na imagem
  generic map(width => 32)
  port map(
    d0 => PCNext1, --[MUDAR PRO PIPELINE] TEM QUE ADICIONAR UM OUTRO MUX2 ANTES DO PC+4
    d1 => ALUResultE,--ResultW, -- [VERIFICAR] VEM DA ALU MESMO?
    s => BranchTakenE, -- [VERIFICAR] SERA QUE ESSE SINAL TA VINDO DA CONTROL UNIT MESMO?
    y => PCNext2
  );

  pcreg : flopENr --[MUDAR QUANDO FOR PIPELINE] torna-lo um registrador para por enanble
  generic map(width => 32)
  port map(
    clk => clk,
    reset => reset,
    en => not_StallF,
    d => PCNext2,
    q => s_PC
  );
  not_StallF <= not StallF; 

  pcadd : adder
  port map(
    a => s_PC,
    b => X"00000004",
    y => PCPlus4F
  );

  ra1mux : mux2
  generic map(width => 4)
  port map
  (
    d0 => instrD(19 downto 16),
    d1 => x"F",
    s => RegSrc(0), -- entrada desta entidade (vem da control unit)
    y => RA1D
  );

  ra2mux : mux2
  generic map(width => 4)
  port map(
    d0 => instrD(3 downto 0),
    d1 => instrD(15 downto 12),
    s => RegSrc(1), -- entrada desta entidade
    y => RA2D
  );

  rf : regfile port map
  (
    clk => clk,
    we3 => RegWriteW, -- [VERIFICAR] JA ESTA NO DATAPATH O SINAL
    ra1 => RA1D,
    ra2 => RA2D,

    wa3 => WA3W,--instrD(15 downto 12), -- [MUDAR PIPELINE] VEM DO WA3W [MUDADO]
    wd3 => ResultW,
    r15 => PCPlus8D, -- [MUDAR PIPELINE] VEM DO PCPlus4F ou PCPlus8D [MUDADO] [VERIFICAR] SE VEM DA ALU

    rd1 => RD1D,--SrcAE, -- [VERIFICAR] TEM UM MUX NO MEIO QUE GERA SrcAE [VERIFICAR] Deve entrar em partial_ID_EX
    rd2 => RD2D,--WriteData [VERIFICAR] tem que entrar em um mux tbm -- [VERIFICAR] Deve entrar em partial_ID_EX
    db_r0 => open, 
    db_r1 => open, 
    db_r2 => open, 
    db_r3 => open, 
    db_r4 => open, 
    db_r5 => open, 
    db_r6 => open, 
    db_r7 => open, 
    db_r8 => open, 
    db_r9 => open, 
    db_r10 => open, 
    db_r11 => open, 
    db_r12 => open, 
    db_r13 => open, 
    db_r14 => open, 
    db_r15 => open

  );

  res_mux : mux2
  generic map(width => 32)
  port map
  (
    d0 => ALUOutW,--ALUResult,
    d1 => ReadDataW,--ReadData,
    s => MemToRegW, -- [VERIFICAR] Sinais devem estar vindo do partial_MEM_WB
    y => ResultW
  );

  ext : extend
  port map
  (
    instr => instrD(23 downto 0),
    ImmSrc => ImmSrc, -- [VERIFICAR] sinal ImmSrc vem direto da Control Unit
    ExtImm => ExtImmD -- [VERIFICAR] Passar a saida para partial_ID_EX
  );

  -- ALU logic
  srcBmux2 : mux2
  generic map(width => 32) --[MUDAR PRO PIPELINE] d0(outro mux intermediario) d1(partial_ID_EX ExtImmE)
  port map
  (
    d0 => WriteDataE,--SrcB,--WriteData, [VERIFICAR] talvez seja o writedata mesmo
    d1 => ExtImmE, -- [VERIFICAR]
    s => ALUSrcE,
    y => SrcBE
  );

  srcBmux4 : mux4 --[MUDAR PRO PIPELINE] d0(partial_ID_EX RD2E),d1(ResultW),d2(partial_EX_MEM AluResultM)
  generic map (width => 32)
  port map (
    d0 => RD2E,
    d1 => ResultW,
    d2 => ALUOutM,--ALUResultM, [VERIFICAR] ACHO QUE O ALURESULTM NEM EXISTE MAIS
    d3 => (others => '0'),
    s => ForwardBE, -- [MUDAR PRO PIPELINE] passara a vir da hazard unit, com o nomr ForwardBE
    y => WriteDataE--SrcB -- [MUDAR PRO PIPELINE] saida ira para o mux que ja existia na versao monociclo
  );

  srcAmux4 : mux4 -- [OBS] SO TEM UM MUX PRO A MESMO
  generic map (width => 32)
  port map
  (
    d0 => RD1E, -- [MUDAR PRO PIPELINE] d0(partial_ID_EX RD1E),d1(ResultW),d2(partial_EX_MEM AluResultM)
    d1 => ResultW,
    d2 => ALUOutM,--ALUResultM, [VERIFICAR] ACHO QUE O ALURESULTM NEM EXISTE MAIS
    d3 => (others => '0'),
    s => ForwardAE, -- [MUDAR PRO PIPELINE] utilizar ForwardAE
    y => SrcAE -- [MUDAR PRO PIPELINE] saida ira para SrcAE [MUDADO] [VERIFICAR] DELCARACAO DESSE SINAL
  );


  aluinst : alu
  port map
  (
    a => SrcAE, -- [MUDAR PRO PIPELINE] Deve vir de SrcAE
    b => SrcBE, -- [MUDAR PRO PIPELINE] Deve vir de SrcBE
    ALUControl => ALUControlE, -- [VERIFICAR] sinal deve vir de partial_ID_EX AluControlE
    Result => ALUResultE,--ALUResult,
    ALUFlags => ALUFlags --[VERIFICAR] ir para cond unit
  );

 ---------------------------------------------------------
 -- Logica para lidar com hazards

 hl : hazard_logic  -- [VERIFICAR] SABE LA O QUE TA CONECTADO AQUI
 port map
 (
     -- ENTRADAS
     clock => clk,
     reset => reset,

     RA1D => RA1D,
     RA2D => RA2D,
     RA1E => RA1E,
     RA2E => RA2E,
     WA3E => WA3E,
     WA3M => WA3M,
     WA3W => WA3W,
     PCSrcD => PCSrcD,
     PCSrcE => PCSrcE1,
     PCSrcM => PCSrcM,
     
     -- SAIDAS
     -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
     Match => Match,
     PCWrPendingF => PCWrPendingF
   );

 --------------------------------------------------------------------------

 hu : hazard_unit -- [VERIFICAR] IDEM
 port map
 (
     -- ENTRADAS
     clock => clk,
     reset => reset,
     -- (Match_12D_E, Match_2E_W, Match_2E_M, Match_1E_W, Match_1E_M)
     Match => Match,
     PCWrPendingF => PCWrPendingF,

     RegWriteM => RegWriteM,
     RegWriteW => RegWriteW,
     MemToRegE => MemToRegE,

     PCSrcW => PCSrcW,
     BranchTakenE => BranchTakenE,

     -- SAIDAS
     StallF => StallF, -- [VERIFICAR] Ligar em enable PC
     StallD => StallD, -- [VERIFICAR] Ligar em enable IF/ID
     FlushD => FlushD, -- [VERIFICAR] Ligar em clear IF/ID
     FlushE => FlushE, -- [VERIFICAR] Ligar em clear ID/EX
     ForwardAE => ForwardAE, -- [VERIFICAR] Ligar em mux para SrcAE
     ForwardBE => ForwardBE -- [VERIFICAR] Ligar em mux para SrcBE
   );

 ---------------------------------------------------------
 --Registradores de Pipeline



cl : cond_unit
port map
(
  clk => clk,
  reset => reset,

  PCS => PCSrcE1, -- entradas transplantadas
  RegW => RegWriteE1, -- entradas transplantadas
  MemW => MemWriteE1,-- entradas transplantadas

  PCSrc => PCSrcE2,
  RegWrite => RegWriteE2,
  MemWrite => MemWriteE2,--MemWrite

  Branch => BranchE, -- [VERIFICAR] VEM DOS REGS DE PIPELINE PRA ENTRAR AQUI
  BranchTaken => BranchTakenE, -- [VERIFICAR] TEM QUE TER UMA SAIDA BRANCH DA CONDUNIT PRA MANDAR LA PRO MUX DO PC

  Flags => Flags, --adicionar sinal e adicionar Flags E na entidade cond_unit
  FlagW => FlagWriteE,

  Cond => condE,--instr(31 downto 28),
  FlagsE => FlagsE, --adicionar sinal e adicionar Flags E na entidade cond_unit
  ALUFlags => ALUFlags
);


-- [VERIFICAR] ENTRADAS E SAIDAS DOS REGS DE PIPELINE
 inst_partial_IF_ID : partial_IF_ID
 port map
 (
  clock => clk,
  reset => reset,
  instrF => instrF,
  stallD => stallD,
  flushD => flushD,

  instrD => instrD
);

--------------------------------------------------------------------------

inst_partial_ID_EX : partial_ID_EX
port map
(
  clock => clk,
  reset => reset,
  PCSrcD => PCSrc,
  RegWriteD => RegWrite,
  MemtoRegD => MemtoReg,
  MemWriteD => MemWriteIn,
  ALUControlD => ALUControl,
  FlagWriteD => FlagWrite, -- vem do controller (UC)
  BranchD => Branch,
  ALUSrcD => ALUSrc,
  RD1D => RD1D,
  RD2D => RD2D,
  ExtImmD => ExtImmD, --[VERIFICAR] vir de Extend
  WA3D => WA3D,
  CondD => CondD,
  FlagsD => Flags,
  FLushE => reset, --[MUDAR PRO PIPELINE]trocar pelo input FlushE do hazard unit
  RA1D => RA1D,
  RA2D => RA2D,
    
  RA1E => RA1E,
  RA2E => RA2E,
  PCSrcE => PCSrcE1,--[VERIFICAR] Colocar sinais com E1 em algum AND
  RegWriteE => RegWriteE1,
  MemtoRegE => MemtoRegE,
  MemWriteE => MemWriteE1,
  ALUControlE => ALUControlE,
  FlagWriteE => FlagWriteE,
  BranchE => BranchE,
  ALUSrcE => ALUSrcE,
  RD1E => RD1E,
  RD2E => RD2E,
  ExtImmE => ExtImmE, --[VERIFICAR] ir para onde esta ExtImmE
  WA3E => WA3E,
  CondE => CondE,

  FlagsE => FlagsE
);

------------------------------------------------------------------

inst_partial_EX_MEM : partial_EX_MEM
port map
(
  clock => clk,
  reset => reset,

  PCSrcE => PCSrcE2, -- [VERIFICAR] E2 vem de uma porta AND
  RegWriteE => RegWriteE2,
  MemtoRegE => MemtoRegE,
  MemWriteE => MemWriteE2,
  -- Sinais combinatorios
  ALUResultE => ALUResultE,
  WriteDataE => WriteDataE,
  WA3E => WA3E,

  PCSrcM => PCSrcM,
  RegWriteM => RegWriteM,
  MemtoRegM => MemtoRegM,
  MemWriteM => MemWriteM,

  -- Sinais combinatorios
  ALUResultM => ALUOutM,
  WriteDataM => WriteDataM,
  WA3M => WA3M
);

--------------------------------------------------------

inst_partial_MEM_WB : partial_MEM_WB
port map (
  clock => clk,
  reset => reset,

  PCSrcM => PCSrcM,
  RegWriteM => RegWriteM,
  MemtoRegM => MemtoRegM,

  ALUOutM => ALUOutM,
  WA3M => WA3M,
  RD => ReadDataM,

  PCSrcW => PCSrcW,
  RegWriteW => RegWriteW,
  MemtoRegW => MemtoRegW,
  ReadDataW => ReadDataW,
  ALUOutW => ALUOutW,
  WA3W => WA3W
);

end;


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- REGFILE
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity regfile is -- three-port register file
  port (
    clk : in std_logic;
    we3 : in std_logic;
    ra1, ra2, wa3 : in std_logic_vector(3 downto 0);
    wd3, r15 : in std_logic_vector(31 downto 0);
    rd1, rd2 : out std_logic_vector(31 downto 0);
    db_r0, db_r1, db_r2, db_r3, db_r4, db_r5, db_r6, db_r7, db_r8, db_r9, db_r10, db_r11, db_r12, db_r13, db_r14, db_r15 : out std_logic_vector(31 downto 0)
  );
end;

architecture behave OF regfile is
  TYPE ramtype is ARRAY (31 downto 0) OF
  std_logic_vector(31 downto 0);
  signal mem : ramtype;
begin
  PROCESS (clk) begin
    IF falling_edge(clk) THEN
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- ADDER
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- EXTEND
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- FLOPNR
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity flopenr is -- flip-flop with enable and asynchronous reset
  generic (width : integer);
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- FLOPR
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity flopr is -- flip-flop with asynchronous reset
  generic (width : integer);
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- MUX2
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity mux2 is -- two-input multiplexer
  generic (width : integer);
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


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- MUX4
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE;
use IEEE.std_logic_1164.all;
entity mux4 is -- two-input multiplexer
  generic (width : integer);
  port (
    d0, d1, d2, d3 : in std_logic_vector(width - 1 downto 0);
    s : in std_logic_vector(1 downto 0);
    y : out std_logic_vector(width - 1 downto 0)
  );
end;

architecture behave OF mux4 is
begin
  with s select
    y <=
      d0 when "00",
      d1 when "01",
      d2 when "10",
      d3 when "11",
      d0 when others;
end;


-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
-- ALU
-- &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library IEEE; 
use IEEE.STD_LOGIC_1164.all; 
use IEEE.NUMERIC_STD_UNSIGNED.all;

entity alu is 
  port(
      a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
       ALUControl: in  STD_LOGIC_VECTOR(1 downto 0);
       Result:     buffer STD_LOGIC_VECTOR(31 downto 0);
       ALUFlags:      out STD_LOGIC_VECTOR(3 downto 0)
  );
end entity;

architecture behave of alu is
  signal condinvb: STD_LOGIC_VECTOR(31 downto 0);
  signal sum:      STD_LOGIC_VECTOR(32 downto 0);
  signal neg, zero, carry, overflow: STD_LOGIC;
begin
  condinvb <= not b when ALUControl(0) else b;
  sum <= ('0', a) + ('0', condinvb) + ALUControl(0);

  process(all) begin
    case? ALUControl(1 downto 0) is
      when "0-"   => result <= sum(31 downto 0); 
      when "10"   => result <= a and b; 
      when "11"   => result <= a or b; 
      when others => result <= (others => '-');
    end case?;
  end process;

  neg      <= Result(31);
  zero     <= '1' when (Result = 0) else '0';
  carry    <= (not ALUControl(1)) and sum(32);
  overflow <= (not ALUControl(1)) and
             (not (a(31) xor b(31) xor ALUControl(0))) and
             (a(31) xor sum(31));
  ALUFlags    <= (neg, zero, carry, overflow);
end architecture;