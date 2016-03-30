LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      -- D7S
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    -- (0)
                                    "1111001" WHEN "00001",    -- (1)
                                    "0100100" WHEN "00010",    -- (2)      +---- a ----+
                                    "0110000" WHEN "00011",    -- (3)      |           |
                                    "0011001" WHEN "00100",    -- (4)      |           |
                                    "0010010" WHEN "00101",    -- (5)      f           b
                                    "0000010" WHEN "00110",    -- (6)      |           |
                                    "1111000" WHEN "00111",    -- (7)      |           |
                                    "0000000" WHEN "01000",    -- (8)      +---- g ----+
                                    "0010000" WHEN "01001",    -- (9)      |           |
                                    "0001000" WHEN "01010",    -- (A)      |           |
                                    "0000011" WHEN "01011",    -- (b)      e           c
                                    "0100111" WHEN "01100",    -- (c)      |           |
                                    "0100001" WHEN "01101",    -- (d)      |           |
                                    "0000110" WHEN "01110",    -- (E)      +---- d ----+
                                    "0001110" WHEN "01111",    -- (F)
                                    "1111111" WHEN OTHERS;     -- ( )

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Lab4 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0); -- 18 dip switches on the board

      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2,hex4, hex6 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab4;

ARCHITECTURE SimpleCircuit OF Lab4 IS

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   COMPONENT SevenSegment PORT(        -- Declare the 7 segment component to be used
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
   CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL Main1HzCLK:   STD_LOGIC; -- main 1Hz clock to drive FSM
   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock
   SIGNAL TenHzModCLK:  STD_LOGIC;

   SIGNAL bin_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset binary counter to zero
   SIGNAL mod_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   SIGNAL mod_TerminalTenHz: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE);
   SIGNAL mod_TerminalOneHz: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE);
   SIGNAL mod_counter10:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE);
   SIGNAL mod_counterfinal:  UNSIGNED(3 DOWNTO 0) := to_unsigned(0,4); -- reset modulus counter to zero
     
   TYPE STATES IS (STATE0, STATE1, STATE2, STATE3, STATE4, STATE5,STATE6,STATE7);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals od type STATES
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment
   
   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
  
   SIGNAL NightMode: STD_LOGIC;
   SIGNAL EWSide:   STD_LOGIC;
   SIGNAL carNS:  STD_LOGIC;
   SIGNAL carEW:  STD_LOGIC;
   
   SIGNAL waitCounterNS :UNSIGNED(3 DOWNTO 0) := to_unsigned(0,4);	-- Lab5 counters
   SIGNAL waitCounterEW :UNSIGNED(3 DOWNTO 0) := to_unsigned(0,4);
   SIGNAL carArrive:   STD_LOGIC;

--BEGIN

   --BinCLK: PROCESS(clock_50)
   --BEGIN
      --IF (rising_edge(clock_50)) THEN -- binary counter increments on rising clock edge
         --bin_counter <= bin_counter + 1;
      --END IF;
   --END PROCESS;
   --OneHzBinCLK <= std_logic(bin_counter(CLK_DIV_SIZE-1)); -- binary counter MSB
   --LEDG(2) <= OneHzBinCLK;
----------------------------------------------------------------------------------------------------
   --WITH sw(2 DOWNTO 0) SELECT -- terminal count for modulus counter for F0= 50 MHz clock input (T0 = 20 ns) 
      --mod_terminal <= --"" WHEN "000",  -- F =   1 Hz, T/2 = 25000000 * T0
                      --"0010011000100101100111111" WHEN "001",  -- F =   5 Hz, T/2 =  5000000 * T0
                     
                      --"0000000111101000010001111" WHEN "011",  -- F = 100 Hz, T/2 =   250000 * T0
                      --"1011111010111100000111111" WHEN OTHERS; -- *** default ***
 BEGIN
    mod_TerminalTenHz <=   "0001001100010010110011111"; -- F =  10 Hz, T/2 =  2500000 * T0   
    mod_TerminalOneHz <=   "0000000000000000000000100";           


	
   NightMode <= sw(17);
   EWSide   <= sw(16);
   carNS  <= sw(15);
   carEW  <= sw(14);
                   
   ledr(17) <= NightMode;	-- Mode indicators
   ledr(16) <= EWSide;
   ledr(15) <= carNS;
   ledr(14) <= carEW;
 
--   ModCLK1: PROCESS(clock_50) 
--   BEGIN
--	IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
--         IF ( mod_counter10= mod_TerminalTenHz) THEN       -- half period
--             TenHzModCLK<= NOT TenHzModCLK;                 -- toggle
--            mod_counter10 <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
--         ELSE
--            mod_counter10 <= mod_counter10 + 1;
--         END IF;
--      END IF;
--   END PROCESS;
	TenHzModCLK <= clock_50;
   LEDG(3) <= TenHzModCLK;
   
   
   ModCLK2: PROCESS(TenHzModCLK) 
    begin
      IF (rising_edge(TenHzModCLK)) THEN -- modulus counter increments on rising clock edge
         IF (mod_counter = mod_TerminalOneHz) THEN       -- half period
            OneHzModCLK <= NOT OneHzModCLK;                 -- toggle
            mod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod_counter <= mod_counter + 1;
           
         END IF;
      END IF;
    END PROCESS;
  LEDG(1) <= OneHzModCLK; 
  ledg(0) <= TenHzModCLK; 
  
----------------------------------------------------------------------------------------------------
   FSM: PROCESS(state, sw) -- main FSM
   BEGIN
--      next_state <= state; 	-- The only purpose of this line is to give initial value to the signal 'next_state' in order to avoid latch creation. 
      --ledr(15 DOWNTO 0) <= "0000000000000000";
      LEDG(7) <='0';
      LEDG(8) <='0';
      CASE state IS
         WHEN STATE0 =>--0
            state_number <= "0000";
            ledg(8) <= TenHzModCLK;
            LEDR(0) <='1';
            LEDR(11)<='0';
            LEDG(7) <='0';
	IF (state_counter  = 1) then
      next_state<=STATE1;
      else
       next_state<=STATE0; 
   end if;
    
         WHEN STATE1 =>--1
            state_number <= "0001";
            LEDG(8)<='1';
            LEDR(0) <='1';
            LEDR(11)<='0';
            LEDG(7) <='0';
         IF (state_counter = 3) then
      next_state<=STATE2;
      else
       next_state<=STATE1;
	 end if;
      
         WHEN STATE2 =>--2
            state_number <= "0010";
            LEDR(11) <= TenHzModCLK;
            LEDR(0) <='1';
            LEDG(7) <='0';
            LEDG(8)<='0';
      IF (state_counter = 1) THEN	   
			   IF (EWSide = '0' AND NightMode = '1' AND CarEW = '0') THEN
				  next_state <= STATE6;
			   ELSE
				  next_state <= STATE3;
			   END IF;
            ELSE
               next_state <= STATE2;
            END IF;
         WHEN STATE3 => --3
            state_number <= "0011";
            LEDR(11) <='1';
            LEDG(8)<='0';
            LEDG(7)<=TenHzModCLK;
            LEDR(0) <='0';
	IF (state_counter = 1) then
      next_state<=STATE4;
      else
       next_state<=STATE3;
      end if ;
            
         WHEN STATE4 =>--4
            state_number <= "0100";
            LEDR(11) <= '1';
            LEDR(0) <='0';
            LEDG(7) <='1';
            LEDG(8)<='0';
	IF (state_counter = 3) then
      next_state<=STATE5;
      else
       next_state<=STATE4;
      end if ;
           
         WHEN STATE5 =>--5
            state_number <= "0101";
            LEDR(11) <= '1';
            LEDR(0) <=TenHzModCLK;
            LEDG(7) <='0';
            LEDG(8)<='0';
 
      IF (state_counter = 1) THEN
               IF (EWSide = '1' AND NightMode = '1' AND CarNS = '0') THEN
				  next_state <= STATE7;
			   ELSE
                  next_state <= STATE0;
               END IF;
            ELSE
               next_state <= STATE5;
            END IF;

         WHEN STATE6 =>--NS default6
            state_number <= "0110";
            LEDR(11) <= '0';
            LEDR(0) <='1';
            LEDG(7) <='0';
            LEDG(8)<='1';
           IF (state_counter = 5) THEN
				next_state <= STATE2;
				
			ELSE
				next_state <= STATE6;
			END IF;
			
            
        WHEN STATE7 =>--EW1 default7
            state_number <= "0111";
            LEDR(11) <= '1';
            LEDR(0) <='0';
            LEDG(7) <='1';
            LEDG(8)<= '0';
            IF (state_counter = 5) THEN
				next_state <= STATE5;
			ELSE
				next_state <= STATE7;
			END IF;
       
       END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(Main1HzCLK, state) -- creats sequential logic to latch the state
 
BEGIN   
      IF (rising_edge(OneHzModCLK)) THEN
         
         state <= next_state;                      -- on the rising edge of clock the current state is updated with next state
         state_counter <= state_counter + 1;    -- on the rising edge of clock the current counter is incremented
         
         IF (state /= next_state) THEN	-- Reset State counter after a switch
			state_counter <= "0000";
		 END IF;
         
      END IF;
   END PROCESS;
 RedTimeRecoder: PROCESS(OneHzModCLK, state) -- creats sequential logic to record red time
   BEGIN  
 IF (rising_edge(OneHzModCLK)) THEN -- modulus counter increments on rising clock edge
            IF (rising_edge(OneHzModCLK)) THEN
         
         IF (CarNS = '1' AND (state = STATE3 OR state = STATE4 OR state = STATE5)) THEN
			waitCounterNS <= waitCounterNS + 1;
		 ELSE
			waitCounterNS <= "0000";
		 END IF;
		 
		 IF (CarEW = '1' AND (state = STATE0 OR state = STATE1 OR state = STATE2 OR state = STATE6)) THEN
			waitCounterEW <= waitCounterEW + 1;
		 ELSE
		 			waitCounterEW <= "0000";
		 END IF;
		 
        -- state <= next_state ;
         --   mod_counterfinal <=mod_counterfinal+1 ; -- reset counter
		--IF (mod_counterfinal = "1111") THEN       -- half period
       --     mod_counterfinal <= to_unsigned(0,4); -- reset counter
		
      --   END IF;
 
       
     END IF; 
     END IF;
  END PROCESS;
----------------------------------------------------------------------------------------------------
   D7S0: SevenSegment PORT MAP( state_number, '0', hex0 );
   D7S4: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );
   D7S2: SevenSegment PORT MAP( std_logic_vector(waitCounterNS), '0', hex4 );
   D7S3: SevenSegment PORT MAP( std_logic_vector(waitCounterEW), '0', hex6 );


END SimpleCircuit;
