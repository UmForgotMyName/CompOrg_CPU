library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.ALL;


entity PC_REG is
	port(
		clk : in std_logic;
		reset : in std_logic;
		Input : in std_logic_vector(15 downto 0);
		Output : out std_logic_vector(15 downto 0)
	);
end PC_REG;

architecture Behavioral of PC_REG is  
begin
	process(reset, clk)
	begin
           if reset = '0' then
            Output <= (others => '0');
           elsif rising_edge(clk) then
            Output <= Input;
           end if;
    end process;
end Behavioral;
