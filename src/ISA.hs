module ISA where
{-
Emulation CPU Memory
Registers:
	x0 - wired zero
	x1 - ra return address
	x2 - sp stack pointer
	x3 - gp global pointer (not used)
	x4 - tp thread pointer (not used)
	x5-x7 t0-t2 temporary registers
	x8-x9 s0-s1 saved registeres
	x10-x17 a0-a7 argument registers with (a0, a1) beeing also return value registers
	x18-x27 s2-s11 saved registers
	x28-x31 t3-t6 temporary registers
	pc program counter

Instruction memory:
	32-bit address space
Data memory:
	32 bit address space with stack and heap
Stack:
	starts at the end of address-space and grows down
Heap:
	starts at the beginning of the address-space and grows up
-}

type Register = String
zero = "x0"

type DataMemory = Int
type InstructionMemory = Int


type Rd = Register
type Rs1 = Register
type Rs2 = Register
type Label = String
type DataOffset = DataMemory
type InstructionOffset = InstructionMemory

data Instruction = RR RegisterRegister | S Store | B Branch | J Jump | I IO | P Pseudo deriving (Show) 

{-
Slang assembly supports the following types of instructions:
	1. Register-Register with rd, rs1, rs2 as desitantion register, first operand, second operand
	2. Register-Memory with rs2 as destiantion register and rs1 as memory start address rs2 <- [rs1 + offset]
	3. Branch, if condition on rs1 and rs2 is met, jump to PC <- PC + offset
	4. Jump, save the return address to rd, and jump to PC <- PC + offset. Call is a jump that returns to PC <- PC + 4
	5. IO operations
		Read from input-device to [rd],
		Write from [rs] to output-device
-}

type Num = Int

data RegisterRegister = 
	Add Rd Rs1 Rs2 |   
	Sub Rd Rs1 Rs2 |
	Mul Rd Rs1 Rs2 |
	Div Rd Rs1 Rs2 |
	Mod Rd Rs1 Rs2
	deriving(Show)  
data Store = Ld Rs1 Rs2 DataOffset | St Rs1 Rs2 DataOffset deriving(Show)
data Branch = JE Rs1 Rs2 InstructionOffset | JG Rs1 Rs2 InstructionOffset | JL Rs1 Rs2 InstructionOffset deriving(Show)
data Jump = Jump Rd InstructionOffset deriving(Show)
data IO = In Rd InDevice | Out Rs OutDevice deriving(Show)
data Pseudo = 
	Mov Rs1 Rs2 |
	Call InstructionOffset |	Ret |	LabelCall Label |
	AddI Rd Rs1 Num | MulI Rd Rs1 Num | DivI Rd Rs1 Num | ModI Rd Rs1 Num deriving(Show)
