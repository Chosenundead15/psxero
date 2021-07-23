use crate::memory::Interconnect;

#[derive(Copy, Clone)]
struct RegisterIndex(u32);
pub struct Cpu {
    pc: u32, //program counter
    regs: [u32; 32], //general purpose register, first one must always contains zero
    inter: Interconnect, //memory interface
    next_pc: u32,
    sr: u32, //status register from coprocesor 0
    out_regs: [u32; 32],
    load: (RegisterIndex, u32),
    hi: u32, //HI register to store division remainder and multiplication high result
    lo: u32, //LO register to store division quotient and mulstiplication low result
    current_pc: u32, //currently instruction on execution used to set EPC exceptions
    cause: u32, //Cop0 register 13: Cause Register
    epc: u32, //Cop0 register 14: EPC
    branch: bool, //set if there is branching and the next instruction is delay slot
    delay_slot: bool, //set if current instruction is delay slot
}

impl Cpu {
    pub fn new(inter: Interconnect) -> Self {
        //don't know what the reset values for register are so I will use simias guide values for reference and debugging
        let mut regs = [0xdeadbeef; 32];

        let pc: u32 = 0xbfc00000;

        regs[0] = 0; //R0 remains zero as it should
        Cpu {
            pc: pc, //beginning of the BIOS
            regs: regs,
            inter: inter,
            next_pc: pc.wrapping_add(4),
            sr: 0,
            out_regs: regs,
            load: (RegisterIndex(0), 0),
            hi: 0xdeadbeef,
            lo: 0xdeadbeef,
            current_pc: pc,
            cause: 0,
            epc: 0,
            branch: false,
            delay_slot: false
        }
    }

    fn reg(&self, index: RegisterIndex) -> u32 {
        self.regs[index.0 as usize]
    }

    fn set_reg(&mut self, index: RegisterIndex, val: u32) {
        self.out_regs[index.0 as usize] = val;

        self.out_regs[0] = 0; //prevention just in case to keep it zero
    }

    fn store32(&mut self, addr: u32, val: u32) {
        self.inter.store32(addr, val);
    }

    fn store16(&mut self, addr: u32, val: u16) {
        self.inter.store16(addr, val);
    }

    fn store8(&mut self, addr: u32, val: u8) {
        self.inter.store8(addr, val);   
    }

    pub fn run_next_instruction(&mut self) {
        let pc = self.pc;

        let instruction = Instruction(self.load32(pc));

        self.current_pc = self.pc;

        self.pc = self.next_pc;
        self.next_pc = self.next_pc.wrapping_add(4);

        self.delay_slot = self.branch;
        self.delay_slot = false;

        let (reg, val) = self.load;
        self.set_reg(reg, val);

        self.load = (RegisterIndex(0), 0);

        self.decode_and_execute(instruction);

        self.regs = self.out_regs;
    }

    pub fn load32(&mut self, addr: u32) -> u32 {
        self.inter.load32(addr)
    }

    pub fn load8(&mut self, addr: u32) -> u8 {
        self.inter.load8(addr)
    }

    pub fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.function() {
            0b000000 => match instruction.subfunction() {
                0b000000 => self.op_sll(instruction),
                0b000010 => self.op_srl(instruction),
                0b000011 => self.op_sra(instruction),
                0b100000 => self.op_add(instruction),
                0b101010 => self.op_slt(instruction),
                0b101011 => self.op_sltu(instruction),
                0b100100 => self.op_and(instruction),
                0b100101 => self.op_or(instruction),
                0b100001 => self.op_addu(instruction),
                0b011010 => self.op_div(instruction),
                0b011011 => self.op_divu(instruction),  
                0b010010 => self.op_mflo(instruction),
                0b010011 => self.op_mtlo(instruction),
                0b010000 => self.op_mfhi(instruction),
                0b010001 => self.op_mthi(instruction),
                0b100011 => self.op_subu(instruction),
                0b001000 => self.op_jr(instruction),
                0b001001 => self.op_jalr(instruction),
                0b001100 => self.op_syscall(instruction),
                _        => panic!("unhandled instruction {:#x}", instruction.0)
            }
            0b000001 => self.op_bxx(instruction),
            0b000010 => self.op_j(instruction),
            0b000011 => self.op_jal(instruction),
            0b001111 => self.op_lui(instruction),
            0b001100 => self.op_andi(instruction),
            0b001101 => self.op_ori(instruction),
            0b001010 => self.op_slti(instruction),
            0b001011 => self.op_sltiu(instruction),
            0b101011 => self.op_sw(instruction),
            0b101001 => self.op_sh(instruction),
            0b101000 => self.op_sb(instruction),
            0b001001 => self.op_addiu(instruction),
            0b001000 => self.op_addi(instruction),
            0b000101 => self.op_bne(instruction),
            0b000100 => self.op_beq(instruction),
            0b000110 => self.op_blez(instruction),
            0b000111 => self.op_bgtz(instruction),
            0b100011 => self.op_lw(instruction),
            0b100000 => self.op_lb(instruction),
            0b100100 => self.op_lbu(instruction),
            0b010000 => self.op_cop0(instruction),
            _        => panic!("Unhandled_instruction_{:#x}", instruction.0)
        }
    }

    //branch to immediate value
    fn branch(&mut self, offset: u32) {
        //32 bits align to the offset
        let offset = offset << 2;

        let mut pc = self.next_pc;

        pc = pc.wrapping_add(offset);

        //compensate for hardcoded wrapping pc + 4 on run_next_instruction
        pc = pc.wrapping_sub(4);
        
        self.next_pc = pc;

        self.branch = true;
    }

    //coprocessor 0 opcode
    fn op_cop0(&mut self, instruction: Instruction) {
        match instruction.cop_opcode() {
            0b00100 => self.op_mtc0(instruction),
            0b00000 => self.op_mfc0(instruction),
            0b10000 => self.op_rfe(instruction),
            _       => panic!("unhandle coprocessor instruction {:#x}", instruction.0)
        }
    }

    //load upper immediate
    fn op_lui(&mut self, instruction: Instruction) {
        let i = instruction.imm();
        let t = instruction.t();

        let v = i << 16; //low 16 bits are set to zero

        self.set_reg(t, v);
    }

    //immediate bitwise and
    fn op_andi(&mut self, instruction: Instruction) {
        let i = instruction.imm();
        let t = instruction.t();
        let s = instruction.s();

        let v = self.reg(s) & i;

        self.set_reg(t, v);
    }

    //or immediate
    fn op_ori(&mut self, instruction: Instruction) {
        let i = instruction.imm();
        let t = instruction.t();
        let s = instruction.s();

        let v = self.reg(s) | i;

        self.set_reg(t, v);
    }

    //bitwise op
    fn op_or(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s) | self.reg(t);

        self.set_reg(d, v);
    }

    //bitwise and
    fn op_and(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s) & self.reg(t);

        self.set_reg(d, v);
    }

    //store signed word
    fn op_sw(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            //cache is isolated ignore write
            println!("ignoring store while cache is isolated");
            return;
        }
        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);
        let v = self.reg(t);

        self.store32(addr, v);
    }

    //store halfword
    fn op_sh(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            //cache is isolated ignore write
            println!("ignoring store while cache is isolated");
            return;
        }
        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);
        let v = self.reg(t);

        self.store16(addr, v as u16);
    }

    //store byte
    fn op_sb(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            //cache is isolated ignore write
            println!("ignoring store while cache is isolated");
            return;
        }
        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);
        let v = self.reg(t);

        self.store8(addr, v as u8);
    }

    //shift left logical
    fn op_sll(&mut self, instruction: Instruction) {
        let i = instruction.shift();
        let t = instruction.t();
        let d = instruction.d();

        let v = self.reg(t) << i;

        self.set_reg(d, v);
    }

    //shift right arithmetic
    fn op_sra(&mut self, instruction: Instruction) {
        let i = instruction.shift();
        let t = instruction.t();
        let d = instruction.d();

        let v = (self.reg(t) as i32) >> i;

        self.set_reg(d, v as u32);
    }

    //shift right logical
    fn op_srl(&mut self, instruction: Instruction) {
        let i = instruction.shift();
        let d = instruction.d();
        let t = instruction.t();

        let v = self.reg(t) >> i;

        self.set_reg(d, v);
    }

    //set if less than signed
    fn op_slt(&mut self, instruction: Instruction) {
        let s = instruction.s();
        let t = instruction.t();
        let d = instruction.d();

        let s = self.reg(s) as i32;
        let t = self.reg(t) as i32;

        let v = s < t;

        self.set_reg(d, v as u32);
    }

    //Set if less than unsigned
    fn op_sltu(&mut self, instruction: Instruction) {
        let t = instruction.t();
        let s = instruction.s();
        let d = instruction.d();

        let v = self.reg(s) < self.reg(t);

        self.set_reg(d, v as u32);
    }

    //set if less than immediate unsigned
    fn op_sltiu(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s) < i;

        self.set_reg(t, v as u32);
    }

    //set if less than immediate
    fn op_slti(&mut self, instruction: Instruction) {
        let i = instruction.imm_se() as i32;
        let s = instruction.s();
        let t = instruction.t();

        let v = (self.reg(s) as i32) < i;

        self.set_reg(t, v as u32);
    }

    //add immediate unsigned
    fn op_addiu(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let v = self.reg(s).wrapping_add(i);

        self.set_reg(t, v);
    }

    //add unsigned
    fn op_addu(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();
        let t = instruction.t();

        let v = self.reg(s).wrapping_add(self.reg(t));

        self.set_reg(d, v);
    }

    //substract unsigned
    fn op_subu(&mut self, instruction: Instruction) {
        let s = instruction.s();
        let d = instruction.d();
        let t = instruction.t();

        let v = self.reg(s).wrapping_sub(self.reg(t));

        self.set_reg(d, v);
    }

    //add immediate unsigned and check overflow
    fn op_addi(&mut self, instruction: Instruction) {
        let i = instruction.imm_se() as i32;
        let t = instruction.t();
        let s = instruction.s();

        let s = self.reg(s) as i32;

        let v = match s.checked_add(i) {
            Some(v) => v as u32,
            None => panic!("ADDI overflow"),
        };

        self.set_reg(t, v);
    }

    //add and generate exception on overflow
    fn op_add(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let t = instruction.t();
        let s = instruction.s();

        let s = self.reg(s) as i32;
        let t = self.reg(t) as i32;

        let v = match s.checked_add(t) {
            Some(v) => v as u32,
            None => panic!("ADD overflow"),
        };

        self.set_reg(d, v);
    }

    //divide signed
    fn op_div(&mut self, instruction: Instruction) {
        let s = instruction.s();
        let t = instruction.t();

        let n = self.reg(s) as i32;
        let d = self.reg(t) as i32;

        if d == 0 {
            self.hi = n as u32;
            
            if n >= 0 {
                self.lo = 0xffffffff;
            } else {
                self.lo = 1;
            }
        } else if n as u32 == 0x8000000 && d == -1 {
            self.hi = 0;
            self.lo = 0x8000000;
        } else {
            self.hi = (n % d) as u32;
            self.lo = (n / d) as u32;
        }
    }

    //divide unsigned
    fn op_divu(&mut self, instruction: Instruction) {
        let s = instruction.s();
        let t = instruction.t();

        let n = self.reg(s);
        let d = self.reg(t);

        if d == 0 {
            self.hi = n;
            self.lo = 0xffffffff;
        } else {
            self.hi = n % d;
            self.lo = n / d;
        }
    }

    //move from LO
    fn op_mflo(&mut self, instruction: Instruction) {
        let d = instruction.d();

        let lo = self.lo;

        self.set_reg(d, lo);
    }

    //move to LO
    fn op_mtlo(&mut self, instruction: Instruction) {
        let s = instruction.s();

        self.lo = self.reg(s);
    }

    //move from HH
    fn op_mfhi(&mut self, instruction: Instruction) {
        let d = instruction.d();

        let hi = self.hi;

        self.set_reg(d, hi);
    }

    //mote to HI
    fn op_mthi(&mut self, instruction: Instruction) {
        let s = instruction.s();

        self.hi = self.reg(s);
    }

    //jump
    fn op_j(&mut self, instruction: Instruction) {
        let i = instruction.imm_jump();

        self.next_pc = (self.pc & 0xf0000000) | (i << 2);

        self.branch = true;
    }

    //jump and link
    fn op_jal(&mut self, instruction: Instruction) {
        let ra = self.next_pc;

        self.set_reg(RegisterIndex(31), ra);

        self.op_j(instruction);
    }

    //jump return
    fn op_jr(&mut self, instruction: Instruction) {
        let s = instruction.s();

        self.next_pc = self.reg(s);

        self.branch = true;
    }

    //jump and link register
    fn op_jalr(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();

        let ra = self.next_pc;

        self.set_reg(d, ra);

        self.next_pc = self.reg(s);

        self.branch = true;
    }

    //branch if not equal
    fn op_bne(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        if self.reg(s) != self.reg(t) {
            self.branch(i);
        }
    }

    //branch if equal
    fn op_beq(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        if self.reg(s) == self.reg(t) {
            self.branch(i);
        }
    }

    //branch if greater than zero
    fn op_bgtz(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();

        let v = self.reg(s) as i32;

        if v > 0 {
            self.branch(i);
        }
    }

    //branch if less than zero
    fn op_blez(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();

        let v = self.reg(s) as i32;

        if v <= 0 {
            self.branch(i);
        }
    }

    //various branch instructions that share MSB: BGEZ, BLTZ, BGEZAL, BLTZAL
    //bits 16 and 20 are to choose which one to use
    fn op_bxx(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();

        let instruction = instruction.0;

        let is_bgez = (instruction >> 16) & 1;
        let is_link = (instruction >> 20) & 1 != 0;

        let v = self.reg(s) as i32;

        let test = (v < 0) as u32;

        let test = test ^ is_bgez;

        if test != 0 {
            if is_link {
                let ra = self.pc;

                self.set_reg(RegisterIndex(31), ra)
            }

            self.branch(i);
        }
    }

    //load word
    fn op_lw(&mut self, instruction: Instruction) {
        if self.sr & 0x10000 != 0 {
            println!("ignoring load while cache is isolated");
            return;
        }

        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load32(addr);

        self.set_reg(t, v);

        self.load = (t, v);
    }

    //load byte
    fn op_lb(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load8(addr) as i8;

        self.load = (t, v as u32);
    }

    //load byte unsigned
    fn op_lbu(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let addr = self.reg(s).wrapping_add(i);

        let v = self.load8(addr);

        self.load = (t, v as u32);
    }

    //move to coprocessor 0
    fn op_mtc0(&mut self, instruction: Instruction) {
        let cpu_r = instruction.t();
        let cop_r = instruction.d().0;

        let v = self.reg(cpu_r);

        match cop_r {
            3 | 5 | 6 | 7 | 9 | 11 => { //breakpoints register possibly unused in most games
                if v != 0 {
                    panic!("unhandle write to cop0 {}", cop_r)
                }
            }
            12 => self.sr = v,
            13 => { //CAUSE register
                if v != 0 {
                    panic!("unhandle write to CAUSE {}", cop_r)
                }
            }
            n => panic!("unhandle cop0 register {:#x}", n),
        }
    }

    //move from coprocesor 0
    fn op_mfc0(&mut self, instruction: Instruction) {
        let cpu_r = instruction.t();
        let cop_r = instruction.d().0;

        let v = match cop_r {
            12 => self.sr,
            13 => self.cause,
            14 => self.epc,
            _ => panic!("unhandle read from cop0r {:#x}", cop_r),
        };

        self.load = (cpu_r, v);
    }

    //return from exception
    fn op_rfe(&mut self, instruction: Instruction) {
        //unlike normal MIPS processors, there is only one instruction with this encoding for the PSX
        //This code is to make sure buggy code is trying to run in the emulator
        if instruction.0 & 0x3f != 0b010000 {
            panic!("invalid cop0 instruction {}", instruction.0);
        }

        //undo the action when the exception started
        let mode = self.sr & 0x3f;
        self.sr &= !0x3f;
        self.sr |= mode >> 2; 
    }

    fn exception(&mut self, cause: Exception) {
        //handler depends on the BEV bit
        let handler = match self.sr & (1 << 22) != 0 {
            true => 0xbfc00180,
            false => 0x80000080,
        };

        let mode = self.sr & 0x3f;
        self.sr &= 0x3f;
        self.sr |= (mode << 2) & 0x3f;

        self.cause = (cause as u32) << 2;

        self.epc = self.current_pc;

        //if an exception occurs durin a delay slot, EPC points to the branch instruction
        //and bit 31 of CAUSE is set
        if self.delay_slot {
            self.epc = self.epc.wrapping_add(4);
            self.cause |= 1 << 31;
        }

        //exceptions don't have branch delay, we jump directly
        self.pc = handler;
        self.next_pc = self.pc.wrapping_add(4);
    }

    fn op_syscall(&mut self, _: Instruction) {
        self.exception(Exception::SysCall);
    }
}

enum Exception {
    SysCall = 0x8,
}

#[derive(Copy, Clone)]
pub struct Instruction(u32);

impl Instruction {
    //get bits 31 to 26 of instruction
    fn function(self) -> u32 {
        let Instruction(op) = self;

        op >> 26
    }

    //gets bits [5:0] of the instruction
    fn subfunction(self) -> u32 {
        let Instruction(op) = self;

        op & 0x3f
    }

    //gets the register index in bits from 20 to 16 bits 
    fn t(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex((op >> 16) & 0x1f)
    }

    //returns register index in bits [25:21]
    fn s(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex((op >> 21) & 0x1f)
    }

    //gets register index in [15:11]
    fn d(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex((op >> 11) & 0x1f)
    }

    //get immediate bits value (16 to 0)
    fn imm(self) -> u32 {
        let Instruction(op) = self;

        op & 0xffff
    }

    //like imm but but it gets the signed value in bits
    fn imm_se(self) -> u32 {
        let Instruction(op) = self;

        let v = (op & 0xffff) as i16;

        v as u32
    }

    //get the shift immediate values in [10:6]
    fn shift(self) -> u32 {
        let Instruction(op) = self;

        (op >> 6) & 0x1f
    }

    //jump to target in bits 25-0
    fn imm_jump(self) -> u32 {
        let Instruction(op) = self;

        op & 0x3ffffff
    }

    fn cop_opcode(self) -> u32 {
        let Instruction(op) = self;

        (op >> 21) & 0x1f
    }
}