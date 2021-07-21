use crate::memory::Interconnect;

#[derive(Copy, Clone)]
struct RegisterIndex(u32);
pub struct Cpu {
    pc: u32, //program counter
    regs: [u32; 32], //general purpose register, first one must always contains zero
    inter: Interconnect, //memory interface
    next_instruction: Instruction,
    sr: u32, //status register from coprocesor 0
    out_regs: [u32; 32],
    load: (RegisterIndex, u32)
}

impl Cpu {
    pub fn new(inter: Interconnect) -> Self {
        //don't know what the reset values for register are so I will use simias guide values for reference and debugging
        let mut regs = [0xdeadbeef; 32];

        regs[0] = 0; //R0 remains zero as it should
        Cpu {
            pc: 0xbfc00000, //beginning of the BIOS
            regs: regs,
            inter: inter,
            next_instruction: Instruction(0x0), //NOP
            sr: 0,
            out_regs: regs,
            load: (RegisterIndex(0), 0)
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

        let instruction = self.next_instruction;

        self.next_instruction = Instruction(self.load32(pc));

        self.pc = pc.wrapping_add(4);

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
                0b100000 => self.op_add(instruction),
                0b101011 => self.op_sltu(instruction),
                0b100100 => self.op_and(instruction),
                0b100101 => self.op_or(instruction),
                0b100001 => self.op_addu(instruction),
                0b001000 => self.op_jr(instruction),
                0b001001 => self.op_jalr(instruction),
                _        => panic!("unhandled instruction {:#x}", instruction.0)
            }
            0b000010 => self.op_j(instruction),
            0b000011 => self.op_jal(instruction),
            0b001111 => self.op_lui(instruction),
            0b001100 => self.op_andi(instruction),
            0b001101 => self.op_ori(instruction),
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

        let mut pc = self.pc;

        pc = pc.wrapping_add(offset);

        //compensate for hardcoded wrapping pc + 4 on run_next_instruction
        pc = pc.wrapping_sub(4);

        self.pc = pc;
    }

    //coprocessor 0 opcode
    fn op_cop0(&mut self, instruction: Instruction) {
        match instruction.cop_opcode() {
            0b00100 => self.op_mtc0(instruction),
            0b00000 => self.op_mfc0(instruction),
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

        println!("{:#x}", self.pc - 4);

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

    //Set on less than unsigned
    fn op_sltu(&mut self, instruction: Instruction) {
        let t = instruction.t();
        let s = instruction.s();
        let d = instruction.d();

        let v = self.reg(s) < self.reg(t);

        self.set_reg(d, v as u32);
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

    //jump
    fn op_j(&mut self, instruction: Instruction) {
        let i = instruction.imm_jump();

        self.pc = (self.pc & 0xf0000000) | (i << 2);
    }

    //jump and link
    fn op_jal(&mut self, instruction: Instruction) {
        let ra = self.pc;

        self.set_reg(RegisterIndex(31), ra);

        self.op_j(instruction);
    }

    //jump return
    fn op_jr(&mut self, instruction: Instruction) {
        let s = instruction.s();

        self.pc = self.reg(s);
    }

    //jump and link register
    fn op_jalr(&mut self, instruction: Instruction) {
        let d = instruction.d();
        let s = instruction.s();

        let ra = self.pc;

        self.set_reg(d, ra);

        self.pc = self.reg(s);
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

    fn op_blez(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();

        let v = self.reg(s) as i32;

        if v <= 0 {
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

    fn op_mfc0(&mut self, instruction: Instruction) {
        let cpu_r = instruction.t();
        let cop_r = instruction.d().0;

        let v = match cop_r {
            12 => self.sr,
            13 => panic!("unhandle read from CAUSE {}", cop_r), //CAUSE Register
            _ => panic!("unhandle read from cop0r {:#x}", cop_r),
        };

        self.load = (cpu_r, v);
    }

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