use std::path::Path;
use std::fs::File;
use std::io::Read;

use crate::map;

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

    pub fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.function() {
            0b000000 => match instruction.subfunction() {
                0b000000 => self.op_sll(instruction),
                0b000101 => self.op_or(instruction),
                _        => panic!("unhandled instruction {:#x}", instruction.0)
            }
            0b000010 => self.op_j(instruction),
            0b001111 => self.op_lui(instruction),
            0b001101 => self.op_ori(instruction),
            0b101011 => self.op_sw(instruction),
            0b001001 => self.op_addiu(instruction),
            0b001000 => self.op_addi(instruction),
            0b000101 => self.op_bne(instruction),
            0b100011 => self.op_lw(instruction),
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

    //shift left logical
    fn op_sll(&mut self, instruction: Instruction) {
        let i = instruction.shift();
        let t = instruction.t();
        let d = instruction.d();

        let v = self.reg(t) << i;

        self.set_reg(d, v);
    }

    //add immediate unsigned
    fn op_addiu(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let t = instruction.t();
        let s = instruction.s();

        let v = self.reg(s).wrapping_add(i);

        self.set_reg(t, v);
    }

    //add immediate unsigned and check overflow
    fn op_addi(&mut self, instruction: Instruction) {
        let i = instruction.imm() as i32;
        let t = instruction.t();
        let s = instruction.s();

        let s = self.reg(s) as i32;

        let v = match s.checked_add(i) {
            Some(v) => v as u32,
            None => panic!("ADDI overflow"),
        };

        self.set_reg(t, v);
    }

    //jump
    fn op_j(&mut self, instruction: Instruction) {
        let i = instruction.imm_jump();

        self.pc = (self.pc & 0xf0000000) | (i << 2);
    }

    //branch not equal
    fn op_bne(&mut self, instruction: Instruction) {
        let i = instruction.imm_se();
        let s = instruction.s();
        let t = instruction.t();

        if self.reg(s) != self.reg(t) {
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

}

const BIOS_SIZE: u64 = 512 * 1024;

pub struct Bios {
    data: Vec<u8>
}

impl Bios {
    pub fn new(path: &Path) -> Result<Bios, &str> {
        let mut file = File::open(path).expect("Bios not found");
        
        let mut data: Vec<u8> = Vec::<u8>::new();

        file.read_to_end(&mut data).unwrap();

        if data.len() == BIOS_SIZE as usize {
            Ok(Bios { data: data })
        } else {
            Err("invalid bios size")
        }
    }

    pub fn load32(&self, offset: u32) -> u32 {
        let offset = offset as usize;

        let b0 = self.data[offset + 0] as u32;
        let b1 = self.data[offset + 1] as u32;
        let b2 = self.data[offset + 2] as u32;
        let b3 = self.data[offset + 3] as u32;

        b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }
}

pub struct Interconnect {
    bios: Bios,
    ram: Ram,
}

impl Interconnect {
    pub fn new(bios: Bios, ram: Ram) -> Self {
        Interconnect {
            bios: bios,
            ram: ram
        }
    }

    pub fn load32(&self, addr: u32) -> u32{
        if addr % 4 != 0 {
            panic!("unaligned_load32_adress_{:08x}", addr);
        }

        if let Some(offset) = map::BIOS.contains(addr) {
            return self.bios.load32(offset)
        }

        if let Some(offset) = map::RAM.contains(addr) {
            return self.ram.load32(offset)
        }
        panic!("unhandled_fecth_at_address_{:08x}", addr);
    }

    pub fn store32(&mut self, addr: u32, val: u32) {
        if addr % 4 != 0 {
            panic!("unaligned_load32_adress_{:08x}", addr);
        }

        if let Some(offset) = map::MEM_CONTROL.contains(addr) {
            match offset {
                0 => if val != 0x1f000000 {
                    panic!("Bad_expansion_1_base_address_{:#x}", val);
                }
                4 => if val != 0x1f802000 {
                    panic!("Bad_expansion_2_base_address_{:#x}", val);
                }
                _ => println!("unhandle_write_to_MEMCONTROL_register"),
            }

            return;
        }

        if let Some(offset) = map::RAM.contains(addr) {
            self.ram.store32(offset, val);

            return;
        }

        if let Some(offset) = map::RAM_SIZE.contains(addr) {
            println!("Ram set attempted at {:#x}", offset);
            return;
        }

        if let Some(offset) = map::CACHE_CONTROL.contains(addr) {
            println!("cache control attempted at {:#x}", offset);
            return;
        }


        panic!("unhandled_store32_at_address_{:08x}", addr);
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

        op & 0x1f
    }

    //gets the register index in bits from 20 to 16 bits 
    fn t(self) -> RegisterIndex {
        let Instruction(op) = self;

        RegisterIndex((op >> 16) & 0x1f)
    }

    //returns register index in bits [26:21]
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

pub struct Ram {
    data: Vec<u8>
}

impl Ram {
    pub fn new() -> Self {
        let data = vec![0xca; 2 * 1024 * 1024];

        Ram { data: data }
    }

    pub fn load32(&self, offset: u32) -> u32 {
        let offset = offset as usize;

        let b0 = self.data[offset + 0] as u32;
        let b1 = self.data[offset + 1] as u32;
        let b2 = self.data[offset + 2] as u32;
        let b3 = self.data[offset + 3] as u32;

        b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }

    pub fn store32(&mut self, offset: u32, val: u32) {
        let offset = offset as usize;

        let b0 = val as u8;
        let b1 = (val >> 8) as u8;
        let b2 = (val >> 16) as u8;
        let b3 = (val >> 24) as u8;

        self.data[offset + 0] = b0;
        self.data[offset + 1] = b1;
        self.data[offset + 2] = b2;
        self.data[offset + 3] = b3;
    }
}