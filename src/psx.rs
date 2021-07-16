use std::path::Path;
use std::fs::File;
use std::io::Read;

use crate::map;

pub struct Cpu {
    pc: u32, //program counter
    regs: [u32; 32], //general purpose register, first one must always contains zero
    inter: Interconnect //memory interface
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
        }
    }

    fn reg(&self, index: u32) -> u32 {
        self.regs[index as usize]
    }

    fn set_reg(&mut self, index: u32, val: u32) {
        self.regs[index as usize] = val;

        self.regs[0] = 0; //prevention just in case to keep it zero
    }

    fn store32(&mut self, addr: u32, val: u32) {
        self.inter.store32(addr, val);
    }

    pub fn run_next_instruction(&mut self) {
        let pc = self.pc;

        let instruction = self.load32(pc);

        self.pc = pc.wrapping_add(4);

        self.decode_and_execute(Instruction(instruction));
    }

    pub fn load32(&mut self, addr: u32) -> u32 {
        self.inter.load32(addr)
    }

    pub fn decode_and_execute(&mut self, instruction: Instruction) {
        match instruction.function() {
            0b000000 => match instruction.subfunction() {
                0b000000 => self.op_sll(instruction),
                _        => panic!("unhandled instruction {:#x}", instruction.0)
            }
            0b001111 => self.op_lui(instruction),
            0b001101 => self.op_ori(instruction),
            0b101011 => self.op_sw(instruction),
            0b001001 => self.op_addiu(instruction),
            _        => panic!("Unhandled_instruction_{:#x}", instruction.0)
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

    //store signed word
    fn op_sw(&mut self, instruction: Instruction) {
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
    bios : Bios ,
}

impl Interconnect {
    pub fn new(bios: Bios) -> Self {
        Interconnect {
            bios: bios
        }
    }

    pub fn load32(&self, addr: u32) -> u32{
        if addr % 4 != 0 {
            panic!("unaligned_load32_adress_{:08x}", addr);
        }

        if let Some(offset) = map::BIOS.contains(addr) {
            return self.bios.load32(offset)
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
                _ => print!("unhandle_write_to_MEMCONTROL_register"),
            }

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
    fn t(self) -> u32 {
        let Instruction(op) = self;

        (op >> 16) & 0x1f
    }

    //returns register index in bits [26:21]
    fn s(self) -> u32 {
        let Instruction(op) = self;

        (op >> 21) & 0x1f
    }

    //gets register index in [15:11]
    fn d(self) -> u32 {
        let Instruction(op) = self;

        (op >> 11) & 0x1f
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
}
