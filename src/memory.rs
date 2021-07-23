use std::path::Path;
use std::io::Read;
use std::fs::File;

pub mod map {
    //a trick to simplify figuring out what part of memory belongs the address
    const REGION_MASK: [u32; 8] = [
        0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, //KUSEG: 2048MB
        0x7fffffff, //KSEG0: 512MB
        0x1fffffff, //KSEG1: 512MB
        0xffffffff, 0xffffffff, //KSEG2: 1024MB
    ];

    //use the mask to get the region bits
    pub fn mask_region(addr: u32) -> u32 {
        let index = (addr >> 29) as usize;

        addr & REGION_MASK[index]
    }

    pub const BIOS: Range = Range(0x1fc00000, 512 * 1024);

    pub const RAM: Range = Range(0x00000000, 2 * 1024 * 1024);

    pub const SYS_CONTROL: Range = Range(0x1f801000, 36);

    pub const RAM_SIZE: Range = Range(0x1f801060, 4);

    pub const CACHE_CONTROL: Range = Range(0xfffe0130, 4);

    pub const SPU: Range = Range(0x1f801c00, 640);

    pub const EXPANSION_1: Range = Range(0x1f000000, 512 * 1024);
    pub const EXPANSION_2: Range = Range(0x1f802000, 66);

    pub const IRQ_CONTROL: Range = Range(0x1f801070, 8);

    pub const TIMERS: Range = Range(0x1f801100, 0x30);

    pub struct Range(u32, u32);

    impl Range {
        pub fn contains(self, addr: u32) -> Option<u32> {
            let Range   (start, length) = self;

            if addr >= start && addr < start + length {
                Some(addr - start)
            } else {
                None
            }
        }
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

    pub fn store8(&mut self, offset: u32, val: u8) {
        self.data[offset as usize] = val;
    }

    pub fn load8(&mut self, offset: u32) -> u8{
        self.data[offset as usize]
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
            panic!("unaligned_load32_adress_{:#x}", addr);
        }

        let abs_addr = map::mask_region(addr);

        if let Some(offset) = map::BIOS.contains(abs_addr) {
            return self.bios.load32(offset)
        }

        if let Some(offset) = map::RAM.contains(abs_addr) {
            return self.ram.load32(offset)
        }

        if let Some(offset) = map::IRQ_CONTROL.contains(abs_addr) {
            println!("IRQ control read {:#x}", offset);
            return 0;
        }
        panic!("unhandled_fecth_at_address_{:08x}", addr);
    }

    pub fn load8(&mut self, addr: u32) -> u8 {
        let abs_addr = map::mask_region(addr);

        if let Some(offset) = map::BIOS.contains(abs_addr) {
            return self.bios.load8(offset);
        }

        if let Some(offset) = map::RAM.contains(abs_addr) {
            return self.ram.load8(offset,);
        }

        if let Some(_) = map::EXPANSION_1.contains(addr) {
            return 0xff;
        }

        panic!("unhandle load8 at address {:#x}", addr);
    }

    pub fn store32(&mut self, addr: u32, val: u32) {
        if addr % 4 != 0 {
            panic!("unaligned_load32_adress_{:08x}", addr);
        }

        let addr = map::mask_region(addr);

        if let Some(offset) = map::SYS_CONTROL.contains(addr) {
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

        if let Some(offset) = map::IRQ_CONTROL.contains(addr) {
            println!("IRQ Control: {:#x} <- {:#x}", offset, val);
            return;
        }

        panic!("unhandled_store32_at_address_{:08x}", addr);
    }

    pub fn store16(&mut self, addr: u32, _val: u16) {
        if addr % 2 != 0 {
            panic!("unligned store16 address {:#x}", addr);
        }

        let abs_addr = map::mask_region(addr);

        if let Some(offset) = map::SPU.contains(abs_addr) {
            println!("unhandle write to SPU register {:#x}", offset);
            return
        }

        if let Some(offset) = map::TIMERS.contains(abs_addr) {
            println!("unhandle write to timer register {:#x}", offset);
            return
        }

        panic!("unhandle store16 address {:#x}", addr);
    }

    pub fn store8(&mut self, addr: u32, val: u8) {
        let abs_addr = map::mask_region(addr);

        if let Some(offset) = map::RAM.contains(abs_addr) {
            return self.ram.store8(offset, val)
        }

        if let Some(offset) = map::EXPANSION_2.contains(abs_addr) {
            println!("unhandled write to expansion 2 register {:#x}", offset);
            return;
        }

        panic!("unhandle store 8 into address {:#x}", addr);
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

    pub fn load8(&self, offset: u32) -> u8 {
        self.data[offset as usize]
    }
}