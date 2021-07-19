mod cpu;
mod memory;

use std::path::Path;

fn main() {
    let bios = memory::Bios::new(&Path::new("resources/SCPH1001.BIN")).unwrap();

    let ram = memory::Ram::new();

    let inter = memory::Interconnect::new(bios, ram);

    let mut cpu = cpu::Cpu::new(inter);

    loop {
        cpu.run_next_instruction();
    }
}
