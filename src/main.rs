mod psx;
mod map;

use std::path::Path;

fn main() {
    let bios = psx::Bios::new(&Path::new("resources/SCPH1001.BIN")).unwrap();

    let inter = psx::Interconnect::new(bios);

    let mut cpu = psx::Cpu::new(inter);

    loop {
        cpu.run_next_instruction();
    }
}
