use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let path = std::path::Path::new(&out_dir).join("zobrist_constants.rs");

    // Generate random numbers for our Zobrist hashing. We want these to be
    // statically available in the easiest format without any delay.
    let mut rng = StdRng::seed_from_u64(0x16dbedd4360872b1);
    let mut output = String::from(
        "pub(crate) const ZOBRIST_PIECES: &'static [&'static [&'static [u64; 2]; 6]; 64] = &[",
    );
    for _ in 0..64 {
        output.push_str("&[\n");
        for _ in 0..6 {
            output.push_str(&format!(
                "  &[0x{:0>16x}, 0x{:0>16x}],\n",
                rng.next_u64(),
                rng.next_u64()
            ));
        }
        output.push_str("],\n");
    }
    output.push_str("];\n\n");
    output.push_str(&format!(
        "pub(crate) const ZOBRIST_BLACK_TO_MOVE: u64 = 0x{:0>16x};\n\n",
        rng.next_u64()
    ));
    output.push_str("pub(crate) const ZOBRIST_EN_PASSANT: &'static [u64; 64] = &[\n");
    for _ in 0..64 {
        output.push_str(&format!("  0x{:0>16x},\n", rng.next_u64()));
    }
    output.push_str("];\n");

    std::fs::write(&path, &output).unwrap();
}
