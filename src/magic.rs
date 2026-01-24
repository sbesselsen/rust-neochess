/// Magic bitboards for fast sliding piece attack generation.
///
/// This module implements magic bitboards, a technique for O(1) lookup of
/// sliding piece (rook, bishop, queen) attacks. Instead of computing attacks
/// at runtime by ray-casting, we use a precomputed lookup table indexed by
/// a "magic" hash of the occupancy on the relevant squares.
///
/// IMPORTANT: This module uses standard bit indexing (bit 0 = LSB, bit 63 = MSB).
/// The board module uses inverted indexing (board_index i = bit position 63-i).
/// Conversion must happen at the interface.
use std::sync::OnceLock;

/// Magic numbers for rook attacks (from Stockfish)
const ROOK_MAGICS: [u64; 64] = [
    0x0080001020400080,
    0x0040001000200040,
    0x0080081000200080,
    0x0080040800100080,
    0x0080020400080080,
    0x0080010200040080,
    0x0080008001000200,
    0x0080002040800100,
    0x0000800020400080,
    0x0000400020005000,
    0x0000801000200080,
    0x0000800800100080,
    0x0000800400080080,
    0x0000800200040080,
    0x0000800100020080,
    0x0000800040800100,
    0x0000208000400080,
    0x0000404000201000,
    0x0000808010002000,
    0x0000808008001000,
    0x0000808004000800,
    0x0000808002000400,
    0x0000010100020004,
    0x0000020000408104,
    0x0000208080004000,
    0x0000200040005000,
    0x0000100080200080,
    0x0000080080100080,
    0x0000040080080080,
    0x0000020080040080,
    0x0000010080800200,
    0x0000800080004100,
    0x0000204000800080,
    0x0000200040401000,
    0x0000100080802000,
    0x0000080080801000,
    0x0000040080800800,
    0x0000020080800400,
    0x0000020001010004,
    0x0000800040800100,
    0x0000204000808000,
    0x0000200040008080,
    0x0000100020008080,
    0x0000080010008080,
    0x0000040008008080,
    0x0000020004008080,
    0x0000010002008080,
    0x0000004081020004,
    0x0000204000800080,
    0x0000200040008080,
    0x0000100020008080,
    0x0000080010008080,
    0x0000040008008080,
    0x0000020004008080,
    0x0000800100020080,
    0x0000800041000080,
    0x00FFFCDDFCED714A,
    0x007FFCDDFCED714A,
    0x003FFFCDFFD88096,
    0x0000040810002101,
    0x0001000204080011,
    0x0001000204000801,
    0x0001000082000401,
    0x0001FFFAABFAD1A2,
];

/// Magic numbers for bishop attacks (from Stockfish)
const BISHOP_MAGICS: [u64; 64] = [
    0x0002020202020200,
    0x0002020202020000,
    0x0004010202000000,
    0x0004040080000000,
    0x0001104000000000,
    0x0000821040000000,
    0x0000410410400000,
    0x0000104104104000,
    0x0000040404040400,
    0x0000020202020200,
    0x0000040102020000,
    0x0000040400800000,
    0x0000011040000000,
    0x0000008210400000,
    0x0000004104104000,
    0x0000002082082000,
    0x0004000808080800,
    0x0002000404040400,
    0x0001000202020200,
    0x0000800802004000,
    0x0000800400A00000,
    0x0000200100884000,
    0x0000400082082000,
    0x0000200041041000,
    0x0002080010101000,
    0x0001040008080800,
    0x0000208004010400,
    0x0000404004010200,
    0x0000840000802000,
    0x0000404002011000,
    0x0000808001041000,
    0x0000404000820800,
    0x0001041000202000,
    0x0000820800101000,
    0x0000104400080800,
    0x0000020080080080,
    0x0000404040040100,
    0x0000808100020100,
    0x0001010100020800,
    0x0000808080010400,
    0x0000820820004000,
    0x0000410410002000,
    0x0000082088001000,
    0x0000002011000800,
    0x0000080100400400,
    0x0001010101000200,
    0x0002020202000400,
    0x0001010101000200,
    0x0000410410400000,
    0x0000208208200000,
    0x0000002084100000,
    0x0000000020880000,
    0x0000001002020000,
    0x0000040408020000,
    0x0004040404040000,
    0x0002020202020000,
    0x0000104104104000,
    0x0000002082082000,
    0x0000000020841000,
    0x0000000000208800,
    0x0000000010020200,
    0x0000000404080200,
    0x0000040404040400,
    0x0002020202020200,
];

/// Number of bits used for rook attack index at each square
const ROOK_BITS: [u32; 64] = [
    12, 11, 11, 11, 11, 11, 11, 12, 11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11, 12, 11, 11, 11, 11, 11, 11, 12,
];

/// Number of bits used for bishop attack index at each square
const BISHOP_BITS: [u32; 64] = [
    6, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 5, 5, 5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5, 5, 5, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 6,
];

/// Combined magic bitboard data for both rooks and bishops
struct MagicTables {
    rook_masks: [u64; 64],
    bishop_masks: [u64; 64],
    rook_attacks: Vec<Vec<u64>>,
    bishop_attacks: Vec<Vec<u64>>,
}

/// Static storage for all magic bitboard tables, lazily initialized on first use
static MAGIC_TABLES: OnceLock<MagicTables> = OnceLock::new();

/// Get the magic tables, initializing them if needed
#[inline]
fn tables() -> &'static MagicTables {
    MAGIC_TABLES.get_or_init(|| {
        // Initialize rook masks
        let mut rook_masks = [0u64; 64];
        for (sq, mask) in rook_masks.iter_mut().enumerate() {
            *mask = rook_mask(sq as u32);
        }

        // Initialize bishop masks
        let mut bishop_masks = [0u64; 64];
        for (sq, mask) in bishop_masks.iter_mut().enumerate() {
            *mask = bishop_mask(sq as u32);
        }

        // Initialize rook attacks - allocate directly on heap to avoid stack overflow
        let mut rook_attacks: Vec<Vec<u64>> = (0..64).map(|_| vec![0u64; 4096]).collect();
        for sq in 0..64 {
            let mask = rook_masks[sq];
            let bits = ROOK_BITS[sq];
            let n = 1 << mask.count_ones();

            for i in 0..n {
                let occupancy = index_to_occupancy(i, mask);
                let index = magic_index(occupancy, ROOK_MAGICS[sq], bits);
                rook_attacks[sq][index] = rook_attacks_slow(sq as u32, occupancy);
            }
        }

        // Initialize bishop attacks - allocate directly on heap to avoid stack overflow
        let mut bishop_attacks: Vec<Vec<u64>> = (0..64).map(|_| vec![0u64; 512]).collect();
        for sq in 0..64 {
            let mask = bishop_masks[sq];
            let bits = BISHOP_BITS[sq];
            let n = 1 << mask.count_ones();

            for i in 0..n {
                let occupancy = index_to_occupancy(i, mask);
                let index = magic_index(occupancy, BISHOP_MAGICS[sq], bits);
                bishop_attacks[sq][index] = bishop_attacks_slow(sq as u32, occupancy);
            }
        }

        MagicTables {
            rook_masks,
            bishop_masks,
            rook_attacks,
            bishop_attacks,
        }
    })
}

/// Compute magic index from occupancy
#[inline(always)]
fn magic_index(occupancy: u64, magic: u64, bits: u32) -> usize {
    ((occupancy.wrapping_mul(magic)) >> (64 - bits)) as usize
}

/// Get rook attacks for a given square and board occupancy.
/// Uses standard bit indexing (bit 0 = square a1, bit 63 = square h8).
/// Tables are lazily initialized on first use.
#[inline(always)]
pub fn get_rook_attacks(sq: u32, occupancy: u64) -> u64 {
    let t = tables();
    let masked = occupancy & t.rook_masks[sq as usize];
    let index = magic_index(masked, ROOK_MAGICS[sq as usize], ROOK_BITS[sq as usize]);
    t.rook_attacks[sq as usize][index]
}

/// Get bishop attacks for a given square and board occupancy.
/// Uses standard bit indexing (bit 0 = square a1, bit 63 = square h8).
/// Tables are lazily initialized on first use.
#[inline(always)]
pub fn get_bishop_attacks(sq: u32, occupancy: u64) -> u64 {
    let t = tables();
    let masked = occupancy & t.bishop_masks[sq as usize];
    let index = magic_index(masked, BISHOP_MAGICS[sq as usize], BISHOP_BITS[sq as usize]);
    t.bishop_attacks[sq as usize][index]
}

/// Get queen attacks (combination of rook and bishop attacks)
#[inline(always)]
pub fn get_queen_attacks(sq: u32, occupancy: u64) -> u64 {
    get_rook_attacks(sq, occupancy) | get_bishop_attacks(sq, occupancy)
}

/// Generate the mask of relevant occupancy squares for a rook on the given square.
/// Uses standard bit indexing (bit 0 = a1, bit 63 = h8).
/// Excludes the edges of the board (since a blocker on the edge doesn't matter).
fn rook_mask(sq: u32) -> u64 {
    let rank = sq / 8; // 0-7, where 0 = rank 1
    let file = sq % 8; // 0-7, where 0 = file a
    let mut mask = 0u64;

    // North (increasing rank)
    for r in (rank + 1)..7 {
        mask |= 1u64 << (r * 8 + file);
    }
    // South (decreasing rank)
    for r in 1..rank {
        mask |= 1u64 << (r * 8 + file);
    }
    // East (increasing file)
    for f in (file + 1)..7 {
        mask |= 1u64 << (rank * 8 + f);
    }
    // West (decreasing file)
    for f in 1..file {
        mask |= 1u64 << (rank * 8 + f);
    }

    mask
}

/// Generate the mask of relevant occupancy squares for a bishop on the given square.
/// Uses standard bit indexing (bit 0 = a1, bit 63 = h8).
fn bishop_mask(sq: u32) -> u64 {
    let rank = sq / 8;
    let file = sq % 8;
    let mut mask = 0u64;

    // Northeast
    let mut r = rank + 1;
    let mut f = file + 1;
    while r < 7 && f < 7 {
        mask |= 1u64 << (r * 8 + f);
        r += 1;
        f += 1;
    }

    // Northwest
    r = rank + 1;
    f = file.wrapping_sub(1);
    while r < 7 && f > 0 && f < 8 {
        mask |= 1u64 << (r * 8 + f);
        r += 1;
        f = f.wrapping_sub(1);
    }

    // Southeast
    r = rank.wrapping_sub(1);
    f = file + 1;
    while r > 0 && r < 8 && f < 7 {
        mask |= 1u64 << (r * 8 + f);
        r = r.wrapping_sub(1);
        f += 1;
    }

    // Southwest
    r = rank.wrapping_sub(1);
    f = file.wrapping_sub(1);
    while r > 0 && r < 8 && f > 0 && f < 8 {
        mask |= 1u64 << (r * 8 + f);
        r = r.wrapping_sub(1);
        f = f.wrapping_sub(1);
    }

    mask
}

/// Convert an index (0..2^popcount) to an occupancy bitboard.
fn index_to_occupancy(index: usize, mut mask: u64) -> u64 {
    let mut occupancy = 0u64;
    let mut bit_index = 0;

    while mask != 0 {
        let sq = mask.trailing_zeros();
        mask &= mask - 1; // Clear lowest set bit

        if (index & (1 << bit_index)) != 0 {
            occupancy |= 1u64 << sq;
        }
        bit_index += 1;
    }

    occupancy
}

/// Compute rook attacks using the slow method (for table initialization).
/// Uses standard bit indexing.
fn rook_attacks_slow(sq: u32, occupancy: u64) -> u64 {
    let rank = sq / 8;
    let file = sq % 8;
    let mut attacks = 0u64;

    // North
    for r in (rank + 1)..8 {
        let bit = 1u64 << (r * 8 + file);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
    }
    // South
    for r in (0..rank).rev() {
        let bit = 1u64 << (r * 8 + file);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
    }
    // East
    for f in (file + 1)..8 {
        let bit = 1u64 << (rank * 8 + f);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
    }
    // West
    for f in (0..file).rev() {
        let bit = 1u64 << (rank * 8 + f);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
    }

    attacks
}

/// Compute bishop attacks using the slow method (for table initialization).
/// Uses standard bit indexing.
fn bishop_attacks_slow(sq: u32, occupancy: u64) -> u64 {
    let rank = sq / 8;
    let file = sq % 8;
    let mut attacks = 0u64;

    // Northeast
    let mut r = rank + 1;
    let mut f = file + 1;
    while r < 8 && f < 8 {
        let bit = 1u64 << (r * 8 + f);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
        r += 1;
        f += 1;
    }

    // Northwest
    r = rank + 1;
    f = file.wrapping_sub(1);
    while r < 8 && f < 8 {
        let bit = 1u64 << (r * 8 + f);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
        r += 1;
        f = f.wrapping_sub(1);
    }

    // Southeast
    r = rank.wrapping_sub(1);
    f = file + 1;
    while r < 8 && f < 8 {
        let bit = 1u64 << (r * 8 + f);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
        r = r.wrapping_sub(1);
        f += 1;
    }

    // Southwest
    r = rank.wrapping_sub(1);
    f = file.wrapping_sub(1);
    while r < 8 && f < 8 {
        let bit = 1u64 << (r * 8 + f);
        attacks |= bit;
        if occupancy & bit != 0 {
            break;
        }
        r = r.wrapping_sub(1);
        f = f.wrapping_sub(1);
    }

    attacks
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rook_mask() {
        // e4 in standard indexing = square 28 (rank 3, file 4)
        let mask = rook_mask(28);
        // Should include squares on e-file and 4th rank, excluding edges
        assert!(mask.count_ones() > 0);
    }

    #[test]
    fn test_lazy_init_and_lookup() {
        // Test rook on e4 (standard sq=28) with no blockers
        let attacks = get_rook_attacks(28, 0);
        // Should attack all squares on e-file and 4th rank
        assert_eq!(attacks.count_ones(), 14);

        // Test bishop on e4 with no blockers
        let attacks = get_bishop_attacks(28, 0);
        assert!(attacks.count_ones() > 0);
    }

    #[test]
    fn test_rook_with_blockers() {
        // Rook on a1 (standard sq=0), blocker on a4 (sq=24)
        let blocker = 1u64 << 24;
        let attacks = get_rook_attacks(0, blocker);

        // Should reach a2, a3, a4 (capture) but not a5+
        assert!(attacks & (1u64 << 8) != 0); // a2
        assert!(attacks & (1u64 << 16) != 0); // a3
        assert!(attacks & (1u64 << 24) != 0); // a4 (capture)
        assert!(attacks & (1u64 << 32) == 0); // a5 blocked
    }

    #[test]
    fn test_bishop_with_blockers() {
        // Bishop on a1 (sq=0), blocker on c3 (sq=18)
        let blocker = 1u64 << 18;
        let attacks = get_bishop_attacks(0, blocker);

        // Should reach b2, c3 but not d4+
        assert!(attacks & (1u64 << 9) != 0); // b2
        assert!(attacks & (1u64 << 18) != 0); // c3 (capture)
        assert!(attacks & (1u64 << 27) == 0); // d4 blocked
    }
}
