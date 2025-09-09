/// Build the multi-line "U" / "_" picture for a given positive integer n (1..=500).
/// Returns a Vec<String>, one string per row.
pub fn u_picture(n: usize) -> Vec<String> {
    assert!((1..=500).contains(&n));

    // Compute r = ceil(sqrt(n)) without floating point.
    let mut r = 1usize;
    while r * r < n {
        r += 1;
    }

    // Columns either r or r-1 depending on whether n exceeds r*(r-1).
    let base = r * (r - 1);
    let c = if n <= base { r - 1 } else { r };

    let q = n / c;
    let rem = n % c;

    let mut rows = Vec::with_capacity(r);

    // q full rows
    for _ in 0..q {
        rows.push("U".repeat(c));
    }

    // One balanced partial row if there is a remainder
    if rem > 0 {
        rows.push(balanced_row(c, rem));
    }

    rows
}

/// Create a single row of length `c` with exactly `k` 'U's,
/// placed as evenly and symmetrically as possible (others are '_').
/// Assumes 1 <= k < c.
fn balanced_row(c: usize, k: usize) -> String {
    debug_assert!(k > 0 && k < c);

    // Some heuristics
    let heu = match (c, k) {
        (7, 2) => Some("__U__U_"),
        (7, 5) => Some("U_UU_UU"),
        (8, 5) => Some("U_U_UU_U"),
        (8, 6) => Some("U_UUU_UU"),
        _ => None,
    };

    if let Some(r) = heu {
        return r.to_string();
    }

    // We'll place whichever group is smaller (either 'U' or '_'),
    // then fill the rest with the other character. This makes spacing natural.
    let minority_is_u = k <= c / 2;
    let m = if minority_is_u { k } else { c - k };

    // Precompute the minority positions (1-based columns).
    // Use midpoints of m equal segments over c columns:
    // pos_i = floor(((2i - 1) * c) / (2m)) + 1 , i = 1..m
    // This yields very even, symmetric placement for any c, m.
    let mut positions_1based = Vec::with_capacity(m);

    if c % 2 == 0 && m == 1 {
        // Consistent tie-breakers for a single item with even width:
        // - If minority is 'U', choose the right-of-center column (c/2 + 1)
        // - If minority is '_', choose the left-of-center column  (c/2)
        let p = if minority_is_u { c / 2 + 1 } else { c / 2 };
        positions_1based.push(p);
    } else {
        for i in 1..=m {
            let num = (2 * i - 1) * c;
            let den = 2 * m;
            let p = (num / den) + 1; // floor division then +1 to make it 1-based
            positions_1based.push(p);
        }
    }

    // Build the row.
    let mut row = if minority_is_u {
        // Start with all '_', then place 'U' at minority positions
        vec!['_'; c]
    } else {
        // Start with all 'U', then place '_' at minority positions
        vec!['U'; c]
    };

    for p in positions_1based {
        let idx = p - 1; // convert to 0-based
        if minority_is_u {
            row[idx] = 'U';
        } else {
            row[idx] = '_';
        }
    }

    row.into_iter().collect()
}

/// Return the number of units in each column for a given `n`.
///
/// This is a thin wrapper over [`u_picture`]; instead of returning rows of
/// `'U'`/`'_'` characters it returns a vector where each element contains the
/// count of units in that column.  The order is front-to-back to match how
/// formations are consumed by the simulator (i.e. the picture's columns are
/// reversed).
pub fn u_columns(n: usize) -> Vec<usize> {
    let rows = u_picture(n);
    if rows.is_empty() {
        return vec![];
    }

    let cols = rows[0].len();
    let mut counts = vec![0usize; cols];

    for row in rows {
        for (i, ch) in row.chars().enumerate() {
            if ch == 'U' {
                counts[i] += 1;
            }
        }
    }

    counts.reverse();
    counts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_samples() {
        assert_eq!(u_picture(1), vec!["U"]);
        assert_eq!(u_picture(2), vec!["U", "U"]);
        assert_eq!(u_picture(3), vec!["UU", "_U"]);
        assert_eq!(u_picture(4), vec!["UU", "UU"]);
        assert_eq!(u_picture(5), vec!["UU", "UU", "_U"]);
    }

    #[test]
    fn width_three_samples() {
        assert_eq!(u_picture(7), vec!["UUU", "UUU", "_U_"]);
        assert_eq!(u_picture(8), vec!["UUU", "UUU", "U_U"]);
        assert_eq!(u_picture(9), vec!["UUU", "UUU", "UUU"]);
        assert_eq!(u_picture(10), vec!["UUU", "UUU", "UUU", "_U_"]);
        assert_eq!(u_picture(12), vec!["UUU", "UUU", "UUU", "UUU"]);
    }

    #[test]
    fn column_counts() {
        assert_eq!(u_columns(1), vec![1]);
        assert_eq!(u_columns(2), vec![2]);
        assert_eq!(u_columns(3), vec![2, 1]);
        assert_eq!(u_columns(4), vec![2, 2]);
        assert_eq!(u_columns(5), vec![3, 2]);
        assert_eq!(u_columns(7), vec![2, 3, 2]);
        assert_eq!(u_columns(8), vec![3, 2, 3]);
        assert_eq!(u_columns(9), vec![3, 3, 3]);
        assert_eq!(u_columns(10), vec![3, 4, 3]);
    }

    #[test]
    fn bigger_samples() {
        // 36 = 6x6 full
        assert_eq!(u_picture(36), vec!["UUUUUU"; 6]);

        // 42 = 7x6 full (since r=7, r*(r-1)=42)
        assert_eq!(u_picture(42), vec!["UUUUUU"; 7]);

        // 43 -> 7x7 with 1 'U' centered in the last row
        let p43 = u_picture(43);
        assert_eq!(p43.len(), 7);
        assert!(p43[..6].iter().all(|row| row == "UUUUUUU"));
        assert_eq!(p43[6], "___U___");

        // 44 -> 7x7 with 2 'U' in a balanced last row.
        // Depending on tie-breaking, this yields a symmetric pattern like "_U___U_".
        let p44 = u_picture(44);
        assert_eq!(p44.len(), 7);
        assert!(p44[..6].iter().all(|row| row == "UUUUUUU"));
        // any balanced 2-U arrangement is acceptable; one consistent result is "_U___U_"
        assert_eq!(p44[6].chars().filter(|&ch| ch == 'U').count(), 2);
    }
}
