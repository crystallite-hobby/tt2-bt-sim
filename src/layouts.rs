use anyhow::{bail, Context, Result};
use std::collections::BTreeMap;
use std::fs;

use crate::lines_layout::u_picture;
use crate::types::UnitKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LayoutLine {
    pub units: Vec<UnitKind>,
}

#[derive(Debug, Clone)]
pub struct LayoutEntry {
    pub side: SideKind,
    pub counts: BTreeMap<UnitKind, usize>,
    pub units: Vec<LayoutLine>,
    pub line_no: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SideKind {
    Left,
    Right,
}

impl SideKind {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "Left" => Some(SideKind::Left),
            "Right" => Some(SideKind::Right),
            _ => None,
        }
    }
}

/// Parse the layouts.dat file and return all layout entries.
pub fn parse_layouts(path: &str) -> Result<Vec<LayoutEntry>> {
    let text = fs::read_to_string(path).with_context(|| format!("reading {path}"))?;
    let mut entries: Vec<LayoutEntry> = vec![];

    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let raw = lines[i];
        let line = raw.trim();
        if line.is_empty() {
            i += 1;
            continue;
        }
        if line.starts_with('#') {
            i += 1;
            continue;
        }

        // Layout header e.g. "Left:He+3Sg+..."
        let (side_str, rest) = line
            .split_once(':')
            .ok_or_else(|| anyhow::anyhow!("Invalid layout header at line {}", i + 1))?;
        let side = SideKind::from_str(side_str)
            .ok_or_else(|| anyhow::anyhow!("Unknown side '{}' at line {}", side_str, i + 1))?;
        let mut counts: BTreeMap<UnitKind, usize> = BTreeMap::new();
        if rest.trim().is_empty() {
            bail!("Missing units specification at line {}", i + 1);
        }
        for part in rest.trim().split('+') {
            if part.is_empty() {
                continue;
            }
            let (num, ab) = part.split_at(part.len().saturating_sub(2));
            let qty: usize = if num.is_empty() {
                1
            } else {
                num.parse().with_context(|| format!("line {}", i + 1))?
            };
            let kind = UnitKind::from_abbr(ab)
                .ok_or_else(|| anyhow::anyhow!("Unknown unit abbr '{}' at line {}", ab, i + 1))?;
            *counts.entry(kind).or_default() += qty;
        }

        i += 1;
        let mut grid: Vec<Vec<Option<UnitKind>>> = vec![];
        let mut grid_counts: BTreeMap<UnitKind, usize> = BTreeMap::new();
        while i < lines.len() {
            let row_raw = lines[i];
            let row = row_raw.trim();
            if row.is_empty() {
                break;
            }
            if row.starts_with('#') {
                break;
            }
            if row.len() % 2 != 0 {
                bail!("Row length must be even at line {}", i + 1);
            }
            let mut cells: Vec<Option<UnitKind>> = vec![];
            for chunk in row.as_bytes().chunks(2) {
                let token = std::str::from_utf8(chunk).unwrap();
                if token == ".." {
                    cells.push(None);
                } else {
                    let kind = UnitKind::from_abbr(token).ok_or_else(|| {
                        anyhow::anyhow!("Unknown unit token '{}' at line {}", token, i + 1)
                    })?;
                    *grid_counts.entry(kind).or_default() += 1;
                    cells.push(Some(kind));
                }
            }
            grid.push(cells);
            i += 1;
        }

        // verify counts
        if counts != grid_counts {
            bail!(
                "Layout header counts mismatch grid at line {}",
                // i is currently at first blank/comment after grid; header line is remembered
                i + 1
            );
        }

        // verify shape using u_picture
        let total_units: usize = counts.values().sum();
        let expected = u_picture(total_units);
        let actual_shape: Vec<String> = grid
            .iter()
            .map(|row| {
                row.iter()
                    .map(|c| if c.is_some() { 'U' } else { '_' })
                    .collect()
            })
            .collect();
        if expected != actual_shape {
            bail!(
                "Layout shape mismatch with u_picture for layout starting at line {}",
                i + 1
            );
        }

        // transform rows into column-oriented layout lines
        let cols = grid.iter().map(|r| r.len()).max().unwrap_or(0);
        let mut lines_vec: Vec<LayoutLine> = Vec::with_capacity(cols);
        for c in 0..cols {
            let mut col_units: Vec<UnitKind> = Vec::new();
            for r in &grid {
                if let Some(Some(kind)) = r.get(c) {
                    col_units.push(*kind);
                }
            }
            lines_vec.push(LayoutLine { units: col_units });
        }
        if side == SideKind::Left {
            lines_vec.reverse();
        }

        entries.push(LayoutEntry {
            side,
            counts,
            units: lines_vec,
            line_no: i + 1,
        });
    }

    Ok(entries)
}

pub fn layout_lines_to_text_rows(s: SideKind, lines: &Vec<LayoutLine>) -> Vec<String> {
    let max_slots = lines.iter().map(|line| line.units.len()).max().unwrap_or(0);
    let mut rows: Vec<String> = Vec::new();
    for si in 0..max_slots {
        let lines_iter: Box<dyn Iterator<Item = &LayoutLine>> = match s {
            SideKind::Left => Box::new(lines.iter().rev()),
            SideKind::Right => Box::new(lines.iter()),
        };
        let mut row = String::new();
        for line in lines_iter {
            if let Some(kind) = line.units.get(si) {
                row.push_str(kind.abbr());
            } else {
                row.push_str("  ");
            }
        }
        rows.push(row);
    }
    rows
}

pub fn find_layout_entry<'a>(
    layouts: &'a [LayoutEntry],
    side: SideKind,
    counts: &BTreeMap<UnitKind, usize>,
) -> Option<&'a LayoutEntry> {
    layouts
        .iter()
        .find(|e| e.side == side && e.counts == *counts)
}

/// Aggregate roster counts.
pub fn canonicalize_counts(counts: &[(UnitKind, usize)]) -> BTreeMap<UnitKind, usize> {
    let mut map: BTreeMap<UnitKind, usize> = BTreeMap::new();
    for (k, v) in counts {
        *map.entry(k.canonical()).or_default() += *v;
    }
    map
}

pub fn select_layout(
    layouts: &[LayoutEntry],
    side: SideKind,
    counts: &BTreeMap<UnitKind, usize>,
) -> Option<Vec<LayoutLine>> {
    if let Some(entry) = find_layout_entry(layouts, side, counts) {
        Some(entry.units.clone())
    } else if counts.len() == 1 {
        let (unit, &num) = counts.iter().next().unwrap();
        Some(single_unit_layout(side, *unit, num))
    } else {
        None
    }
}

fn single_unit_layout(side: SideKind, unit: UnitKind, count: usize) -> Vec<LayoutLine> {
    let picture = u_picture(count);
    if picture.is_empty() {
        return vec![];
    }
    let rows = picture.len();
    let cols = picture[0].len();
    let mut lines: Vec<LayoutLine> = Vec::with_capacity(cols);
    for c in 0..cols {
        let mut col_units: Vec<UnitKind> = Vec::new();
        for r in 0..rows {
            if picture[r].as_bytes()[c] == b'U' {
                col_units.push(unit);
            }
        }
        lines.push(LayoutLine { units: col_units });
    }
    if side == SideKind::Right {
        lines.reverse();
    }
    lines
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fallback_single_unit_layout_right() {
        let layouts: Vec<LayoutEntry> = vec![];
        let mut counts: BTreeMap<UnitKind, usize> = BTreeMap::new();
        counts.insert(UnitKind::BroilerDragon, 3);
        let grid = select_layout(&layouts, SideKind::Right, &counts).unwrap();
        let expected = vec![
            LayoutLine {
                units: vec![UnitKind::BroilerDragon],
            },
            LayoutLine {
                units: vec![UnitKind::BroilerDragon, UnitKind::BroilerDragon],
            },
        ];
        assert_eq!(grid, expected);
    }
}
