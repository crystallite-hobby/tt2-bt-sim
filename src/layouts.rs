use anyhow::{bail, Context, Result};
use std::collections::{BTreeMap, HashMap};
use std::fs;

use crate::lines_layout::u_picture;

#[derive(Debug, Clone)]
pub struct LayoutEntry {
    pub side: SideKind,
    pub counts: BTreeMap<String, usize>,
    pub grid: Vec<Vec<Option<String>>>,
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
pub fn parse_layouts(path: &str) -> Result<(HashMap<String, String>, Vec<LayoutEntry>)> {
    let text = fs::read_to_string(path).with_context(|| format!("reading {path}"))?;
    let mut abbr: HashMap<String, String> = HashMap::new();
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
        if let Some(rest) = line.strip_prefix('#') {
            // comment or abbreviation mapping
            let rest = rest.trim();
            if let Some((abbr_str, name)) = rest.split_once('-') {
                let a = abbr_str.trim();
                let n = name.trim();
                if a.len() == 2 {
                    abbr.insert(a.to_string(), n.to_string());
                }
            }
            i += 1;
            continue;
        }

        // Layout header e.g. "Left:He+3Sg+..."
        let (side_str, rest) = line
            .split_once(':')
            .ok_or_else(|| anyhow::anyhow!("Invalid layout header at line {}", i + 1))?;
        let side = SideKind::from_str(side_str)
            .ok_or_else(|| anyhow::anyhow!("Unknown side '{}' at line {}", side_str, i + 1))?;
        let mut counts: BTreeMap<String, usize> = BTreeMap::new();
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
            let name = abbr
                .get(ab)
                .ok_or_else(|| anyhow::anyhow!("Unknown unit abbr '{}' at line {}", ab, i + 1))?
                .clone();
            *counts.entry(name).or_default() += qty;
        }

        i += 1;
        let mut grid: Vec<Vec<Option<String>>> = vec![];
        let mut grid_counts: BTreeMap<String, usize> = BTreeMap::new();
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
            let mut cells: Vec<Option<String>> = vec![];
            for chunk in row.as_bytes().chunks(2) {
                let token = std::str::from_utf8(chunk).unwrap();
                if token == ".." {
                    cells.push(None);
                } else {
                    let name = abbr
                        .get(token)
                        .ok_or_else(|| {
                            anyhow::anyhow!("Unknown unit token '{}' at line {}", token, i + 1)
                        })?
                        .clone();
                    *grid_counts.entry(name.clone()).or_default() += 1;
                    cells.push(Some(name));
                }
            }
            if side == SideKind::Right {
                cells.reverse();
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

        entries.push(LayoutEntry {
            side,
            counts,
            grid,
            line_no: i + 1,
        });
    }

    Ok((abbr, entries))
}

pub fn find_layout_entry<'a>(
    layouts: &'a [LayoutEntry],
    side: SideKind,
    counts: &BTreeMap<String, usize>,
) -> Option<&'a LayoutEntry> {
    layouts
        .iter()
        .find(|e| e.side == side && e.counts == *counts)
}

/// Helper to compute roster counts with canonicalization based on mapping.
pub fn canonicalize_counts(
    counts: &[(String, usize)],
    abbr: &HashMap<String, String>,
) -> BTreeMap<String, usize> {
    let mut map: BTreeMap<String, usize> = BTreeMap::new();
    for (k, v) in counts {
        let mut found = false;
        for name in abbr.values() {
            if k.starts_with(name) {
                *map.entry(name.clone()).or_default() += *v;
                found = true;
                break;
            }
        }
        if !found {
            *map.entry(k.clone()).or_default() += *v;
        }
    }
    map
}

pub fn select_layout(
    layouts: &[LayoutEntry],
    side: SideKind,
    counts: &BTreeMap<String, usize>,
) -> Option<Vec<Vec<Option<String>>>> {
    if let Some(entry) = find_layout_entry(layouts, side, counts) {
        Some(entry.grid.clone())
    } else if counts.len() == 1 {
        let (unit, &num) = counts.iter().next().unwrap();
        Some(single_unit_layout(side, unit, num))
    } else {
        None
    }
}

fn single_unit_layout(side: SideKind, unit: &str, count: usize) -> Vec<Vec<Option<String>>> {
    let picture = u_picture(count);
    picture
        .into_iter()
        .map(|row| {
            let mut cells: Vec<Option<String>> = row
                .chars()
                .map(|ch| {
                    if ch == 'U' {
                        Some(unit.to_string())
                    } else {
                        None
                    }
                })
                .collect();
            if side == SideKind::Right {
                cells.reverse();
            }
            cells
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fallback_single_unit_layout_right() {
        let layouts: Vec<LayoutEntry> = vec![];
        let mut counts: BTreeMap<String, usize> = BTreeMap::new();
        counts.insert("Dragon".into(), 3);
        let grid = select_layout(&layouts, SideKind::Right, &counts).unwrap();
        let expected = vec![
            vec![Some("Dragon".into()), Some("Dragon".into())],
            vec![Some("Dragon".into()), None],
        ];
        assert_eq!(grid, expected);
    }
}
