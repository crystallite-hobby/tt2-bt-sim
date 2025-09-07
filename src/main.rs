//! TT2 battle report simulator prototype.

use anyhow::{Context, Result};
use clap::Parser;
use itertools::Itertools;
use serde::Deserialize;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;

#[derive(Parser, Debug)]
#[command(
    name = "tt2-sim",
    version,
    about = "TT2 battle report simulator (prototype)"
)]
struct Cli {
    /// Path to battle JSON spec
    file: String,
}

#[derive(Debug, Clone, Deserialize, Default)]
struct SideModifiers {
    #[serde(default = "one")]
    attack_mult: f64,
    #[serde(default = "one")]
    heal_mult: f64,
    #[serde(default = "one")]
    health_mult: f64,
    #[serde(default = "one")]
    armor_mult: f64,
}
fn one() -> f64 {
    1.0
}

#[derive(Debug, Clone, Deserialize, Default)]
struct Building {
    name: String,
    #[serde(default)]
    health_bonus: f64,
    #[serde(default)]
    applies_to: String, // "attacker" | "defender" | "both"
}

#[derive(Debug, Clone, Deserialize, Default)]
struct CountedUnit {
    kind: String,
    count: usize,
    #[serde(default)]
    traits: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Default)]
struct HeroDef {
    name: String,
    attack: f64,
    range: usize,
    defense: f64,
    health: f64,
    #[serde(default)]
    regen: f64,
    #[serde(default = "default_hero_policy")]
    prioritize: String, // "lowest_health" | "closest_then_reading_order"
}
fn default_hero_policy() -> String {
    "lowest_health".to_string()
}

#[derive(Debug, Clone, Deserialize, Default)]
struct ArmyConfig {
    roster: Vec<CountedUnit>,
    #[serde(default)]
    hero: Option<HeroDef>,
    #[serde(default)]
    side_mods: SideModifiers,
    #[serde(default)]
    layout: LayoutConfig,
}

#[derive(Debug, Clone, Deserialize, Default)]
struct LayoutConfig {
    #[serde(default)]
    columns_hint: Option<usize>,
}

#[derive(Debug, Clone, Deserialize, Default)]
struct RulesConfig {
    #[serde(default = "default_max_rounds")]
    max_rounds: usize,
    #[serde(default = "default_true")]
    defender_acts_first: bool,
    #[serde(default)]
    healer_spillover: bool,
    #[serde(default = "default_targeting")]
    targeting: String,
}
fn default_true() -> bool {
    true
}
fn default_max_rounds() -> usize {
    50
}
fn default_targeting() -> String {
    "closest_then_reading_order".to_string()
}

#[derive(Debug, Clone, Deserialize, Default)]
struct BattleConfig {
    units: HashMap<String, UnitDef>,
    attacker: ArmyConfig,
    defender: ArmyConfig,
    #[serde(default)]
    buildings: Vec<Building>,
    #[serde(default)]
    rules: RulesConfig,
}

#[derive(Debug, Clone, Deserialize, Default)]
struct UnitDef {
    name: Option<String>,
    class: String, // "land" | "naval" | "aerial"
    #[serde(default)]
    attack: Option<f64>,
    #[serde(default)]
    range: usize,
    #[serde(default)]
    heal: Option<f64>,
    #[serde(default)]
    heal_range: Option<usize>,
    health: f64,
    armor: f64,
    #[serde(default)]
    size: u8,
    #[serde(default)]
    can_target: Vec<String>,
    #[serde(default)]
    bonus_vs: Vec<BonusRule>,
    #[serde(default)]
    traits: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Default)]
struct BonusRule {
    when: Vec<String>, // require all traits present to apply
    add: f64,          // flat bonus added to attack
}

#[derive(Debug, Clone)]
struct BattleState {
    cfg: BattleConfig,
    round: usize,
    // current formations (repacked each round):
    att: Formation,
    def_: Formation,
    def_building_hp_bonus: f64,
}

#[derive(Debug, Clone)]
struct Formation {
    /// All alive entities in reading order (Line asc, Slot asc). Repacked per round.
    grid: Vec<Vec<EntityId>>, // lines -> slots -> entity id
    entities: BTreeMap<EntityId, Entity>,
    next_local_id: usize,
    side: Side,
    side_mods: SideModifiers,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct EntityId(u64);

#[derive(Debug, Clone)]
struct Entity {
    id: EntityId,
    side: Side,
    line: usize,  // 1-based within current layout
    slot: usize,  // 1-based within current layout
    kind: String, // e.g. "Horseman" | "SisterOfMercy" | "Hero"
    class: String,
    traits: Vec<String>,
    attack: Option<f64>,
    range: usize,
    heal: Option<f64>,
    heal_range: Option<usize>,
    defense: f64, // a.k.a armor after modifiers
    base_health: f64,
    bonus_health: f64,
    cur_health: f64,
    can_target: Vec<String>,
    is_hero: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Side {
    Attacker,
    Defender,
}

impl Side {
    fn other(self) -> Side {
        match self {
            Side::Attacker => Side::Defender,
            Side::Defender => Side::Attacker,
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let text =
        fs::read_to_string(&cli.file).with_context(|| format!("Failed to read {}", cli.file))?;
    let mut cfg: BattleConfig =
        serde_json::from_str(&text).with_context(|| "Invalid JSON battle config")?;

    normalize_defs(&mut cfg);

    let def_building_hp_bonus = cfg
        .buildings
        .iter()
        .filter(|b| b.health_bonus > 0.0 && (b.applies_to == "defender" || b.applies_to == "both"))
        .map(|b| b.health_bonus)
        .sum::<f64>();

    let mut state = BattleState {
        round: 0,
        att: Formation::new(Side::Attacker, cfg.attacker.side_mods.clone()),
        def_: Formation::new(Side::Defender, cfg.defender.side_mods.clone()),
        cfg,
        def_building_hp_bonus,
    };

    // Seed entities
    seed_side(&mut state, Side::Attacker)?;
    seed_side(&mut state, Side::Defender)?;

    // Initial layout
    repack_layouts(&mut state);

    // Print header
    print_header(&state);

    // Run rounds
    let max_rounds = state.cfg.rules.max_rounds;
    loop {
        state.round += 1;
        if state.round > max_rounds {
            break;
        }

        println!("\n## Round {}\n", state.round);
        print_layouts(&state);

        let mut events: Vec<String> = vec![];
        let mut eliminations: Vec<String> = vec![];

        // Round start regen (hero)
        for eid in state.att.entities.keys().cloned().collect::<Vec<_>>() {
            if let Some(e) = state.att.entities.get_mut(&eid) {
                if e.is_hero {
                    let regen = state
                        .cfg
                        .attacker
                        .hero
                        .as_ref()
                        .map(|h| h.regen)
                        .unwrap_or(0.0);
                    if regen > 0.0 && e.cur_health > 0.0 {
                        e.cur_health = (e.cur_health + regen).min(e.base_health + e.bonus_health);
                        events.push(format!(
                            "1. Hero #A{}.{} regenerates. Health +{:.2}. Now {:.2}",
                            e.line, e.slot, regen, e.cur_health
                        ));
                    }
                }
            }
        }

        // Acting order: defenders then attackers (optional rule)
        let order_sides: Vec<Side> = if state.cfg.rules.defender_acts_first {
            vec![Side::Defender, Side::Attacker]
        } else {
            vec![Side::Attacker, Side::Defender]
        };

        let mut action_index = if events.is_empty() { 1 } else { 2 }; // continue numbering

        for side in order_sides {
            let (acting_form, _other_form) = match side {
                Side::Attacker => (&state.att, &state.def_),
                Side::Defender => (&state.def_, &state.att),
            };
            // Reading order snapshot (entities may die during iteration)
            let mut roster: Vec<EntityId> = acting_form.grid.iter().flatten().cloned().collect();

            for eid in roster.drain(..) {
                if !is_alive(&state, eid) {
                    continue;
                }
                let is_hero = { get_entity(&state, eid).is_hero };

                // Healer?
                if get_entity(&state, eid).heal.unwrap_or(0.0) > 0.0 {
                    let heal_done = healer_act(&mut state, eid, &mut events, &mut action_index);
                    if heal_done {
                        continue;
                    }
                }

                // Attacker?
                if get_entity(&state, eid).attack.unwrap_or(0.0) > 0.0 {
                    let policy = if is_hero {
                        state
                            .cfg
                            .attacker
                            .hero
                            .as_ref()
                            .map(|h| h.prioritize.clone())
                            .unwrap_or(default_hero_policy())
                    } else {
                        state.cfg.rules.targeting.clone()
                    };

                    let _acted =
                        attacker_act(&mut state, eid, &mut events, &mut action_index, &policy);
                }
            }
        }

        // Print events for the round
        if !events.is_empty() {
            println!("Events:");
            for e in &events {
                println!("{}", e);
            }
        }

        // Remove dead + note eliminations
        collect_eliminations(&mut state, &mut eliminations);

        // End-of-round summary lines
        for note in eliminations {
            println!("{}", note);
        }

        // Victory check
        let att_alive = state.att.entities.values().any(|e| e.cur_health > 0.0);
        let def_alive = state.def_.entities.values().any(|e| e.cur_health > 0.0);
        if !att_alive || !def_alive {
            println!(
                "\nReport finished: {} side {}.\n",
                if att_alive { "Defending" } else { "Attacking" },
                if att_alive { "defeated" } else { "defeated" }
            );
            break;
        }

        // Repack for next round (lines/slots may change)
        repack_layouts(&mut state);
    }

    Ok(())
}

// --------------------------- Core mechanics ---------------------------

fn normalize_defs(cfg: &mut BattleConfig) {
    for (k, v) in cfg.units.clone() {
        // ensure name exists
        let mut u = v;
        if u.name.is_none() {
            u.name = Some(k.clone());
        }
        cfg.units.insert(k, u);
    }
}

fn seed_side(state: &mut BattleState, side: Side) -> Result<()> {
    let acfg = match side {
        Side::Attacker => &state.cfg.attacker,
        Side::Defender => &state.cfg.defender,
    };

    let formation = match side {
        Side::Attacker => &mut state.att,
        Side::Defender => &mut state.def_,
    };

    // Roster units
    for cu in &acfg.roster {
        let def = state
            .cfg
            .units
            .get(&cu.kind)
            .with_context(|| format!("Unknown unit kind '{}'", cu.kind))?
            .clone();
        for _ in 0..cu.count {
            let eid = formation.alloc_id();
            let (base_hp, bonus_hp) = apply_health_mods(
                def.health,
                &acfg.side_mods,
                side,
                state.def_building_hp_bonus,
            );
            let def_val = def.armor * acfg.side_mods.armor_mult;
            let ent = Entity {
                id: eid,
                side,
                line: 0,
                slot: 0,
                kind: def.name.clone().unwrap_or(cu.kind.clone()),
                class: def.class.clone(),
                traits: cu.traits.clone(),
                attack: def.attack.map(|a| a * acfg.side_mods.attack_mult),
                range: def.range,
                heal: def.heal.map(|h| h * acfg.side_mods.heal_mult),
                heal_range: def.heal_range,
                defense: def_val,
                base_health: base_hp,
                bonus_health: bonus_hp,
                cur_health: base_hp + bonus_hp,
                can_target: if def.can_target.is_empty() {
                    vec!["land".into(), "naval".into(), "aerial".into()]
                } else {
                    def.can_target.clone()
                },
                is_hero: false,
            };
            formation.entities.insert(eid, ent);
        }
    }

    // Hero (attacker only by default)
    if side == Side::Attacker {
        if let Some(h) = &acfg.hero {
            let eid = formation.alloc_id();
            let (base_hp, bonus_hp) = (h.health, 0.0);
            let ent = Entity {
                id: eid,
                side,
                line: 0,
                slot: 0,
                kind: h.name.clone(),
                class: "land".into(),
                traits: vec!["hero".into()],
                attack: Some(h.attack),
                range: h.range,
                heal: None,
                heal_range: None,
                defense: h.defense,
                base_health: base_hp,
                bonus_health: bonus_hp,
                cur_health: base_hp + bonus_hp,
                can_target: vec!["land".into(), "naval".into(), "aerial".into()],
                is_hero: true,
            };
            formation.entities.insert(eid, ent);
        }
    }

    Ok(())
}

fn apply_health_mods(base: f64, mods: &SideModifiers, side: Side, def_bonus: f64) -> (f64, f64) {
    let bonus_from_mult = (base * (mods.health_mult - 1.0)).max(0.0);
    let building = match side {
        Side::Defender => def_bonus,
        Side::Attacker => 0.0,
    };
    (base, bonus_from_mult + building)
}

fn repack_layouts(state: &mut BattleState) {
    state.att.repack(&state.cfg, state.round, true);
    state.def_.repack(&state.cfg, state.round, false);
}

impl Formation {
    fn new(side: Side, side_mods: SideModifiers) -> Self {
        Self {
            grid: vec![],
            entities: BTreeMap::new(),
            next_local_id: 1,
            side,
            side_mods,
        }
    }

    fn alloc_id(&mut self) -> EntityId {
        let id = self.next_local_id as u64;
        self.next_local_id += 1;
        EntityId(((self.side as u64) << 56) | id)
    }
    fn side(&self) -> Side {
        self.side
    }

    fn repack(&mut self, cfg: &BattleConfig, _round: usize, is_attacker: bool) {
        // Collect alive entities
        let mut alive: Vec<Entity> = self
            .entities
            .values()
            .cloned()
            .filter(|e| e.cur_health > 0.0)
            .collect();
        // Determine columns
        let n = alive.len().max(1);
        let columns = cfg_column_hint(cfg, is_attacker)
            .unwrap_or_else(|| (f64::ceil((n as f64).sqrt()) as usize).max(3));
        // Lines
        let lines = (n + columns - 1) / columns;
        let mut grid: Vec<Vec<Entity>> = vec![vec![]; lines];

        // Heuristic placement order:
        // 1) Reserve hero at line 1, slot min(5, columns) if exists
        // 2) Sisters: from last line down to 2, one per line; repeat until exhausted
        // 3) Sages: from last line backward, spread
        // 4) Crossbows: ensure at least one in line 2, rest from back lines
        // 5) Fill remaining with others (Horsemen, etc.) from front line forward

        // Buckets by kind
        let mut hero: Vec<Entity> = vec![];
        let mut sisters: Vec<Entity> = vec![];
        let mut sages: Vec<Entity> = vec![];
        let mut xbows: Vec<Entity> = vec![];
        let mut others: Vec<Entity> = vec![];
        for e in alive.into_iter() {
            if e.is_hero {
                hero.push(e);
            } else if e.kind.contains("Sister") {
                sisters.push(e);
            } else if e.kind.contains("Sage") {
                sages.push(e);
            } else if e.kind.contains("Crossbow") {
                xbows.push(e);
            } else {
                others.push(e);
            }
        }

        // helper to place entity at (line, slot) if free
        let mut place_at =
            |grid: &mut Vec<Vec<Entity>>, line_idx: usize, max_cols: usize, ent: Entity| -> bool {
                if grid[line_idx].len() < max_cols {
                    grid[line_idx].push(ent);
                    true
                } else {
                    false
                }
            };

        // 1) Hero
        if let Some(mut h) = hero.pop() {
            let line_idx = 0usize; // front line
            let hero_slot_pref = (5usize).min(columns).saturating_sub(1); // zero-based push order: we just push and will sort later
                                                                          // We'll temporarily mark desired slot as position by inserting placeholder empties
            while grid[line_idx].len() < hero_slot_pref {
                grid[line_idx].push(dummy());
            }
            grid[line_idx].push(h);
        }

        // 2) Sisters: back -> line 2, one per line pass until exhausted
        let mut iter_sis = sisters.into_iter();
        'outer_sis: loop {
            let mut any = false;
            for li in (1..lines).rev() {
                // last..=1 (line #2 is index 1)
                if let Some(mut s) = iter_sis.next() {
                    if grid[li].len() < columns {
                        grid[li].push(s);
                        any = true;
                    } else {
                        continue;
                    }
                } else {
                    break 'outer_sis;
                }
            }
            if !any {
                break;
            }
        }

        // 3) Sages: prefer last lines
        for mut sg in sages.into_iter() {
            for li in (0..lines).rev() {
                if place_at(&mut grid, li, columns, sg.clone()) {
                    break;
                }
            }
        }

        // 4) Crossbows: ensure at least one in line 2
        let mut xbows_left = xbows;
        if !xbows_left.is_empty() && lines >= 2 {
            let mut xb = xbows_left.remove(0);
            let _ = place_at(&mut grid, 1, columns, xb);
        }
        for mut xb in xbows_left.into_iter() {
            for li in (0..lines).rev() {
                if place_at(&mut grid, li, columns, xb.clone()) {
                    break;
                }
            }
        }

        // 5) Others fill front to back
        for mut o in others.into_iter() {
            for li in 0..lines {
                if place_at(&mut grid, li, columns, o.clone()) {
                    break;
                }
            }
        }

        // Clean dummies and compact per line to max columns
        for li in 0..lines {
            grid[li].retain(|e| e.base_health >= 0.0);
            // Ensure we don't exceed columns
            if grid[li].len() > columns {
                grid[li].truncate(columns);
            }
        }

        // Stamp new positions back to entities map + build final grid of IDs
        let mut new_grid: Vec<Vec<EntityId>> = vec![vec![]; lines];
        for (li, row) in grid.into_iter().enumerate() {
            // assign slots in the order present (reading order)
            for (si, mut e) in row.into_iter().enumerate() {
                if let Some(mut origin) = self.entities.remove(&e.id) {
                    origin.line = li + 1;
                    origin.slot = si + 1;
                    let eid = origin.id;
                    new_grid[li].push(eid);
                    self.entities.insert(eid, origin);
                }
            }
        }
        self.grid = new_grid;
    }
}

fn dummy() -> Entity {
    Entity {
        id: EntityId(0),
        side: Side::Attacker,
        line: 0,
        slot: 0,
        kind: "_dummy".into(),
        class: "land".into(),
        traits: vec![],
        attack: None,
        range: 0,
        heal: None,
        heal_range: None,
        defense: 0.0,
        base_health: -1.0,
        bonus_health: 0.0,
        cur_health: 0.0,
        can_target: vec![],
        is_hero: false,
    }
}

fn print_header(state: &BattleState) {
    let att_units: HashMap<&str, usize> = aggregate_kinds(&state.att);
    let def_units: HashMap<&str, usize> = aggregate_kinds(&state.def_);

    println!("# Attack report (simulated)\n");
    println!("## Armies\n");
    println!("Attacking army:");
    for (k, v) in att_units.iter().sorted_by_key(|(k, _)| *k) {
        println!("{}x{}", v, k);
    }
    println!("-- total {} units\n", state.att.entities.len());
    println!("Defending army:");
    for (k, v) in def_units.iter().sorted_by_key(|(k, _)| *k) {
        println!("{}x{}", v, k);
    }
    println!("-- total {} units\n", state.def_.entities.len());

    if let Some(h) = &state.cfg.attacker.hero {
        println!("## Hero\n- Health {:.0}\n- Attack {:.2} (range {})\n- Defense {:.2}\n- Regen +{:.2} per round\n- Perks → army mods: attack x{:.2}, heal x{:.2}, health x{:.2}, armor x{:.2}\n",
            h.health, h.attack, h.range, h.defense, h.regen,
            state.att.side_mods.attack_mult, state.att.side_mods.heal_mult,
            state.att.side_mods.health_mult, state.att.side_mods.armor_mult);
    }

    if !state.cfg.buildings.is_empty() {
        println!("## Buildings\n");
        for b in &state.cfg.buildings {
            println!(
                "- {} (applies_to: {}, +{:.0} HP)",
                b.name, b.applies_to, b.health_bonus
            );
        }
        println!("");
    }
}

fn aggregate_kinds(form: &Formation) -> HashMap<&str, usize> {
    let mut m: HashMap<&str, usize> = HashMap::new();
    for e in form.entities.values() {
        if e.cur_health > 0.0 {
            *m.entry(&e.kind).or_default() += 1;
        }
    }
    m
}

fn cfg_column_hint(cfg: &BattleConfig, attacker: bool) -> Option<usize> {
    if attacker {
        cfg.attacker.layout.columns_hint
    } else {
        cfg.defender.layout.columns_hint
    }
}

fn print_layouts(state: &BattleState) {
    // Attacker
    println!("Attacker army layout:");
    for (li, row) in state.att.grid.iter().enumerate() {
        println!("- Line #{}:", li + 1);
        for (si, eid) in row.iter().enumerate() {
            let e = get_entity(state, *eid);
            let hp_str = if e.bonus_health > 0.0 {
                format!("({:.0}+{:.2})", e.base_health, e.bonus_health)
            } else {
                format!("{:.2}", e.cur_health)
            };
            println!(
                "    - Unit #A{}.{}: {} {} health",
                li + 1,
                si + 1,
                e.kind,
                hp_str
            );
        }
    }
    println!("");

    // Defender
    println!("Defending army layout:");
    for (li, row) in state.def_.grid.iter().enumerate() {
        println!("- Line #{}:", li + 1);
        for (si, eid) in row.iter().enumerate() {
            let e = get_entity(state, *eid);
            let hp_str = if e.bonus_health > 0.0 {
                format!("({:.0}+{:.0})", e.base_health, e.bonus_health)
            } else {
                format!("{:.2}", e.cur_health)
            };
            println!(
                "    - Unit #D{}.{}: {} {} health",
                li + 1,
                si + 1,
                e.kind,
                hp_str
            );
        }
    }
    println!("");
}

fn is_alive(state: &BattleState, eid: EntityId) -> bool {
    get_entity(state, eid).cur_health > 0.0
}
fn get_entity<'a>(state: &'a BattleState, eid: EntityId) -> &'a Entity {
    state
        .att
        .entities
        .get(&eid)
        .or_else(|| state.def_.entities.get(&eid))
        .expect("entity")
}
fn get_entity_mut<'a>(state: &'a mut BattleState, eid: EntityId) -> &'a mut Entity {
    if state.att.entities.contains_key(&eid) {
        state.att.entities.get_mut(&eid).unwrap()
    } else {
        state.def_.entities.get_mut(&eid).unwrap()
    }
}

fn healer_act(
    state: &mut BattleState,
    healer_id: EntityId,
    events: &mut Vec<String>,
    idx: &mut usize,
) -> bool {
    let (side, heal_amt, range, line) = {
        let h = get_entity(state, healer_id);
        (
            h.side,
            h.heal.unwrap_or(0.0),
            h.heal_range.unwrap_or(0),
            h.line,
        )
    };
    if heal_amt <= 0.0 {
        return false;
    }

    // Wounded allies within |Δline| <= heal range
    let allies = match side {
        Side::Attacker => &state.att,
        Side::Defender => &state.def_,
    };
    let mut wounded: Vec<EntityId> = vec![];
    for (eli, row) in allies.grid.iter().enumerate() {
        let dist = li_dist_same(line, eli + 1);
        if dist <= range {
            for &eid in row {
                let e = get_entity(state, eid);
                if e.cur_health > 0.0 && e.cur_health < e.base_health + e.bonus_health {
                    wounded.push(eid);
                }
            }
        }
    }
    if wounded.is_empty() {
        return false;
    }

    // Priority: lowest absolute HP, then reading order
    wounded.sort_by(|&a, &b| {
        let ea = get_entity(state, a);
        let eb = get_entity(state, b);
        ea.cur_health
            .partial_cmp(&eb.cur_health)
            .unwrap_or(Ordering::Equal)
            .then_with(|| (ea.line, ea.slot).cmp(&(eb.line, eb.slot)))
    });

    let mut remaining = heal_amt;
    for &wid in &wounded {
        if remaining <= 0.0 {
            break;
        }
        let e0 = get_entity(state, wid).clone();
        let need = (e0.base_health + e0.bonus_health - e0.cur_health).max(0.0);
        if need <= 0.0 {
            continue;
        }
        let apply = remaining.min(need);

        let healer_info = {
            let h = get_entity(state, healer_id);
            (h.kind.clone(), h.side, h.line, h.slot)
        };

        let (target_kind, target_side, target_line, target_slot, new_health) = {
            let e = get_entity_mut(state, wid);
            e.cur_health += apply;
            (e.kind.clone(), e.side, e.line, e.slot, e.cur_health)
        };

        events.push(format!(
            "{}. {} #{}{}.{} heals {} #{}{}.{} ({:.2}) => +{:.2}. Now {:.2}",
            *idx,
            healer_info.0,
            side_char(healer_info.1),
            healer_info.2,
            healer_info.3,
            target_kind,
            side_char(target_side),
            target_line,
            target_slot,
            heal_amt,
            apply,
            new_health
        ));
        *idx += 1;
        remaining -= apply;
        if !state.cfg.rules.healer_spillover {
            break;
        }
    }
    true
}

fn attacker_act(
    state: &mut BattleState,
    attacker_id: EntityId,
    events: &mut Vec<String>,
    idx: &mut usize,
    policy: &str,
) -> bool {
    // Snapshot attacker properties first
    let (side, line, range, atk_opt, kind, traits, can_target) = {
        let a = get_entity(state, attacker_id);
        (
            a.side,
            a.line,
            a.range,
            a.attack,
            a.kind.clone(),
            a.traits.clone(),
            a.can_target.clone(),
        )
    };
    let atk = if let Some(v) = atk_opt {
        v
    } else {
        return false;
    };

    // Collect enemy candidates in range
    let enemies = match side {
        Side::Attacker => &state.def_,
        Side::Defender => &state.att,
    };
    let mut cands: Vec<(usize, EntityId)> = vec![]; // (line_distance, eid)
    for (eli, row) in enemies.grid.iter().enumerate() {
        let dist = li_dist_cross(line, eli + 1);
        if dist <= range {
            for &eid in row {
                let e = get_entity(state, eid);
                if e.cur_health > 0.0 && can_target.iter().any(|cls| *cls == e.class) {
                    cands.push((dist, eid));
                }
            }
        }
    }
    if cands.is_empty() {
        return false;
    }

    // Choose target per policy
    let target_eid = match policy {
        "lowest_health" => {
            cands
                .into_iter()
                .min_by(|a, b| {
                    let ea = get_entity(state, a.1);
                    let eb = get_entity(state, b.1);
                    ea.cur_health
                        .partial_cmp(&eb.cur_health)
                        .unwrap_or(Ordering::Equal)
                        .then_with(|| a.0.cmp(&b.0))
                        .then_with(|| (ea.line, ea.slot).cmp(&(eb.line, eb.slot)))
                })
                .unwrap()
                .1
        }
        _ => {
            // closest_then_reading_order
            cands
                .into_iter()
                .min_by(|a, b| {
                    let ea = get_entity(state, a.1);
                    let eb = get_entity(state, b.1);
                    a.0.cmp(&b.0)
                        .then_with(|| (ea.line, ea.slot).cmp(&(eb.line, eb.slot)))
                })
                .unwrap()
                .1
        }
    };

    // Attack bonus vs traits from unit definition (flat add)
    let (target_traits, def_val, t_kind, t_side, t_line, t_slot) = {
        let t = get_entity(state, target_eid);
        (
            t.traits.clone(),
            t.defense,
            t.kind.clone(),
            t.side,
            t.line,
            t.slot,
        )
    };
    let atk_val = atk + attack_bonus_vs(&traits, &target_traits, &state.cfg.units, &kind);
    let dmg = -(atk_val * atk_val) / (atk_val + def_val);

    // Apply damage
    let new_health = {
        let t = get_entity_mut(state, target_eid);
        t.cur_health = (t.cur_health + dmg).max(0.0);
        t.cur_health
    };

    // Log event
    let (src_kind, src_side, src_line, src_slot) = {
        let src = get_entity(state, attacker_id);
        (src.kind.clone(), src.side, src.line, src.slot)
    };
    events.push(format!(
        "{}. {} #{}{}.{} attacks {} #{}{}.{}. Attack {:.2} vs Defense {:.2} => damage {:+.2}. {} now {:.2}",
        *idx,
        src_kind,
        side_char(src_side), src_line, src_slot,
        t_kind, side_char(t_side), t_line, t_slot,
        atk_val, def_val, dmg,
        t_kind, new_health
    ));
    *idx += 1;

    true
}

fn attack_bonus_vs(
    _attacker_traits: &[String],
    target_traits: &[String],
    units: &HashMap<String, UnitDef>,
    kind: &str,
) -> f64 {
    if let Some(def) = units.get(kind) {
        let mut add = 0.0;
        for br in &def.bonus_vs {
            if br.when.iter().all(|w| target_traits.iter().any(|t| t == w)) {
                add += br.add;
            }
        }
        add
    } else {
        0.0
    }
}

fn li_dist_cross(att_line: usize, def_line: usize) -> usize {
    att_line + def_line - 1
}
fn li_dist_same(l1: usize, l2: usize) -> usize {
    l1.max(l2) - l1.min(l2)
}

fn side_char(s: Side) -> char {
    match s {
        Side::Attacker => 'A',
        Side::Defender => 'D',
    }
}

fn collect_eliminations(state: &mut BattleState, notes: &mut Vec<String>) {
    // Scan both sides for 0 HP
    let mut gone: Vec<(Side, usize, usize, String)> = vec![];
    for e in state.att.entities.values() {
        if e.cur_health <= 0.0 {
            gone.push((Side::Attacker, e.line, e.slot, e.kind.clone()));
        }
    }
    for e in state.def_.entities.values() {
        if e.cur_health <= 0.0 {
            gone.push((Side::Defender, e.line, e.slot, e.kind.clone()));
        }
    }

    // Print once per entity (some may be removed multiple rounds if not repacked yet)
    let mut seen: HashSet<(Side, usize, usize)> = HashSet::new();
    for (s, li, si, kind) in gone {
        if seen.insert((s, li, si)) {
            notes.push(format!(
                "- #{}{}.{}: {} is eliminated!",
                side_char(s),
                li,
                si,
                kind
            ));
        }
    }

    // Remove dead
    state.att.entities.retain(|_, e| e.cur_health > 0.0);
    state.def_.entities.retain(|_, e| e.cur_health > 0.0);
}
