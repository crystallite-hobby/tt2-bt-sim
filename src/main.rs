//! TT2 battle report simulator prototype.

use anyhow::{anyhow, Context, Result};
use clap::Parser;
use itertools::Itertools;
use serde::Deserialize;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::fs;
use tt2_bt_sim::layouts::{canonicalize_counts, parse_layouts, select_layout, SideKind};
use tt2_bt_sim::types::{AppliesTo, BuildingName, TargetingPolicy, Trait, UnitClass, UnitKind};

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

#[derive(Debug, Clone, Deserialize)]
struct Building {
    name: BuildingName,
    #[serde(default)]
    health_bonus: f64,
    #[serde(default)]
    applies_to: AppliesTo,
}

#[derive(Debug, Clone, Deserialize)]
struct CountedUnit {
    kind: UnitKind,
    count: usize,
    #[serde(default)]
    traits: Vec<Trait>,
}

#[derive(Debug, Clone, Deserialize)]
struct HeroDef {
    name: UnitKind,
    attack: f64,
    range: usize,
    defense: f64,
    health: f64,
    #[serde(default)]
    regen: f64,
    #[serde(default = "default_hero_policy")]
    prioritize: TargetingPolicy,
}
fn default_hero_policy() -> TargetingPolicy {
    TargetingPolicy::LowestHealth
}

#[derive(Debug, Clone, Deserialize, Default)]
struct ArmyConfig {
    roster: Vec<CountedUnit>,
    #[serde(default)]
    hero: Option<HeroDef>,
    #[serde(default)]
    side_mods: SideModifiers,
}

#[derive(Debug, Clone, Deserialize)]
struct RulesConfig {
    #[serde(default = "default_max_rounds")]
    max_rounds: usize,
    #[serde(default = "default_true")]
    defender_acts_first: bool,
    #[serde(default)]
    healer_spillover: bool,
    #[serde(default = "default_targeting")]
    targeting: TargetingPolicy,
}
fn default_true() -> bool {
    true
}
fn default_max_rounds() -> usize {
    50
}
fn default_targeting() -> TargetingPolicy {
    TargetingPolicy::ClosestThenReadingOrder
}

impl Default for RulesConfig {
    fn default() -> Self {
        Self {
            max_rounds: default_max_rounds(),
            defender_acts_first: default_true(),
            healer_spillover: false,
            targeting: default_targeting(),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
struct BattleConfig {
    units: HashMap<UnitKind, UnitDef>,
    attacker: ArmyConfig,
    defender: ArmyConfig,
    #[serde(default)]
    buildings: Vec<Building>,
    #[serde(default)]
    rules: RulesConfig,
}

#[derive(Debug, Clone, Deserialize)]
struct UnitDef {
    class: UnitClass, // "land" | "naval" | "aerial"
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
    #[allow(dead_code)]
    size: u8,
    #[serde(default)]
    can_target: Vec<UnitClass>,
    #[serde(default)]
    bonus_vs: Vec<BonusRule>,
    #[serde(default)]
    #[allow(dead_code)]
    traits: Vec<Trait>,
}

#[derive(Debug, Clone, Deserialize, Default)]
struct BonusRule {
    when: Vec<Trait>, // require all traits present to apply
    add: f64,         // flat bonus added to attack
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
    template: Vec<Vec<Option<UnitKind>>>,
    canon_names: Vec<UnitKind>,
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
    line: usize,    // 1-based within current layout
    slot: usize,    // 1-based within current layout
    kind: UnitKind, // e.g. Horseman | SisterOfMercy | Hero
    class: UnitClass,
    traits: Vec<Trait>,
    attack: Option<f64>,
    range: usize,
    heal: Option<f64>,
    heal_range: Option<usize>,
    defense: f64, // a.k.a armor after modifiers
    base_health: f64,
    bonus_health: f64,
    cur_health: f64,
    can_target: Vec<UnitClass>,
    is_hero: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Side {
    Attacker,
    Defender,
}

impl Side {}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let text =
        fs::read_to_string(&cli.file).with_context(|| format!("Failed to read {}", cli.file))?;
    let cfg: BattleConfig =
        serde_json::from_str(&text).with_context(|| "Invalid JSON battle config")?;

    let def_building_hp_bonus = cfg
        .buildings
        .iter()
        .filter(|b| {
            b.health_bonus > 0.0 && matches!(b.applies_to, AppliesTo::Defender | AppliesTo::Both)
        })
        .map(|b| b.health_bonus)
        .sum::<f64>();

    let layouts = parse_layouts("layouts.dat")?;

    let mut att_counts_raw: Vec<(UnitKind, usize)> = cfg
        .attacker
        .roster
        .iter()
        .map(|cu| (cu.kind, cu.count))
        .collect();
    if let Some(h) = &cfg.attacker.hero {
        att_counts_raw.push((h.name, 1));
    }
    let att_counts = canonicalize_counts(&att_counts_raw);
    let att_template = select_layout(&layouts, SideKind::Left, &att_counts)
        .ok_or_else(|| anyhow!("No matching layout for attacker"))?;

    let def_counts_raw: Vec<(UnitKind, usize)> = cfg
        .defender
        .roster
        .iter()
        .map(|cu| (cu.kind, cu.count))
        .collect();
    let def_counts = canonicalize_counts(&def_counts_raw);
    let def_template = select_layout(&layouts, SideKind::Right, &def_counts)
        .ok_or_else(|| anyhow!("No matching layout for defender"))?;

    let mut state = BattleState {
        round: 0,
        att: Formation::new(Side::Attacker, cfg.attacker.side_mods.clone(), att_template),
        def_: Formation::new(Side::Defender, cfg.defender.side_mods.clone(), def_template),
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
                            .map(|h| h.prioritize)
                            .unwrap_or(default_hero_policy())
                    } else {
                        state.cfg.rules.targeting
                    };

                    let _acted =
                        attacker_act(&mut state, eid, &mut events, &mut action_index, policy);
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
                "\nReport finished: {} side defeated.\n",
                if att_alive { "Defending" } else { "Attacking" }
            );
            break;
        }

        // Repack for next round (lines/slots may change)
        repack_layouts(&mut state);
    }

    Ok(())
}

// --------------------------- Core mechanics ---------------------------

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
            .with_context(|| format!("Unknown unit kind '{}'", cu.kind.name()))?
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
                kind: cu.kind,
                class: def.class,
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
                    vec![UnitClass::Land, UnitClass::Naval, UnitClass::Aerial]
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
                kind: h.name,
                class: UnitClass::Land,
                traits: vec![Trait::Hero],
                attack: Some(h.attack),
                range: h.range,
                heal: None,
                heal_range: None,
                defense: h.defense,
                base_health: base_hp,
                bonus_health: bonus_hp,
                cur_health: base_hp + bonus_hp,
                can_target: vec![UnitClass::Land, UnitClass::Naval, UnitClass::Aerial],
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
    fn new(side: Side, side_mods: SideModifiers, template: Vec<Vec<Option<UnitKind>>>) -> Self {
        let mut set = BTreeSet::new();
        for row in &template {
            for &kind in row.iter().flatten() {
                set.insert(kind);
            }
        }
        Self {
            grid: vec![],
            template,
            canon_names: set.into_iter().collect(),
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
    fn canonicalize(&self, kind: UnitKind) -> UnitKind {
        let ck = kind.canonical();
        if self.canon_names.contains(&ck) {
            ck
        } else {
            kind
        }
    }

    fn repack(&mut self, _cfg: &BattleConfig, _round: usize, _is_attacker: bool) {
        let mut buckets: HashMap<UnitKind, VecDeque<Entity>> = HashMap::new();
        for e in self
            .entities
            .values()
            .filter(|e| e.cur_health > 0.0)
            .cloned()
        {
            let key = self.canonicalize(e.kind);
            buckets.entry(key).or_default().push_back(e);
        }

        let mut new_grid: Vec<Vec<EntityId>> = vec![vec![]; self.template.len()];
        for (li, row) in self.template.iter().enumerate() {
            let mut ids: Vec<EntityId> = vec![];
            for (si, cell) in row.iter().enumerate() {
                if let Some(kind) = cell {
                    if let Some(bucket) = buckets.get_mut(kind) {
                        if let Some(e) = bucket.pop_front() {
                            if let Some(mut origin) = self.entities.remove(&e.id) {
                                origin.line = li + 1;
                                origin.slot = si + 1;
                                let eid = origin.id;
                                ids.push(eid);
                                self.entities.insert(eid, origin);
                            }
                        }
                    }
                }
            }
            new_grid[li] = ids;
        }

        if buckets.values().any(|v| !v.is_empty()) {
            panic!("More units than layout capacity");
        }

        self.grid = new_grid;
    }
}

fn print_header(state: &BattleState) {
    let att_units: HashMap<UnitKind, usize> = aggregate_kinds(&state.att);
    let def_units: HashMap<UnitKind, usize> = aggregate_kinds(&state.def_);

    println!("# Attack report (simulated)\n");
    println!("## Armies\n");
    println!("Attacking army:");
    for (k, v) in att_units.iter().sorted_by_key(|(k, _)| k.name()) {
        println!("{}x{}", v, k.name());
    }
    println!("-- total {} units\n", state.att.entities.len());
    println!("Defending army:");
    for (k, v) in def_units.iter().sorted_by_key(|(k, _)| k.name()) {
        println!("{}x{}", v, k.name());
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
        println!();
    }
}

fn aggregate_kinds(form: &Formation) -> HashMap<UnitKind, usize> {
    let mut m: HashMap<UnitKind, usize> = HashMap::new();
    for e in form.entities.values() {
        if e.cur_health > 0.0 {
            *m.entry(e.kind).or_default() += 1;
        }
    }
    m
}

fn grid_to_text_rows(s: SideKind, state: &BattleState) -> Vec<String> {
    let grid = match s {
        SideKind::Left => &state.att.grid,
        SideKind::Right => &state.def_.grid,
    };
    grid.iter()
        .map(|row| {
            let es = match s {
                SideKind::Left => row.iter().collect::<Vec<_>>(),
                SideKind::Right => row.iter().rev().collect::<Vec<_>>(),
            };
            es.into_iter()
                .map(|e| get_entity(state, *e).kind.abbr())
                .join("")
        })
        .collect::<Vec<_>>()
}

fn print_layouts(state: &BattleState) {
    let att_text_rows = grid_to_text_rows(SideKind::Left, &state);
    let def_text_rows = grid_to_text_rows(SideKind::Right, &state);

    use itertools::EitherOrBoth::*;
    for pair in att_text_rows
        .into_iter()
        .zip_longest(def_text_rows.into_iter())
    {
        match pair {
            Both(l, r) => println!("{} {}", l, r),
            Left(l) => println!("{} ...", l),
            Right(r) => println!("... {}", r),
        }
    }

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
                e.kind.name(),
                hp_str
            );
        }
    }
    println!();

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
                e.kind.name(),
                hp_str
            );
        }
    }
    println!();
}

fn is_alive(state: &BattleState, eid: EntityId) -> bool {
    get_entity(state, eid).cur_health > 0.0
}
fn get_entity(state: &BattleState, eid: EntityId) -> &Entity {
    state
        .att
        .entities
        .get(&eid)
        .or_else(|| state.def_.entities.get(&eid))
        .expect("entity")
}
fn get_entity_mut(state: &mut BattleState, eid: EntityId) -> &mut Entity {
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
            (h.kind, h.side, h.line, h.slot)
        };

        let (target_kind, target_side, target_line, target_slot, new_health) = {
            let e = get_entity_mut(state, wid);
            e.cur_health += apply;
            (e.kind, e.side, e.line, e.slot, e.cur_health)
        };

        events.push(format!(
            "{}. {} #{}{}.{} heals {} #{}{}.{} ({:.2}) => +{:.2}. Now {:.2}",
            *idx,
            healer_info.0.name(),
            side_char(healer_info.1),
            healer_info.2,
            healer_info.3,
            target_kind.name(),
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
    policy: TargetingPolicy,
) -> bool {
    // Snapshot attacker properties first
    let (side, line, range, atk_opt, kind, traits, can_target) = {
        let a = get_entity(state, attacker_id);
        (
            a.side,
            a.line,
            a.range,
            a.attack,
            a.kind,
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
                if e.cur_health > 0.0 && can_target.contains(&e.class) {
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
        TargetingPolicy::LowestHealth => {
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
        TargetingPolicy::ClosestThenReadingOrder => {
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
        (t.traits.clone(), t.defense, t.kind, t.side, t.line, t.slot)
    };
    let atk_val = atk + attack_bonus_vs(&traits, &target_traits, &state.cfg.units, kind);
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
        (src.kind, src.side, src.line, src.slot)
    };
    events.push(format!(
        "{}. {} #{}{}.{} attacks {} #{}{}.{}. Attack {:.2} vs Defense {:.2} => damage {:+.2}. {} now {:.2}",
        *idx,
        src_kind.name(),
        side_char(src_side), src_line, src_slot,
        t_kind.name(), side_char(t_side), t_line, t_slot,
        atk_val, def_val, dmg,
        t_kind.name(), new_health
    ));
    *idx += 1;

    true
}

fn attack_bonus_vs(
    _attacker_traits: &[Trait],
    target_traits: &[Trait],
    units: &HashMap<UnitKind, UnitDef>,
    kind: UnitKind,
) -> f64 {
    if let Some(def) = units.get(&kind) {
        let mut add = 0.0;
        for br in &def.bonus_vs {
            if br.when.iter().all(|w| target_traits.contains(w)) {
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
    let mut gone: Vec<(Side, usize, usize, UnitKind)> = vec![];
    for e in state.att.entities.values() {
        if e.cur_health <= 0.0 {
            gone.push((Side::Attacker, e.line, e.slot, e.kind));
        }
    }
    for e in state.def_.entities.values() {
        if e.cur_health <= 0.0 {
            gone.push((Side::Defender, e.line, e.slot, e.kind));
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
                kind.name()
            ));
        }
    }

    // Remove dead
    state.att.entities.retain(|_, e| e.cur_health > 0.0);
    state.def_.entities.retain(|_, e| e.cur_health > 0.0);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn add_unit(form: &mut Formation, kind: UnitKind, is_hero: bool) {
        let id = form.alloc_id();
        let ent = Entity {
            id,
            side: form.side,
            line: 0,
            slot: 0,
            kind,
            class: UnitClass::Land,
            traits: vec![],
            attack: None,
            range: 0,
            heal: None,
            heal_range: None,
            defense: 0.0,
            base_health: 1.0,
            bonus_health: 0.0,
            cur_health: 1.0,
            can_target: vec![],
            is_hero,
        };
        form.entities.insert(id, ent);
    }

    #[test]
    fn repack_attacker_battle_json_layout() {
        let cfg = BattleConfig {
            units: HashMap::new(),
            attacker: ArmyConfig::default(),
            defender: ArmyConfig::default(),
            buildings: vec![],
            rules: RulesConfig {
                max_rounds: 0,
                defender_acts_first: true,
                healer_spillover: false,
                targeting: TargetingPolicy::ClosestThenReadingOrder,
            },
        };

        let layouts = parse_layouts("layouts.dat").unwrap();
        let raw_counts = vec![
            (UnitKind::Horseman, 18),
            (UnitKind::Crossbowman, 4),
            (UnitKind::SisterOfMercy, 9),
            (UnitKind::SageLvl3, 3),
            (UnitKind::Hero, 1),
        ];
        let counts = canonicalize_counts(&raw_counts);
        let template = select_layout(&layouts, SideKind::Left, &counts).unwrap();
        let mut form = Formation::new(Side::Attacker, SideModifiers::default(), template);

        for _ in 0..18 {
            add_unit(&mut form, UnitKind::Horseman, false);
        }
        for _ in 0..4 {
            add_unit(&mut form, UnitKind::Crossbowman, false);
        }
        for _ in 0..9 {
            add_unit(&mut form, UnitKind::SisterOfMercy, false);
        }
        for _ in 0..3 {
            add_unit(&mut form, UnitKind::SageLvl3, false);
        }
        add_unit(&mut form, UnitKind::Hero, true);

        form.repack(&cfg, 1, true);

        let kinds: Vec<Vec<UnitKind>> = form
            .grid
            .iter()
            .map(|row| {
                row.iter()
                    .map(|eid| form.entities.get(eid).unwrap().kind)
                    .collect::<Vec<_>>()
            })
            .collect();

        let expected: Vec<Vec<UnitKind>> = vec![
            vec![
                UnitKind::Crossbowman,
                UnitKind::Crossbowman,
                UnitKind::SageLvl3,
                UnitKind::Horseman,
                UnitKind::Horseman,
                UnitKind::Horseman,
            ],
            vec![
                UnitKind::SisterOfMercy,
                UnitKind::Horseman,
                UnitKind::Horseman,
                UnitKind::SisterOfMercy,
                UnitKind::SisterOfMercy,
                UnitKind::Horseman,
            ],
            vec![
                UnitKind::SageLvl3,
                UnitKind::SisterOfMercy,
                UnitKind::SisterOfMercy,
                UnitKind::Horseman,
                UnitKind::Horseman,
                UnitKind::Horseman,
            ],
            vec![
                UnitKind::Horseman,
                UnitKind::SageLvl3,
                UnitKind::Horseman,
                UnitKind::Horseman,
                UnitKind::Horseman,
                UnitKind::Horseman,
            ],
            vec![
                UnitKind::SisterOfMercy,
                UnitKind::Horseman,
                UnitKind::SisterOfMercy,
                UnitKind::SisterOfMercy,
                UnitKind::Horseman,
                UnitKind::Hero,
            ],
            vec![
                UnitKind::Crossbowman,
                UnitKind::SisterOfMercy,
                UnitKind::Horseman,
                UnitKind::Crossbowman,
                UnitKind::Horseman,
            ],
        ];

        assert_eq!(kinds, expected);
        assert_eq!(form.entities.len(), 35);
    }
}
