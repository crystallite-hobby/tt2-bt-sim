use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

use crate::config::{BattleConfig, SideModifiers};
use crate::layouts::{select_layout, LayoutEntry, LayoutLine, SideKind};
use crate::types::{Trait, UnitClass, UnitKind};

#[derive(Debug, Clone)]
pub struct BattleState {
    pub cfg: BattleConfig,
    pub round: usize,
    pub att: Formation,
    pub def_: Formation,
    pub def_building_hp_bonus: f64,
}

#[derive(Debug, Clone)]
pub struct Formation {
    /// All alive entities in reading order (Line asc, Slot asc). Repacked per round.
    pub units: Vec<ArmyLine>, // lines -> slots -> entity id
    pub template: Vec<LayoutLine>,
    pub canon_names: Vec<UnitKind>,
    pub entities: BTreeMap<EntityId, Entity>,
    pub next_local_id: usize,
    pub side: Side,
    pub side_mods: SideModifiers,
}

#[derive(Debug, Clone)]
pub struct ArmyLine {
    pub units: Vec<EntityId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntityId(pub u64);

#[derive(Debug, Clone)]
pub struct Entity {
    pub id: EntityId,
    pub side: Side,
    pub line: usize,    // 1-based within current layout
    pub slot: usize,    // 1-based within current layout
    pub kind: UnitKind, // e.g. Horseman | SisterOfMercy | Hero
    pub class: UnitClass,
    pub traits: Vec<Trait>,
    pub attack: Option<f64>,
    pub range: usize,
    pub heal: Option<f64>,
    pub heal_range: Option<usize>,
    pub defense: f64, // a.k.a armor after modifiers
    pub base_health: f64,
    pub bonus_health: f64,
    pub cur_health: f64,
    pub can_target: Vec<UnitClass>,
    pub is_hero: bool,
    pub next_enemy_idx: usize,
    pub next_ally_idx: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Side {
    Attacker,
    Defender,
}

impl From<Side> for SideKind {
    fn from(value: Side) -> Self {
        match value {
            Side::Attacker => SideKind::Left,
            Side::Defender => SideKind::Right,
        }
    }
}

impl Formation {
    pub fn new(side: Side, side_mods: SideModifiers, template: Vec<LayoutLine>) -> Self {
        let mut set = BTreeSet::new();
        for line in &template {
            for &kind in &line.units {
                set.insert(kind);
            }
        }
        Self {
            units: vec![],
            template,
            canon_names: set.into_iter().collect(),
            entities: BTreeMap::new(),
            next_local_id: 1,
            side,
            side_mods,
        }
    }

    pub fn alloc_id(&mut self) -> EntityId {
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

    pub fn repack(
        &mut self,
        _cfg: &BattleConfig,
        _round: usize,
        _is_attacker: bool,
        layouts: Option<&[LayoutEntry]>,
    ) {
        if let Some(layouts) = layouts {
            let mut counts: BTreeMap<UnitKind, usize> = BTreeMap::new();
            for e in self
                .entities
                .values()
                .filter(|e| e.cur_health > 0.0)
            {
                let key = self.canonicalize(e.kind);
                *counts.entry(key).or_default() += 1;
            }

            let side_kind: SideKind = self.side.into();
            let template = select_layout(layouts, side_kind, &counts)
                .unwrap_or_else(|| {
                    panic!(
                        "No matching layout for {:?}: {:?}",
                        self.side, counts
                    )
                });

            self.template = template.clone();
            let mut set = BTreeSet::new();
            for line in &self.template {
                for &kind in &line.units {
                    set.insert(kind);
                }
            }
            self.canon_names = set.into_iter().collect();
        }

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

        let mut new_units: Vec<ArmyLine> = Vec::with_capacity(self.template.len());
        for (li, line) in self.template.iter().enumerate() {
            let mut ids: Vec<EntityId> = vec![];
            for (si, &kind) in line.units.iter().enumerate() {
                if let Some(bucket) = buckets.get_mut(&kind) {
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
            new_units.push(ArmyLine { units: ids });
        }

        if buckets.values().any(|v| !v.is_empty()) {
            panic!("More units than layout capacity");
        }

        self.units = new_units;
        self.reset_memory();
    }

    pub fn reset_memory(&mut self) {
        for e in self.entities.values_mut() {
            e.next_enemy_idx = 0;
            e.next_ally_idx = 0;
        }
    }
}
