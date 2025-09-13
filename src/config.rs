use serde::Deserialize;
use std::collections::HashMap;

use crate::types::{AppliesTo, BuildingName, TargetingPolicy, Trait, UnitClass, UnitKind};

#[derive(Debug, Clone, Deserialize, Default)]
pub struct SideModifiers {
    #[serde(default = "one")]
    pub attack_mult: f64,
    #[serde(default = "one")]
    pub heal_mult: f64,
    #[serde(default = "one")]
    pub health_mult: f64,
    #[serde(default = "one")]
    pub armor_mult: f64,
}
fn one() -> f64 {
    1.0
}

#[derive(Debug, Clone, Deserialize)]
pub struct Building {
    pub name: BuildingName,
    #[serde(default)]
    pub health_bonus: f64,
    #[serde(default)]
    pub applies_to: AppliesTo,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CountedUnit {
    pub kind: UnitKind,
    pub count: usize,
    #[serde(default)]
    pub traits: Vec<Trait>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct HeroDef {
    pub name: UnitKind,
    pub attack: f64,
    pub range: usize,
    pub defense: f64,
    pub health: f64,
    #[serde(default)]
    pub regen: f64,
    #[serde(default = "default_hero_policy")]
    pub prioritize: TargetingPolicy,
}
pub fn default_hero_policy() -> TargetingPolicy {
    TargetingPolicy::LowestHealth
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct ArmyConfig {
    pub roster: Vec<CountedUnit>,
    #[serde(default)]
    pub hero: Option<HeroDef>,
    #[serde(default)]
    pub side_mods: SideModifiers,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RulesConfig {
    #[serde(default = "default_max_rounds")]
    pub max_rounds: usize,
    #[serde(default = "default_true")]
    pub defender_acts_first: bool,
    #[serde(default)]
    pub healer_spillover: bool,
    #[serde(default = "default_targeting")]
    pub targeting: TargetingPolicy,
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
pub struct BattleConfig {
    pub units: HashMap<UnitKind, UnitDef>,
    pub attacker: ArmyConfig,
    pub defender: ArmyConfig,
    #[serde(default)]
    pub buildings: Vec<Building>,
    #[serde(default)]
    pub rules: RulesConfig,
}

#[derive(Debug, Clone, Deserialize)]
pub struct UnitDef {
    pub class: UnitClass, // "land" | "naval" | "aerial"
    #[serde(default)]
    pub attack: Option<f64>,
    #[serde(default)]
    pub range: usize,
    #[serde(default)]
    pub heal: Option<f64>,
    #[serde(default)]
    pub heal_range: Option<usize>,
    pub health: f64,
    pub armor: f64,
    #[serde(default)]
    #[allow(dead_code)]
    pub size: u8,
    #[serde(default)]
    pub can_target: Vec<UnitClass>,
    #[serde(default)]
    pub bonus_vs: Vec<BonusRule>,
    #[serde(default)]
    #[allow(dead_code)]
    pub traits: Vec<Trait>,
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct BonusRule {
    pub when: Vec<Trait>, // require all traits present to apply
    pub add: f64,         // flat bonus added to attack
}
