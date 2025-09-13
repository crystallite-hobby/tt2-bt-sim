use serde::{Deserialize, Deserializer};
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnitKind {
    Hero,
    Sage,
    SageLvl3,
    Crossbowman,
    SisterOfMercy,
    Horseman,
    BroilerDragon,
}

impl UnitKind {
    pub fn from_name(s: &str) -> Option<Self> {
        match s {
            "Hero" => Some(UnitKind::Hero),
            "Sage" => Some(UnitKind::Sage),
            "SageLvl3" => Some(UnitKind::SageLvl3),
            "Crossbowman" => Some(UnitKind::Crossbowman),
            "SisterOfMercy" => Some(UnitKind::SisterOfMercy),
            "Horseman" => Some(UnitKind::Horseman),
            "Broiler Dragon" => Some(UnitKind::BroilerDragon),
            _ => None,
        }
    }
    pub fn from_abbr(s: &str) -> Option<Self> {
        match s {
            "He" => Some(UnitKind::Hero),
            "Sg" => Some(UnitKind::Sage),
            "S3" => Some(UnitKind::SageLvl3),
            "Cr" => Some(UnitKind::Crossbowman),
            "Si" => Some(UnitKind::SisterOfMercy),
            "Hm" => Some(UnitKind::Horseman),
            "Dr" => Some(UnitKind::BroilerDragon),
            _ => None,
        }
    }
    pub fn name(&self) -> &'static str {
        match self {
            UnitKind::Hero => "Hero",
            UnitKind::Sage => "Sage",
            UnitKind::SageLvl3 => "SageLvl3",
            UnitKind::Crossbowman => "Crossbowman",
            UnitKind::SisterOfMercy => "SisterOfMercy",
            UnitKind::Horseman => "Horseman",
            UnitKind::BroilerDragon => "Broiler Dragon",
        }
    }
    pub fn abbr(&self) -> &'static str {
        match self {
            UnitKind::Hero => "He",
            UnitKind::Sage => "Sg",
            UnitKind::SageLvl3 => "S3",
            UnitKind::Crossbowman => "Cr",
            UnitKind::SisterOfMercy => "Si",
            UnitKind::Horseman => "Hm",
            UnitKind::BroilerDragon => "Dr",
        }
    }

    pub fn canonical(self) -> Self {
        match self {
            UnitKind::SageLvl3 => UnitKind::Sage,
            other => other,
        }
    }
}

impl fmt::Display for UnitKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl FromStr for UnitKind {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        UnitKind::from_name(s).ok_or_else(|| format!("Unknown unit kind: {}", s))
    }
}

impl<'de> Deserialize<'de> for UnitKind {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        UnitKind::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnitClass {
    Land,
    Naval,
    Aerial,
}

impl UnitClass {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "land" => Some(UnitClass::Land),
            "naval" => Some(UnitClass::Naval),
            "aerial" => Some(UnitClass::Aerial),
            _ => None,
        }
    }
    pub fn as_str(&self) -> &'static str {
        match self {
            UnitClass::Land => "land",
            UnitClass::Naval => "naval",
            UnitClass::Aerial => "aerial",
        }
    }
}

impl fmt::Display for UnitClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for UnitClass {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        UnitClass::from_str(s).ok_or_else(|| format!("Unknown class: {}", s))
    }
}

impl<'de> Deserialize<'de> for UnitClass {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        UnitClass::from_str(&s)
            .ok_or_else(|| serde::de::Error::custom(format!("Unknown class: {}", s)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Trait {
    Mounted,
    Heavy,
    Swords,
    Armored,
    Human,
    Shooting,
    Foot,
    Small,
    Lightweight,
    Medic,
    Heart,
    Magic,
    Monster,
    Hero,
}

impl Trait {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "mounted" => Some(Trait::Mounted),
            "heavy" => Some(Trait::Heavy),
            "swords" => Some(Trait::Swords),
            "armored" => Some(Trait::Armored),
            "human" => Some(Trait::Human),
            "shooting" => Some(Trait::Shooting),
            "foot" => Some(Trait::Foot),
            "small" => Some(Trait::Small),
            "lightweight" => Some(Trait::Lightweight),
            "medic" => Some(Trait::Medic),
            "heart" => Some(Trait::Heart),
            "magic" => Some(Trait::Magic),
            "monster" => Some(Trait::Monster),
            "hero" => Some(Trait::Hero),
            _ => None,
        }
    }
    pub fn as_str(&self) -> &'static str {
        match self {
            Trait::Mounted => "mounted",
            Trait::Heavy => "heavy",
            Trait::Swords => "swords",
            Trait::Armored => "armored",
            Trait::Human => "human",
            Trait::Shooting => "shooting",
            Trait::Foot => "foot",
            Trait::Small => "small",
            Trait::Lightweight => "lightweight",
            Trait::Medic => "medic",
            Trait::Heart => "heart",
            Trait::Magic => "magic",
            Trait::Monster => "monster",
            Trait::Hero => "hero",
        }
    }
}

impl fmt::Display for Trait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for Trait {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Trait::from_str(s).ok_or_else(|| format!("Unknown trait: {}", s))
    }
}

impl<'de> Deserialize<'de> for Trait {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Trait::from_str(&s).ok_or_else(|| serde::de::Error::custom(format!("Unknown trait: {}", s)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AppliesTo {
    Attacker,
    Defender,
    Both,
}

impl AppliesTo {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "attacker" => Some(AppliesTo::Attacker),
            "defender" => Some(AppliesTo::Defender),
            "both" => Some(AppliesTo::Both),
            _ => None,
        }
    }
    pub fn as_str(&self) -> &'static str {
        match self {
            AppliesTo::Attacker => "attacker",
            AppliesTo::Defender => "defender",
            AppliesTo::Both => "both",
        }
    }
}

impl Default for AppliesTo {
    fn default() -> Self {
        AppliesTo::Attacker
    }
}

impl fmt::Display for AppliesTo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for AppliesTo {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        AppliesTo::from_str(s).ok_or_else(|| format!("Unknown applies_to: {}", s))
    }
}

impl<'de> Deserialize<'de> for AppliesTo {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        AppliesTo::from_str(&s)
            .ok_or_else(|| serde::de::Error::custom(format!("Unknown applies_to: {}", s)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TargetingPolicy {
    LowestHealth,
    ClosestThenReadingOrder,
}

impl TargetingPolicy {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "lowest_health" => Some(TargetingPolicy::LowestHealth),
            "closest_then_reading_order" => Some(TargetingPolicy::ClosestThenReadingOrder),
            _ => None,
        }
    }
    pub fn as_str(&self) -> &'static str {
        match self {
            TargetingPolicy::LowestHealth => "lowest_health",
            TargetingPolicy::ClosestThenReadingOrder => "closest_then_reading_order",
        }
    }
}

impl Default for TargetingPolicy {
    fn default() -> Self {
        TargetingPolicy::ClosestThenReadingOrder
    }
}

impl fmt::Display for TargetingPolicy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for TargetingPolicy {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TargetingPolicy::from_str(s).ok_or_else(|| format!("Unknown targeting policy: {}", s))
    }
}

impl<'de> Deserialize<'de> for TargetingPolicy {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        TargetingPolicy::from_str(&s)
            .ok_or_else(|| serde::de::Error::custom(format!("Unknown targeting policy: {}", s)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuildingName {
    FortifiedWallLvl10,
}

impl BuildingName {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "FortifiedWallLvl10" => Some(BuildingName::FortifiedWallLvl10),
            _ => None,
        }
    }
    pub fn as_str(&self) -> &'static str {
        match self {
            BuildingName::FortifiedWallLvl10 => "FortifiedWallLvl10",
        }
    }
}

impl fmt::Display for BuildingName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for BuildingName {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        BuildingName::from_str(s).ok_or_else(|| format!("Unknown building name: {}", s))
    }
}

impl<'de> Deserialize<'de> for BuildingName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        BuildingName::from_str(&s)
            .ok_or_else(|| serde::de::Error::custom(format!("Unknown building name: {}", s)))
    }
}
