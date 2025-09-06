# tt2-bt-sim

Prototype **Totem Tribe 2** battle report simulator written in Rust.

The CLI consumes a JSON battle specification describing unit types,
attacker and defender rosters, optional hero and buildings, plus a few
rule toggles. It outputs a round-by-round combat log that mirrors the
in‑game report: army layouts, numbered attack/heal events with
damage formulas, and eliminations.

## Build and run

```bash
cargo build --release
cargo run --release -- battle.json
```

## JSON outline

The input file contains:

* **`units`** – stats for every unit kind (`class`, `attack`, `range`,
  `health`, `armor`, optional `heal`, `can_target`, bonuses and traits).
* **`attacker`** / **`defender`** – each has a `roster` of units,
  optional `hero`, `side_mods` for global attack/heal/health/armor
  multipliers, and optional layout hints.
* **`buildings`** – optional flat health bonuses that apply to a side.
* **`rules`** – `max_rounds`, `defender_acts_first`, `healer_spillover`
  and the default `targeting` policy.

The repository contains a large example battle transcript in
`attack-reports.txt` for additional context.

## Notes

The simulator uses heuristic mechanics:

* damage formula: `-(Atk^2)/(Atk + Def)`
* unit placement preferences for sisters, sages, crossbows and hero
* optional healer spillover and hero regeneration

These rules aim to approximate observed behaviour and are easy to tweak
by editing the JSON or the Rust code.

