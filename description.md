It is prototype Rust CLI called **“TT2 battle report simulator (Rust CLI, prototype)”**. It reads a JSON battle spec and prints a round-by-round report that mirrors the style of your attached transcript: army layouts per line, numbered events (attacks/heals with formulas, HP changes), and elimination notes. It’s structured to let you tweak unit counts and parameters in the JSON and immediately see a new report.

Here’s what you get and how to use it:

# **What the tool does**

* **Input**: one JSON file describing unit types, attacker roster \+ hero perks/modifiers, defender roster, optional buildings, and a few rule toggles.

* **Output** (stdout): a **multi-round attack report** with:

  * Attacker & defender **layout** by lines/slots.

  * **Events** in order (dragon shots, hero hit, healer ticks w/ spillover, crossbow/sage shots, etc.) showing Attack vs Defense and the computed damage/healing.

  * **Eliminations** as units reach 0 HP.

  * Stops on victory or a configured max round count.

* **Heuristics** (configurable): layout preferences (sisters/sages/crossbows bias to back lines; hero up front), targeting policies, and healer spillover.

# **Build & run**

1. Create a new project and paste the canvas file into `src/main.rs`:

`cargo new tt2-sim --bin`  
`# replace src/main.rs with the canvas code`

2. Add these to `Cargo.toml`:

`[dependencies]`  
`clap = { version = "4", features = ["derive"] }`  
`serde = { version = "1", features = ["derive"] }`  
`serde_json = "1"`  
`anyhow = "1"`  
`itertools = "0.13"`

3. Run with your JSON:

`cargo run --release -- battle.json`

# **JSON you can start with**

In the file header I included a **minimal JSON schema** and a **complete example** (matching your transcript’s unit mix). You can copy-paste that, then tweak:

* `attacker.roster` / `defender.roster` counts

* `attacker.hero` perks (attack/range/defense/health/regen; hero’s target policy)

* `attacker.side_mods` / `defender.side_mods`: `attack_mult`, `heal_mult`, `health_mult`, `armor_mult`

* `buildings`: e.g., `{ "name": "FortifiedWallLvl10", "health_bonus": 100, "applies_to": "defender" }`

* `rules`: `defender_acts_first`, `healer_spillover`, `targeting`, `max_rounds`

* `attacker.layout.columns_hint` to nudge how many slots per line (default approximates √N)

# **Principles I used (short version)**

These are distilled from your transcribed report; the simulator makes them explicit and parameterized so you can experiment.

* **Damage formula**:  
   For a hit, damage ≈ **−(Atk²) / (Atk \+ Def)**; we show `Attack A vs Defense D ⇒ damage −X.YZ` and subtract from target HP.  
   (This reproduces samples like Crossbow 18×1.46 vs Def 36 ⇒ about −23.95, Dragon 112 vs Def 5.05 ⇒ about −107.17.)

* **Army-wide multipliers**:  
   Side modifiers scale **attack**, **healing**, **health**, and **armor** (e.g., \+45% health printed as “225+101.25”, \+1% armor 5→5.05, \+12% healing 15→16.8, \+46% attack 18→26.28). You can set these per side in `side_mods`.

* **Buildings**:  
   Fortifications add flat HP (e.g., **\+100 HP to defenders**). This is added on top of any %-based health multiplier and printed as a \+bonus component.

* **Ranges & line-distance**:  
   Distance across armies is **lineDistance \= attackLine \+ defenseLine − 1**. Units can act if `distance ≤ range`.  
   (E.g., a defender in Line 2 can hit an attacker in Line 1 at distance 2, but not an attacker in Line 2 at distance 3 if its range is only 2.)

* **Healers**:  
   Sisters heal **one target within heal range**; if overflow remains and `healer_spillover=true`, leftover heal **spills** to the next most-wounded ally in range. (This mirrors the “heal top-up, then carryover” steps in your report.)

* **Acting order**:  
   Observationally, defenders seemed to act before attackers, then units proceeded in **reading order** (front line to back, left to right), with healers and ranged attackers taking turns as they come up. The sim follows this deterministic rule:

  * **Defenders** act in reading order.

  * **Attackers** act in reading order.

  * Within a unit’s turn: heal if healer; otherwise attack if in range.

  * **Hero** uses a target policy (default **lowest\_health**) and **regen** happens at round start.

* **Target selection**:

  * **Hero**: `lowest_health` (ties by nearest, then reading order).

  * Others: default `closest_then_reading_order` (ties by reading order). You can switch the global default via `rules.targeting`.

* **Layout** (heuristic but tunable):

  * **Hero**: front line, near the middle (slot 5 if available).

  * **Sisters**: prefer back lines (one per line from back to front in passes).

  * **Sages**/**Crossbows**: prefer back lines (ensure at least one crossbow on line 2 to reach early).

  * **Others** fill from the front.  
     Repacking happens each round so **line/slot IDs change** as units fall—just like in your transcript.

# **Extending/tuning**

* If you later derive the **exact** TT2 placement/initiative/targeting rules, you can swap out the corresponding functions:

  * Layout: `Formation::repack`

  * Targeting: `attacker_act` policy branch

  * Healer logic: `healer_act` (toggle spillover or change priority)

* The damage model already isolates attack/defense and bonuses so you can plug a different function if you discover an alternate curve.

If you want, I can also drop in a **JSON schema** file and a **second preset** (e.g., with different hero perks or tech/buffs) so you can flip between scenarios quickly.

