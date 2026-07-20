# cropCalendars NEWS

## 0.2.0 — annual sliding-window pipeline

This repository has **two parts**, and the changes below touch both — each bucket notes
which layer it applies to:

1. **The portable R package (`R/`)** — the general crop-calendar library (Waha 2012
   sowing, Minoli 2019 harvest). It has no cluster / scheduler / path dependencies. Its
   **algorithmic changes are always on** — the daily-climatology rule port with default
   `smooth_window = 31`, circular-centroid phase anchors, sowing-anchored harvest
   crossings, the two-tier wet-season rule, the `doy_wet2` retirement, and ΣP/ΣPET are
   structural (no toggle), so the library's default output **differs from the published /
   `master` rules**. What is off by default is only the **year-to-year stability gates**
   (`eps = 0` / margin 0 / area 0): they need a multi-year series, so a single library call
   simply runs the rules without them.
2. **The GGCMI / ISIMIP3b pipeline (`utils/ggcmi_ph3/`)** — a PIK-specific driver that
   composes the package over the global grid, runs the **annual 30-year sliding-window**
   scheme, **enables and calibrates** those stability gates, and writes the ISIMIP3b /
   LPJmL outputs.

The algorithm changes (always on) and the bug fixes live in the package; only the annual
scheme, the choice to enable and calibrate the stability gates, and all I/O and deployment
are pipeline-specific. Rationale is in `docs/METHODOLOGY.md`; the portability split is in
`README.md`.

### Architecture & pipeline

*Pipeline layer (the annual scheme and I/O), built on two package changes it uses — the
daily-climatology rule port and the engine rename.*

- **Annual 30-year sliding window** (`R/slidingClimate.R`). The calendar for each year
  `T` is built from the climatology of the preceding 30 years `[T-30, T-1]`, advanced
  one year at a time by a ring buffer. Consecutive years share 29/30 of their window,
  so the series is smooth *and* rule-consistent — the previous 10-year-step scheme and
  its output moving average (`plant-day-mavg`/`-window`) are dropped. Future scenarios
  seed the ring from historical climate so the historical→scenario series is continuous.
- **Rules ported to the daily climatology.** All calendar rules read per-DOY daily
  climatologies (`dtemp`, `dprec`, `dpet`); extremum/phase reductions use daily windows
  instead of the 12 calendar months, removing the ~30-day quantisation jumps. Direct
  callers with no daily series fall back to the exact legacy monthly rule
  (`R/zz_daily_reductions.R`).
- **Stage 01 split into `01a` (climatology) + `01b` (calendars).** `01a` streams the
  raw climate through the ring and caches the per-year daily climatology; `01b` loads
  one year at a time and runs `calcCropCalendars` under `mclapply`. Rule changes re-run
  in minutes on the cache without re-reading the raw climate. A fused single-pass
  driver (`01_compute_annual_calendars.R`) is retained as an option.
- **Publication-ready output written directly in R.** Stage 02
  (`02_assemble_annual_ncdf.R`) writes the ISIMIP3b DRS NetCDF in one pass (final
  variable names, "years since 1601" axis, ascending latitude, fill values, chunking,
  publish path), replacing the former NCO/CDO post-processing stages `04`–`07`. Stage
  03 computes PHUs and stage 04 writes LPJmL `.clm` inputs (24-band CFT + 30-band CLM).
- **Climate engine renamed** to drop the misleading "Monthly": `initMonthlyClimate` →
  `initClimateAccum`, `addYearMonthlyClimate` → `addYearClimate`,
  `finalizeMonthlyClimate` → `finalizeClimate`, `calcMonthlyClimate` → `calcClimatology`
  (files, `man/`, tests updated). Pure rename, no behaviour change.
- **Dynamic climate discovery.** Climate files are globbed from the official ISIMIP
  roots (3b primary+secondary, 3a obsclim/spinclim); ensemble member, scenario tag and
  year range are read from the file names. Uniform 67 420-cell GGCMI mask
  (`ggcmi_landcells.csv`). Deployment paths/accounts centralised in `settings.sh`,
  toolchain in `env.sh`, tunables in `00_config.R`.

### Performance

*Package (`calcCropCalendars`, the climate engine, PHU vectorisation) and pipeline (the
fork / out-of-memory handling).*

- **`calcCropCalendars` ~2.5× faster** via cumulative-sum rolling windows, `list2env`
  parameter unpacking, and an optional pre-extracted `crop_parameters` row. Output
  bit-identical.
- **Streaming, cell-vectorised climate engine** (`R/climateAccum.R`): adds years one at
  a time, vectorised over all grid cells, so the pipeline processes the full grid one
  year at a time without holding the multi-year series in memory. `calcClimatology` is
  now a thin per-pixel wrapper over it. Bit-identical output.
- **PHU stage fully vectorised** across land cells (`generatePHUTserie_isimip3.R`):
  flat-index mapping, year-by-year climate accumulation to cap peak RAM, and
  parallelisation with `mclapply` (`ncores`); decadal-median reduction vectorised.
- **`mclapply` OOM removed by the 01a/01b split.** The single-pass driver forked
  workers from a parent holding the ~24 GB ring, ballooning via copy-on-write; the split
  holds only one year's climatology at the fork. The single-pass path documents
  `R_GC_MEM_GROW=0` and a worker cap as a fallback.

### Refactoring & cleanup

*Package, plus removal of the superseded pipeline scripts.*

- **One smoothing primitive and one window.** `.circRollSum` and `.smoothCycle` are
  merged into `.circRoll(x, w, position, mean)` (a circular O(n) rolling sum re-indexed
  by `start`/`center`/`end`, optionally divided to a mean). A single global
  `smooth_window` (default **31**, odd) drives both the threshold-crossing inputs and
  the extremum/window reductions, replacing the former separate 15-day crossing window
  and 30-day reduction window. The dormant build-time climatology smoother
  (`.circSmooth`) and the `.driestWindowPpet` helper are removed; the always-wet floor
  now reads the same P/PET series as the wet-end crossing.
- **Scalar rule helpers delegate to their vectorised cores.** `calcVd`, `isWinterCrop`,
  `calcVrf` and `calcPHU` are now thin wrappers over the `_vec` cores used by the PHU
  pipeline (one source of truth per rule; public signatures unchanged), and
  `.monthly_temps_vec` is subsumed by `.monthlyFromDaily`. An equivalence harness
  (`tests/testthat/test-vec-equivalence.R`) fuzzes the wrappers against the original
  scalar bodies.
- **Dead code and files removed:** `replaceJumps`, `rollMeanInSteps`,
  `whichOverlappingSeasons`, the legacy 10-year-step pipeline scripts, the NCO/CDO
  post-processing stages, and scratch test runners.
- **Deprecated parameters retained (ignored) for call compatibility:**
  `monthly_ppet_diff` and `harv_ppet_eps` in `calcHarvestDateVector` /
  `calcCropCalendars`; `wet_window_eps = 0` and the other stability knobs default to off,
  reproducing the plain rules.

### Bug fixes

*Mostly package (rule and PET correctness); the latitude-inversion and integer-code
fixes are in the pipeline's output writer.*

- **Harvest threshold crossings anchored at sowing.** `calcDoyCrossThreshold` returned
  the earliest crossing scanning from Jan 1, so a marginal year whose smoothed signal
  grazes a threshold mid-season registered a spurious early crossing that pre-empted the
  genuine after-sowing one (wet-end and hot-day detectors). All harvest crossings now
  scan from the sowing date, matching the coldest/warmest-day anchors on the sowing side.
- **Wettest-window hysteresis anchor.** The per-cell carried state was taken from crop
  #1's resolved *sowing day*, which is not the wettest-window DOY for a winter/vernalising
  crop — corrupting the anchor fed to every crop in a combined run. `wet_doy` is now
  computed crop-independently from `dprec`/`dpet`.
- **`ΣP/ΣPET` for the wet season** (sowing onset and `hd_wetseas`). The daily P/PET
  *ratio* blows up where PET ≈ 0; `calcDoyWetMonth`/`hd_wetseas` now use the ratio of
  summed P and PET, fixing spurious sowing flips in monsoon cells.
- **Daily rewrites of `calcDoyWetMonth` and `calcDoyCrossThreshold`** fixed the
  index-wrapping errors (up to ~351 days) of the previous monthly→daily interpolation,
  using a direct 120-day circular window and a Dec→Jan wrapped crossing detector.
- **Daily-resolution harvest candidates** in `calcHarvestDateVector`: `hd_temp_base`
  now uses the warmest 30-day window (was warmest-month mid-day); `hd_wetseas`/
  `hd_temp_opt` no longer return a month index used as a DOY.
- **`seasonality` / `harvest_reason` integer codes** were encoded per-pixel with
  `as.numeric(as.factor(...))`, collapsing every temporally-constant pixel to code 1.
  Now mapped through fixed global factor levels matching the NetCDF `long_name`.
- **Latitude inversion** in the DRS output fixed (the official product is ascending).
- **Leap-year DOY fold** in `date_to_doy(skip_feb29 = TRUE)` corrected to DOY 59.
- **FAO-56 PET correctness:** `calcPET_FAO56` preserves matrix shape for vectorised
  callers; monthly and daily P/PET denominators floored to avoid `NaN`/`Inf` from
  zero-PET polar months; `calcPET` no longer requires latitude/day when `rsds`/`rlds`
  are supplied.
- **`readNcdf`** subsets the time axis by position (`index_dims`) for non-0-based ISIMIP
  axes; PHU stage namespace/grid fixes (`isWinterCrop`/`calcVd`/`calcVrf`, `grid_df`),
  and the 1-cell LPJmL/GGCMI grid mismatch resolved.

### Hysteresis gates (year-to-year stability)

A subset of cells flipped sowing/harvest by tens-to-hundreds of days between adjacent
years even under the annual window, by grazing a rule boundary. Each mechanism gets a
matching deadband/gate; **all default to off**, so turning them off runs the rules
**without any year-to-year stickiness** (note: that is the always-on rule set of this
release, not the original published rules — see the intro). *These parameters live in the
package functions, but enabling them and choosing the calibrated values is a pipeline
decision — they are meaningful only across a year-to-year series like the sliding window,
not for a single calendar.*

- **Wet-window near-tie** (`calcDoyWetMonth`). PREC/PRECTEMP cells often have a second
  120-day `ΣP/ΣPET` peak close to the best, so the plain argmax flips between far-apart
  peaks. The window is chosen by a distance-weighted, max-normalised selection keyed on
  last year's window, with a Gaussian distance kernel and an explicit floor
  (`wet_window_eps`, `wet_window_decay`).
- **Seasonality class deadband** (`calcSeasonality`). Grazing a classifier threshold
  (`CV_prec`, `CV_temp`, `min_temp`) swaps the entire sowing rule; a thermostat-style
  deadband keyed on last year's class (`seas_eps`, `mtemp_margin`) keeps it unless a
  variable moves decisively past the boundary.
- **Winter-regime deadbands** (`calcSowingDate`). The warm/mild/cold winter boundaries
  each pick a different autumn anchor; `winter_margin` and `winter_cold_margin` make each
  boundary sticky, decoupled so the continental cold boundary can be widened independently.
- **`cross_min_area` deficit-days guard** (`calcDoyCrossThreshold`). A P/PET
  down-crossing counts only if its integrated excursion reaches a budget, magnitude-
  weighting persistence. It is a hysteretic Schmitt pair `c(lo, hi)` selected on whether
  a wet-end existed last year, so a grazing dip can't flip the wet-end's existence.
- **`temp_cross_min_area`** — the temperature analogue on the reproductive hot-day
  crossing (`hd_temp_opt`), damping its existence flicker in marginal warm-plateau cells.
- **Wet-near gate** (`calcHarvestDateVector`). Whether the cell is in / imminently
  entering an active wet season is tested over a hysteretic window after sowing
  (`cc_wet_window_lo`/`hi`); `wet_near_min_area` optionally replaces the single-day max
  with an integrated-area test.
- **Harvest-rule deadband** (`harv_tmax_margin`) on the warmest-month thermal class.
- **Winter-wheat `earliest_sdate` clamp.** A mild vernalising cell whose autumn
  crossing grazes the earliest allowed sowing date is clamped to it and winter-sown,
  instead of flipping half a year to the spring fallback.

### Behavioral / methodology

*Package rule behaviour, except the per-year PHU accumulation, which is pipeline.*

- **Two-tier wet-season harvest decision**, organised by the state at sowing rather than
  "wet-end found / not found". Tier 1 (wet near sowing): escape at the first persistent
  P/PET down-crossing, or `hd_last` if the season never ends. Tier 2 (dry at sowing):
  fall back to the aridity floor `ppet_min` with the same persistence-guarded machinery,
  else `hd_first`. This replaces the raw-min "always-wet" test (and retires `min_ppet`,
  `harv_ppet_eps`, `prev_always_wet`).
- **Retired the `doy_wet2` trend wet-end estimate.** The escape now uses the single
  level crossing `doy_wet1`; the trend candidate never decided wet-end existence and was
  the noisier of the two series. `USE_WET2`/`cc_use_wet2` and the `ppet_ratio_diff`
  input are obsolete.
- **Phase anchoring by circular centroid.** The coldest/warmest-day anchors (winter
  sowing, spring/autumn crossing scans, `hd_temp_base`, the spring fallback) use a
  magnitude-weighted circular centroid of the smoothed cycle rather than the raw
  argmin/argmax, with a degenerate/semiannual-cycle guard — stable across climatology
  years in cells with a flat trough or plateau.
- **Robust spring / no-crossing fallbacks.** When `temp_spring` is never crossed, the
  spring sowing DOY falls back to the day of closest approach (warmest day for cold
  cells, winter trough for warm cells) so the default↔found transition is continuous.
- **FAO-56 Penman-Monteith PET** (`calcPET_FAO56`; needs
  `rsds`/`rlds`/`huss`/`sfcwind`/`ps`) is available in the package alongside
  Priestley-Taylor; the pipeline selects FAO-56 via `pet_method`. `calcPET` gains an
  observed-radiation branch that uses actual `rsds`/`rlds` fluxes.
- **PHU matches each year's growing period** (`generatePHUTserie_isimip3` reads
  `planting_day`/`maturity_day` from the DRS file; `smooth_window` optionally widens only
  the temperature averaging).

---

## 0.1.2 and earlier

See git log on the `master` branch.
