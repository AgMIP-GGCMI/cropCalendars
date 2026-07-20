# Rule-based crop calendars — methodology and design rationale

This document describes *what the method does* and *why the changes in this release
were made*. It complements the condensed changelog in `../NEWS.md`: `NEWS.md` lists the
changes, this file gives the methodology reference and the rationale behind the
algorithmic and stability changes. Authoritative detail lives in the code
(`../R/`, `../utils/ggcmi_ph3/`).

Method references:

- **Sowing dates** — Waha, K., van Bussel, L.G.J., Müller, C., Bondeau, A. (2012).
  Climate-driven simulation of global crop sowing dates. *Global Ecology and
  Biogeography* 21, 247–259. doi:10.1111/j.1466-8238.2011.00678.x
- **Harvest / maturity dates** — Minoli, S., Egli, D.B., Rolinski, S., Müller, C.
  (2019). Modelling cropping periods of grain crops at the global scale. *Global and
  Planetary Change* 174, 35–46. doi:10.1016/j.gloplacha.2018.12.013

---

## Repository scope — the portable package vs. the GGCMI pipeline

The code has **two layers**, and several choices in this document apply to only one of
them — keep the distinction in mind, because some design decisions are pipeline-specific
and would not belong in a general use of the library:

- **The portable R package (`../R/`)** — the general crop-calendar library
  (`calcClimatology`, `calcSowingDate`, `calcHarvestDate*`, `calcPHU`, `calcPET*`, …). It
  operates on in-memory climate series with no cluster / scheduler / path assumptions. Its
  **algorithmic changes are always on** — the daily-climatology rule port with default
  `smooth_window = 31`, the circular-centroid phase anchors, the sowing-anchored harvest
  crossings, the two-tier wet-season rule, the `doy_wet2` retirement and ΣP/ΣPET (§6.4,
  §6.6) are structural, with no toggle — so the package's default output **differs from the
  published Waha (2012) / Minoli (2019) and `master` rules**. What is off by default is only
  the **year-to-year stability gates** (§6.5): they need a multi-year series, so a single
  library call simply runs the (new) rules without them.
- **The GGCMI / ISIMIP3b pipeline (`../utils/ggcmi_ph3/`)** — a PIK-specific driver that
  runs the package over the global grid, applies the **annual sliding-window scheme** (§3),
  **enables and calibrates** the stability gates, replaces no-season cells with observed
  GGCMI/AgMIP dates, and writes the ISIMIP3b DRS and LPJmL outputs. The annual scheme, the
  specific enabled gate values, and everything about I/O and deployment are pipeline
  choices.

Roughly: §3 and §5 and §6.1 describe the **pipeline**; §4 and §6.3/6.4/6.6 describe the
**package**; §6.5 (stability gates) is package machinery that only the pipeline switches
on. The stability gates are meaningful only across a year-to-year series — they carry
per-cell state from the previous year — so they have no effect on a one-off library call.

## 1. What it produces

For each GCM × scenario, per crop and irrigation regime (rainfed / irrigated), an
annual time series (e.g. historical 1850–2014) of:

- **planting_day** — rule-based sowing day-of-year (DOY),
- **maturity_day** — rule-based maturity ("harvest") DOY,
- **growing_period** — growing-period length (days),
- **seasonality** — climate seasonality type (1–5),
- **harvest_reason** — which rule set the harvest (1–6),
- **planting_season** — winter (1) / spring (2) sowing.

These feed (a) the published ISIMIP3b DRS NetCDF product (stage 02) and (b) the LPJmL
`.clm` inputs — PHUs (stage 03) written into the CLM binaries alongside the sowing and
maturity dates (stage 04).

## 2. Climate inputs

ISIMIP3b bias-adjusted daily climate (per GCM × scenario): `tas` (K → °C), `pr`
(kg m⁻² s⁻¹ → mm/day), `rsds`, `rlds` (W m⁻²), `huss` (kg/kg), `sfcwind` (m/s), `ps`
(Pa). PET is computed with **FAO-56 Penman-Monteith** (needs rsds/rlds/huss/sfcwind/ps
in addition to tas/pr); the older Priestley-Taylor option is retained but not used by
default.

## 3. Temporal structure — annual sliding window

*This annual scheme is a **pipeline** decision, implemented by the driver in
`../utils/ggcmi_ph3/`; the package supplies the ring engine (`R/slidingClimate.R`) and the
per-cell rules it drives, but does not itself impose a sliding window.*

Calendars are computed **annually**: for each year `T` the climatology is the
**preceding `clm_avg_years = 30` years** `[T-30, T-1]`, advanced one year at a time by a
ring buffer (drop the oldest year, add the newest — `R/slidingClimate.R`). Consecutive
years share 29/30 of the window, so the calendar is **smooth by construction and still
rule-consistent** — no output moving average is needed (the earlier 10-year step and its
`plant-day-mavg` variable are gone). `clm_emit_step` (default 1) can coarsen the step to
N years if compute is tight.

For **future scenarios** the window seeds from historical climate: an SSP's first year
(2015) uses `[1985, 2014]` (all historical), and the ring then slides into scenario
climate as `T` advances — so the historical→scenario calendar is continuous.

Intra-year windows operate *within* the 30-year-mean seasonal cycle (`dtemp`, `dprec`,
`dpet` — one value per DOY, averaged across the 30 years):

| quantity | window |
|---|---|
| seasonality type | full 12 months (coefficient of variation) |
| wet-season onset → sowing (`calcDoyWetMonth`) | start of the wettest **120-day window**, by **ΣP/ΣPET** |
| warmest period → `hd_temp_base` harvest | centre of the warmest **`smooth_window`-day window** |
| spring/fall, wet-season-end, hot-day thresholds (`calcDoyCrossThreshold`) | pointwise crossing of the smoothed cycle |
| growing-season limits (fixed crop-parameter offsets) | `min`, `maxrp`, `max_spring`, `rphase_duration` days |

The wettest window and the wet-season end use **ΣP/ΣPET** (ratio of summed P to summed
PET over the window/DOY), **not** the mean of the daily P/PET ratio — the latter
explodes on the rare day where daily PET ≈ 0 (see §6, Bug fixes). A **single** global
day-window `smooth_window` (default 31, odd, ~1 month) drives both the crossing
detectors (each smooths its own input to reject single-day jitter) and the
window/extremum reductions. The stored daily climatology stays raw; smoothing is applied
at rule time, through one primitive `.circRoll(x, w, position, mean)` (a circular O(n)
rolling sum re-indexed by `start`/`center`/`end` and optionally divided to a mean). The
120-day wettest window keeps its own fixed width.

So a published value at year `T` reflects the 30-year climatology `[T-30, T-1]` → the
intra-year windows that locate sowing and harvest. There is no across-year smoothing.

## 4. Algorithm

1. **Daily climatology** — from the 30-year daily series build the daily climatologies
   `dtemp`, `dprec`, `dpet` (one value per DOY 1–365, averaged across years). These drive
   every rule. The two monthly statistics still needed (mean monthly temperature and
   summed monthly precipitation, for the seasonality CVs) are reconstructed downstream by
   binning the daily cycle into 12 calendar months (`.monthlyFromDaily` in
   `calcCropCalendars`); the sliding ring itself carries only the daily fields.

2. **Seasonality type** (`calcSeasonality`, Waha 2012) — from the coefficient of
   variation of the 12 monthly precipitation totals (threshold 0.4) and of the 12 monthly
   mean temperatures in Kelvin (threshold 0.010), plus the coldest-month temperature (the
   mean of the coldest `smooth_window`-day window of `dtemp`): 1 = no seasonality,
   2 = precipitation, 3 = precip+temp, 4 = temperature, 5 = temp+precip. In the sliding
   window a threshold deadband (`seas_eps`) keeps last year's class unless a variable
   moves decisively past its boundary (§6, Hysteresis gates).

### Sowing rules (`calcSowingDate`, Waha 2012)

The crop's `calcmethod_sdate` parameter selects one of two paths:

- **Spring-sown crops** (`STYP_CALC_SDATE`, e.g. maize, rice, soybean, sorghum, millet,
  spring wheat). Sowing depends on the seasonality type:
  - `NO_SEASONALITY` → no usable trigger: a hemisphere placeholder (DOY 1 in NH / 182 in
    SH, `sowing_month = 0`) is emitted and replaced downstream by the observed GGCMI/AgMIP
    date.
  - `PREC` / `PRECTEMP` (water-driven) → sow at the **start of the wettest 120-day
    window** of the daily P/PET climatology (`calcDoyWetMonth`).
  - `TEMP` / `TEMPPREC` (temperature-driven) → sow at the spring warming date, the
    up-crossing of the daily temperature climatology through `temp_spring`
    (`calcDoyCrossThreshold`).
- **Winter-sown crops** (`WTYP_CALC_SDATE`, e.g. winter wheat — vernalising). First the
  winter type is established from the climate:
  - *warm winter* (coldest-window mean > `basetemp.low`): sow ~2.5 months (75 d) before
    the **coldest day** (centre of the coldest window of `dtemp`);
  - *cold winter* (coldest-window mean < −10 °C): winter sowing impossible (sentinel
    −9999);
  - *mild winter*: sow at the autumn cooling date (down-crossing of `temp_fall`).

  The coldest-month temperature and coldest-day anchor are taken from daily windows
  rather than the discrete calendar months, removing the ~30-day quantisation that made
  these dates jump between adjacent climate windows; the spring up-crossing is scanned
  **from the coldest day** so it cannot latch onto an autumn plateau grazing `temp_spring`
  before the winter minimum. Then: if the winter date is after the earliest allowed
  sowing date, sow in **winter**; otherwise, if it is not too cold, clamp to the earliest
  allowed date (winter); otherwise fall back to **spring** sowing.

`sowing_season` (winter/spring) is carried forward — it changes which harvest candidates
apply.

### Harvest rules (`calcHarvestDate*`, Minoli 2019)

Harvest works in three layers.

**(a) Six candidate harvest dates** (`calcHarvestDateVector`). Three are cultivar-cycle
limits (fixed offsets from sowing); three are environmental escapes anchored by the
reproductive-phase duration `rphase_duration`:

| candidate | code | spring formula | meaning |
|---|---|---|---|
| `hd_first` | 1 GPmin | sowing + `min_growingseason` | earliest cultivar — shortest viable cycle (floor) |
| `hd_maxrp` | 2 GPmed | sowing + `maxrp_growingseason` | medium cycle — maximises grain filling |
| `hd_last` | 3 GPmax | sowing + `max_growingseason_st` | latest cultivar — longest viable cycle (ceiling) |
| `hd_wetseas` | 4 Wstress | (P/PET down-crossing of `ppet_ratio` = wet-season end) + `rphase_duration` | escape terminal **water stress** |
| `hd_temp_base` | 5 Topt | (centre of warmest window) + `rphase_duration` | grain filling in the **warmest period** |
| `hd_temp_opt` | 6 Thigh | (last day temp crosses `temp_opt_rphase`) + `rphase_duration` | escape **supra-optimal heat** |

**(b) Agro-climatic classification** (`calcHarvestRule`) — a 3 × 3 matrix of
seasonality × thermal regime of the warmest month, giving a rule number 1–9:

| | t-low (`Tmax ≤ Tbase`) | t-mid (`Tbase < Tmax ≤ Topt`) | t-high (`Tmax > Topt`) |
|---|---|---|---|
| no seasonality | 1 | 4 | 7 |
| precip seasonality | 2 | 5 | 8 |
| mixed (temp) seasonality | 3 | 6 | 9 |

**(c) Selection** (`calcHarvestDate`) — the rule + seasonality + sowing season pick the
winning candidate, computed separately for rainfed and irrigated:

- t-low (rules 1–3): never warm enough to reproduce → harvest as early as possible
  (`hd_first`); a functionality fallback for marginal-cold cells.
- no seasonality, t-mid/high (4, 7): `hd_maxrp` — no season to escape. Rainfed = irrigated.
- precip seasonality (5, 8): rainfed `min(max(hd_first, hd_wetseas), hd_maxrp)` (cut
  short to escape the dry season); irrigated `hd_maxrp` (no water limit).
- mixed/temperature seasonality (6, 9): winter-sown is temperature-driven and bounded by
  `hd_last`, rainfed = irrigated. Spring-sown adds the water term for rainfed only.

`harvest_reason` records which candidate won. **Irrigation enters only at this step**:
rainfed harvest can be advanced by terminal water stress (`hd_wetseas`), irrigated
harvest drops that term and runs to the thermal / cultivar limit. Hence seasonality and
sowing are identical for rainfed and irrigated, while maturity differs.

## 5. Pipeline stages (`utils/ggcmi_ph3/`)

Deployment-specific values (paths, SLURM account, output trees) are centralized in
`settings.sh`; toolchain loading in `env.sh`; algorithm tunables in `00_config.R`.

| stage | script | does |
|---|---|---|
| 00 | `00_config.R` | config: paths from `settings.sh`, `gcms`/`scenarios` matrix, tunables (`clm_avg_years`, `clm_emit_step`, `pet_method`, `smooth_window`, the stability knobs, `phu_smooth_window`) |
| 01 | `01_compute_annual_calendars.R` | **single-pass** form: one job per (GCM, scenario) — stream climate once into the 30-yr ring and emit annual per-crop calendars |
| 01a | `01a_climatology_annual.R` | **split** form, part 1: stream climate through the ring, cache the per-year daily climatology to disk (I/O- and RAM-heavy; no fork here) |
| 01b | `01b_calendars_annual.R` | **split** form, part 2: load one year's climatology at a time, run `calcCropCalendars` via `mclapply`. Re-runs in minutes on any rule change without re-reading the raw climate |
| 02 | `02_assemble_annual_ncdf.R` | GGCMI default-replacement → publication-ready ISIMIP3b DRS NetCDF, written in one pass |
| 03 | `03_calc_phu_for_lpjml.R` | PHUs per year (heat units between sowing and maturity) → NetCDF |
| 04 | `04_write_lpjml_clm.R` | write LPJmL `.clm` inputs (sdate, hdate, phu) as 30-band CLM binaries |

The single-pass `01` and the split `01a`+`01b` are two forms of the same stage. The
split is preferred: it avoids the copy-on-write OOM of the 64-worker fork (§6,
Performance) and lets `01b` re-run cheaply on the cached climatology for rule
experiments. Cost: ~0.6 GB/year climatology cache, regenerable. (Not to be confused with
the deleted 10-year-step `01a`/`01b` of the previous scheme.)

Climate files are **discovered by globbing** the official ISIMIP roots (`CLIMATE_DIR`
search list); ensemble member, scenario tag and year range are read from the file names.
The DRS standardisation (final names, "years since 1601" axis, ascending latitude, fill
values, chunking) is done in the stage-02 R write — there is no NCO/CDO post-processing.

---

## 6. Design rationale for the changes

Grouped by type, matching `NEWS.md`; the package-vs-pipeline layer of each group is as set
out in the Scope section above (§6.1 pipeline, §6.3/6.4/6.6 package, §6.5 package machinery
the pipeline enables). Each entry states the problem and the design choice; all the
stability gates in §6.5 default to off, reproducing the plain rules.

### 6.1 Architecture & pipeline

- **10-year step → annual 30-yr sliding window.** The previous scheme computed calendars
  on a 10-year step and smoothed the result with a moving average. A ring buffer
  (`R/slidingClimate.R`) instead advances the 30-yr climatology one year at a time, so
  calendars are computed every year and are smooth *and* rule-consistent without any
  post-hoc averaging. Future scenarios seed the ring from historical climate.
- **Daily-climatology rule port.** All rule reductions moved from the 12 calendar months
  onto daily windows of the daily cycle (`R/zz_daily_reductions.R`). A daily mean is
  already a per-DOY 30-year mean, so a window reproduces the calendar-month value
  continuously — removing the ~30-day flips that occurred when the warmest/coldest (or
  wettest/driest) "month" alternated between adjacent windows. The seasonality CV
  classifier stays on 12 monthly bins (reconstructed from the daily cycle) to preserve
  its calibrated hard thresholds. Direct callers with no daily series fall back to the
  exact legacy monthly rule.
- **DRS product written directly in R** (stage 02), replacing the NCO/CDO
  post-processing chain (former stages 04–07). Stage 03 computes PHUs and stage 04 writes
  the LPJmL `.clm` inputs.
- **Engine rename.** After the daily port the climate engine builds primarily the
  *daily* climatology, so the "Monthly" names were dropped (`calcMonthlyClimate` →
  `calcClimatology`, etc.). Pure rename.
- **Dynamic climate discovery** replaces hardcoded year/member tables: files are globbed
  from the official ISIMIP roots and their metadata read from the file names. A uniform
  67 420-cell GGCMI mask (`ggcmi_landcells.csv`) makes the full-grid official files yield
  a consistent cell set.

### 6.2 Performance (output-preserving)

- **`calcCropCalendars` ~2.5× faster** — cumulative-sum rolling windows, `list2env`
  parameter unpacking, optional pre-extracted `crop_parameters` row. Bit-identical.
- **Streaming, cell-vectorised climate engine** (`R/climateAccum.R`) shared by
  `calcClimatology` and stage 01a: adds years one at a time, vectorised over all cells,
  so the full grid is processed one year at a time without holding the multi-year series
  in memory. Bit-identical.
- **PHU stage vectorised** across all land cells (flat-index mapping, year-by-year
  climate accumulation, `mclapply`), and the decadal-median reduction vectorised.
- **`mclapply` copy-on-write OOM.** The single-pass driver forks workers from a parent
  holding the ~24 GB ring; each worker's copy-on-write blew peak RSS past the node
  budget. Because RSS and the SLURM memory allowance both scale with the worker count,
  fewer cores does not help. The structural fix is the **01a/01b split**: `01b` holds only
  one year's climatology at fork time. The single-pass path documents `R_GC_MEM_GROW=0`
  and a worker cap as a fallback.

### 6.3 Refactoring & cleanup

- **One smoothing primitive, one window.** The daily port initially left the climatology
  read through two windows (a tunable crossing smoother and a structural reduction window)
  plus a dormant build-time smoother. These are consolidated into a single global
  `smooth_window` (default 31, odd) and a single primitive `.circRoll(x, w, position,
  mean)`. An odd window is exactly symmetric for the centred smoothing/argmax; 31 keeps
  the ~1-month width. The build-time smoother, `.circRollSum`, `.smoothCycle` and
  `.driestWindowPpet` are removed.
- **Always-wet floor unified onto the wet-end series.** The no-wet-end aridity test now
  reads `min(daily_ppet)` — the minimum of the same curve the wet-end crossing tests
  against `ppet_ratio`. Under one `smooth_window` this is bit-identical to the old
  separate driest-window reduction, so it is a no-op for the default path but makes
  `ppet_min` and `ppet_ratio` compare against one curve.
- **Scalar rule helpers delegate to their vectorised cores.** `calcVd`, `isWinterCrop`,
  `calcVrf` and `calcPHU` are thin wrappers over the `_vec` cores the PHU pipeline
  already used, giving one source of truth per rule (public signatures unchanged); an
  equivalence harness (`tests/testthat/test-vec-equivalence.R`) fuzzes them against the
  original scalar bodies. `.monthly_temps_vec` is subsumed by `.monthlyFromDaily`.
- **Dead code / files removed:** `replaceJumps`, `rollMeanInSteps`,
  `whichOverlappingSeasons`, the legacy 10-year-step pipeline scripts and the NCO/CDO
  stages, and scratch test runners. Deprecated parameters (`monthly_ppet_diff`,
  `harv_ppet_eps`) are kept in signatures, ignored, so existing calls do not break.

### 6.4 Bug fixes

- **Harvest crossings anchored at sowing.** `calcDoyCrossThreshold` returned the earliest
  crossing scanning from Jan 1. On the high-fidelity daily series a marginal year whose
  smoothed signal grazes a threshold mid-season then registered a spurious early crossing
  that pre-empted the genuine after-sowing one (wet-end and hot-day detectors), flipping
  the harvest year to year. All harvest crossings now scan from the sowing date — the
  physically meaningful first crossing after sowing — matching the coldest/warmest-day
  anchors on the sowing side. (The flaw was latent in the monthly-interpolated `master`
  code, which rarely produced the fine graze crossings; the daily port exposed it.)
- **Wettest-window hysteresis anchor.** The per-cell carried state was taken from crop
  #1's resolved *sowing day*, which equals the wettest-window DOY only for crops on the
  wet-season branch — but a vernalising winter crop (crop #1 in a combined run) takes the
  winter branch, so its sowing day corrupted the anchor fed to every crop. `wet_doy` is
  now computed crop-independently from `dprec`/`dpet`.
- **P/PET blow-up.** The daily P/PET *ratio* explodes where PET ≈ 0, and one such day in
  the 30-yr mean dominated the wettest-window search. `calcDoyWetMonth` and `hd_wetseas`
  now use `ΣP/ΣPET` (sum P and PET separately, then divide).
- **Daily rewrites of `calcDoyWetMonth` / `calcDoyCrossThreshold`** replaced a
  monthly→daily interpolation whose modular index reduction was wrong for late-year events
  (wrap errors up to ~351 days), using a 120-day circular window and a Dec→Jan wrapped
  crossing detector.
- **Daily-resolution harvest candidates.** `hd_temp_base` uses the warmest window (was
  the warmest-month mid-day, which flipped ~31 days when two months were near-tied);
  `hd_wetseas`/`hd_temp_opt` no longer return a month index used as a DOY.
- **`seasonality` / `harvest_reason` integer codes** were encoded per-pixel with
  `as.numeric(as.factor(...))`, so every temporally-constant pixel collapsed to code 1.
  Now mapped through fixed global factor levels matching the NetCDF `long_name`.
- **Latitude inversion** in the DRS output fixed (the official product is ascending; the
  old `cdo invertlat` produced descending).
- **Leap-year DOY fold** in `date_to_doy(skip_feb29 = TRUE)` corrected to fold Feb 29
  onto DOY 59.
- **FAO-56 / PET correctness:** `calcPET_FAO56` preserves matrix shape for vectorised
  callers; the P/PET denominator is floored to avoid `NaN`/`Inf` from zero-PET polar
  months; `calcPET` no longer requires latitude/day when observed `rsds`/`rlds` are given.
- **I/O and namespace:** `readNcdf` subsets the time axis positionally (`index_dims`) for
  non-0-based ISIMIP axes; PHU-stage namespace fixes (`isWinterCrop`/`calcVd`/`calcVrf`,
  `grid_df`) and the 1-cell LPJmL/GGCMI grid mismatch resolved.

### 6.5 Hysteresis gates (year-to-year stability)

Even under the annual sliding window, a subset of cells flipped sowing/harvest by
tens-to-hundreds of days between adjacent years by *grazing* a rule boundary — a variable
sitting on a threshold, a near-tie between two peaks, a shallow dip. The fixes below give
each mechanism a matching deadband or Schmitt-trigger gate. Every gate defaults to off
(`eps = 0` / area budget 0), so with them off the rules run **without any year-to-year
hysteresis** (the always-on rule set of §6.4/§6.6, not the original published rules); the
stability comes from carrying one small piece of per-cell state from the previous year.
**These parameters live in the package functions, but they are meaningful only across a
year-to-year series and are enabled and calibrated by the pipeline** — a single-calendar
library call leaves them off.

- **Wet-window near-tie** (`calcDoyWetMonth`). Many PREC/PRECTEMP cells have a second
  120-day `ΣP/ΣPET` peak close to the best, so the plain argmax flips between far-apart
  peaks. The window is chosen as `argmax( (ws/max ws) · w(Δ) )`, a distance-weighted,
  max-normalised selection keyed on last year's window `Δ`. The weight
  `w(x) = (1−eps) + eps·exp(−(x/decay)²)` is Gaussian with an explicit floor `1−eps`: a
  near peak is essentially free, a far peak is penalised but still wins when decisively
  wetter (a genuine regime shift). `eps = 0` / no prior reproduces the plain argmax.
- **Seasonality class deadband** (`calcSeasonality`). Grazing a classifier threshold
  swaps the entire sowing rule. Each threshold is relaxed toward keeping last year's class
  (a test the class was on the high side of uses `thr·(1−seas_eps)`, else `thr·(1+seas_eps)`;
  the `min_temp` test uses an absolute °C margin). The class is crop-independent and
  carried per-cell.
- **Winter-regime deadbands** (`calcSowingDate`). The warm/mild/cold winter boundaries
  each select a different autumn anchor, so a grazing coldest-window temperature flips the
  sowing date by ~half a year. `winter_margin` (warm boundary) and `winter_cold_margin`
  (cold boundary) make each boundary sticky within a `2·margin` band; they are decoupled
  so the continental cold boundary can be widened independently of the warm one.
- **`cross_min_area` deficit-days guard** (`calcDoyCrossThreshold`). A P/PET
  down-crossing counts only if its integrated excursion Σ|daily_ppet − threshold| over the
  sustained run reaches a budget, magnitude-weighting persistence (a shallow dip must
  persist far longer than a deep one). It is a hysteretic Schmitt pair `c(lo, hi)`: a
  previously-found wet-end uses the lenient `lo`, a previously-absent one the strict `hi`,
  so a dip whose area grazes the budget cannot flip the wet-end's existence. Nudging the
  *area* is monotonic in dip depth, so — unlike nudging the threshold level — it cannot
  destroy the crossing it is meant to keep. It also guards the tier-2 `ppet_min` floor
  crossing.
- **`temp_cross_min_area`** — the temperature analogue on the reproductive hot-day
  crossing (`hd_temp_opt` vs `temp_opt_rphase`), damping its existence flicker in marginal
  cells whose warm plateau sits at the threshold.
- **Wet-near gate** (`calcHarvestDateVector`). Whether the cell is in, or imminently
  entering, an active wet season is tested over a window after sowing — admitting a monsoon
  onset that opens a week or two after a temperature-set sowing. The window is hysteretic
  (`cc_wet_window_lo`/`hi`); `wet_near_min_area` optionally replaces the single-day
  above-threshold max with an integrated-area test so a thin P/PET touch reads as not wet.
- **Harvest-rule deadband** (`harv_tmax_margin`) on the warmest-window temperature vs the
  base/optimum reproductive thresholds, suppressing thermal-class flips.
- **Winter-wheat `earliest_sdate` clamp.** A mild vernalising cell whose autumn
  `temp_fall` crossing grazes the earliest allowed sowing date is clamped to that date and
  winter-sown, rather than kicked half a year to the spring fallback on a one-day crossing
  wobble.
- **Phase anchors as circular centroids** (`R/zz_daily_reductions.R`). The coldest/
  warmest-day anchors are computed as a magnitude-weighted circular centroid of the
  smoothed cycle, not the raw argmin/argmax, because the plain extremum is hypersensitive
  in cells with a broad flat trough or plateau (the single lowest/highest day jumps ~30 d
  between climatology years though the centre barely moves). A degenerate/semiannual-cycle
  guard falls back to the plain extremum when the first-harmonic resultant is weak. The
  centroid is a pure phase estimator — weights depend on the shape of the cycle, not its
  level — so it does not drift with warming.

### 6.6 Behavioral / methodology changes

- **Two-tier wet-season harvest decision** (`calcHarvestDateVector`), organised by the
  **state at sowing** rather than "wet-end found / not found". Tier 1 (wet near sowing):
  the wet-end is the first persistent down-crossing after sowing → escape, clamped to
  `[hd_first, hd_last]`; if none is found the season never ends → `hd_last`. Tier 2 (dry
  at sowing): fall back to the aridity floor `ppet_min` with the same persistence-guarded
  machinery one threshold lower → `hd_last` if still wet enough, else `hd_first`. For the
  crops with `ppet_min == ppet_ratio` tier 2 is a no-op; only Rice (`ppet_ratio = 1.0`,
  `ppet_min = 0.5`) exercises it. This replaces the raw-min "always-wet" test (which a
  shallow sub-threshold dip could still drag below the floor) and is self-healing when a
  wet-end oscillates around sowing — both directions land on `hd_first`.
- **Retired the `doy_wet2` trend wet-end estimate.** The escape now uses the single level
  crossing `doy_wet1`. The trend candidate (a declining-moisture crossing) never decided
  wet-end existence — it only pulled a found escape earlier — and its series crossed its
  threshold more jitterily year to year, so dropping it both simplifies the rule and
  removes a flicker source; the affected cells route to the stable `hd_maxrp` rotation cap.
- **Robust spring / no-crossing fallbacks.** When `temp_spring` is never crossed, the
  spring sowing DOY falls back to the day of closest approach — the warmest day for a cold
  cell (whose series only reaches up to the threshold at the summer peak), the winter
  trough for a warm cell (whose series only dips toward it in winter) — so the
  default↔found transition is continuous instead of a half-year jump.
- **FAO-56 Penman-Monteith PET** (`calcPET_FAO56`; grass reference crop, `rs = 70 s/m`) is
  the PET method the pipeline selects (`pet_method`), with Priestley-Taylor retained as a
  package option. `calcPET` gains an observed-radiation branch that computes net radiation
  from actual `rsds`/`rlds` fluxes rather than orbital geometry.
- **PHU matches each year's growing period** (`generatePHUTserie_isimip3`): reads
  `planting_day`/`maturity_day` from the DRS file and accumulates heat units between them;
  `phu_smooth_window` optionally widens only the temperature averaging.

---

## 7. Differences vs. the original standalone pipeline (by design)

These follow from the annual sliding-window design, not from bugs:

- **`planting_day` is the smooth annual rule-derived series.** The annual window makes it
  smooth without averaging, so the standalone's `plant-day` (a 30-yr rolling mean of a
  10-yr-step series) maps to it directly; the moving-average variables are dropped.
- **Harvest** is the annual rule-derived `maturity_day` (no separate smoothed variant);
  the daily-resolution fixes removed the whole-month quantisation jumps.

## 8. Output variables (DRS NetCDF)

| variable | meaning |
|---|---|
| `planting_day` | sowing DOY (annual, 30-yr sliding window) |
| `maturity_day` | maturity DOY (= sowing + growing period) |
| `growing_period` | growing-period length (days) |
| `seasonality` | 1=NoSeas 2=Prec 3=PrecTemp 4=Temp 5=TempPrec |
| `harvest_reason` | 1=GPmin 2=GPmed 3=GPmax 4=Wstress 5=Topt 6=Thigh |
| `planting_season` | 1=winter, 2=spring |

`time` = "years since 1601-1-1", calendar standard; latitude **ascending**;
`_FillValue`/`missing_value` = 1e20.

## 9. References

- Waha, K., van Bussel, L.G.J., Müller, C., Bondeau, A. (2012). Climate-driven simulation
  of global crop sowing dates. *Global Ecology and Biogeography* 21, 247–259.
- Minoli, S., Egli, D.B., Rolinski, S., Müller, C. (2019). Modelling cropping periods of
  grain crops at the global scale. *Global and Planetary Change* 174, 35–46.
