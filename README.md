# cropCalendars <a href=''><img src='./inst/img/logo_0.1.0.png' align="right" height="139" /></a>

R-package for simulating crop calendars and their adaptation following the approaches from [Waha et al. (2012)](https://doi.org/10.1111/j.1466-8238.2011.00678.x) and [Minoli et al. (2019)](https://doi.org/10.1016/j.gloplacha.2018.12.013).

**Documentation:** [`NEWS.md`](NEWS.md) is the changelog; [`docs/METHODOLOGY.md`](docs/METHODOLOGY.md) is the methodology reference and the design rationale behind the algorithm and stability changes.

## Installation

```bash
git clone https://github.com/AgMIP-GGCMI/cropCalendars.git .
cd ..
R CMD build cropCalendars
# this generates e.g. cropCalendars_0.2.0.tar.gz
R CMD INSTALL [-l my/Rlib/path] cropCalendars_0.2.0.tar.gz
```

Alternative in R with `devtools`

```r
library(devtools)
devtools::build()
devtools::install()
```

## Usage

See examples in `./utils`

```r
library(cropCalendars)

# Main functions
climate <- calcClimatology()
crop_calendars <- calcCropCalendars()
phenological_heat_units <- calcPHU()
```

## What is portable, and what is not

This repository has **two layers**:

1. **The R package (`R/`, portable).** The core algorithm functions
   (`calcClimatology`, `calcCropCalendars`, `calcSowingDate`, `calcHarvestDate`,
   `calcPHU`, `calcPET` / `calcPET_FAO56`, …) are plain R that operate on in-memory
   vectors / data frames. Default crop parameters ship inside the package
   (`inst/extdata/crop_parameters.csv`, read via `system.file`). The only hard
   dependencies are CRAN packages (`ncdf4`, `data.table`). This layer runs on **any**
   machine with R — no cluster, scheduler, or fixed data paths required. Note that the
   **algorithmic improvements are baked into these functions and are always on** (daily-
   climatology evaluation with default `smooth_window = 31`, circular-centroid phase
   anchors, sowing-anchored harvest crossings, the two-tier wet-season rule, the
   `doy_wet2` retirement, ΣP/ΣPET), so the library's default output **differs from the
   original published Waha (2012) / Minoli (2019) implementation**. What *is* off by
   default is only the **year-to-year stability gates** (the `*_eps` / `*_margin` /
   `*_min_area` knobs): they need a multi-year series and are enabled and calibrated by the
   pipeline, so a single library call runs the rules without them (see below).

2. **The GGCMI / ISIMIP3b pipeline (`utils/ggcmi_ph3/`, PIK-specific).** A set of
   driver scripts that run the package over the global grid for the GGCMI phase-3
   protocol. This layer is tied to the PIK cluster (SLURM, environment modules) and to a
   fixed input-data layout. See below for how to run it, and what to change to run it
   elsewhere.

### Using just the package

```r
library(cropCalendars)

# Per-pixel daily/monthly climate -> crop calendar -> phenological heat units.
# Feed your own climate series (tas, pr, and for FAO-56 PET also rsds, rlds,
# huss, sfcwind, ps) as plain R objects; no files or paths are required.
clim <- calcClimatology(lat = lat, temp = tas, prec = pr, syear = y0, eyear = y1, ...)
ccal <- calcCropCalendars(lon = lon, lat = lat, ...)
phu  <- calcPHU(sdate = ccal$sdate, hdate = ccal$hdate, ...)
```

`?calcCropCalendars`, `?calcClimatology`, `?calcPHU` document the expected inputs.

## Running the GGCMI / ISIMIP3b pipeline

Scripts live in `utils/ggcmi_ph3/`. All deployment-specific values (paths, SLURM
account, derived output trees) are centralized in **`settings.sh`** — edit that one file
when moving the pipeline; the `.sh` jobs `source` it and `00_config.R` parses it.

The pipeline is an **annual sliding-window** scheme: for each year T the calendar is
built from the preceding 30 years `[T-30, T-1]`, advanced one year at a time by a ring
buffer — smooth and rule-consistent without any output smoothing. Stages (run in order):

| Stage | Script | Does | Needs |
|---|---|---|---|
| config | `00_config.R` | reads `settings.sh`; `gcms`/`scenarios` matrix; tunables (`clm_avg_years`, `clm_emit_step`, `pet_method`, `smooth_window`, `phu_smooth_window`) | R + package |
| 1 | `01_compute_annual_calendars.{R,sh}` | one job per (GCM, scenario): stream climate once into a 30-yr ring → annual per-crop calendars (67420 GGCMI cells via `ggcmi_landcells.csv`). **Single-pass** form | R + package |
| 1a / 1b | `01a_climatology_annual.{R,sh}` + `01b_calendars_annual.{R,sh}` | **Split** form (preferred): 1a caches the per-year daily climatology; 1b runs `calcCropCalendars` on the cache via `mclapply`. Avoids the 64-worker fork OOM and re-runs in minutes on rule changes | R + package |
| 2 | `02_assemble_annual_ncdf.{R,sh}` | per (GCM, scen, crop, irri): GGCMI default-replacement → **publication-ready ISIMIP3b DRS NetCDF** (final names, 1601 time axis, ascending lat, fill values, chunking, publish path) | R + package, AgMIP ref. |
| 3 | `03_calc_phu_for_lpjml.{R,sh}` | PHUs for LPJmL → NetCDF (reads the DRS file) | R + package, `.clm` climate, **`lpjmlkit`** (LPJmL grid) |
| 4 | `04_write_lpjml_clm.{R,sh}` | write LPJmL `.clm` inputs (sdate, hdate, phu) as 30-band CLM binaries from the stage-02/03 products | R + package, **`lpjmlkit`** |

Stage 1 has two interchangeable forms: the single-pass `01_compute_annual_calendars`, or
the split `01a` (climatology cache) + `01b` (calendars) — the split is preferred for full
runs. There are no NCO/CDO post-processing stages — the DRS standardisation is done in the
stage-02 R write. (The deleted 10-year-step scripts of the previous scheme —
`02_generate_crop_cal_timeseries` and the old `04`–`07` NCO/CDO stages — are superseded by
this annual pipeline; do not confuse them with the current annual `01a`/`01b`.)

Climate files are **discovered** by globbing the official ISIMIP roots listed in
`settings.sh` `CLIMATE_DIR` (a colon-separated search list): ISIMIP3b
primary+secondary for the ESMs, ISIMIP3a obsclim/spinclim for the observational
forcings. The ensemble member, scenario tag and year range are read from the file
names — no hardcoded year/member tables.

The `.R` drivers run standalone (submitted with `sbatch` via the matching `.sh`):

```bash
cd utils/ggcmi_ph3
# Stage 1 — single-pass, or the split 01a + 01b (preferred for full runs):
Rscript --vanilla 01_compute_annual_calendars.R GFDL-ESM4 historical 8   # -> annual calendars (8 cores)
Rscript --vanilla 01a_climatology_annual.R      GFDL-ESM4 historical     # -> per-year climatology cache
Rscript --vanilla 01b_calendars_annual.R        GFDL-ESM4 historical 8   # -> annual calendars (from cache)
Rscript --vanilla 02_assemble_annual_ncdf.R     GFDL-ESM4 historical mai ir   # -> DRS NetCDF
Rscript --vanilla 03_calc_phu_for_lpjml.R       GFDL-ESM4 historical     # -> PHU NetCDF
Rscript --vanilla 04_write_lpjml_clm.R          GFDL-ESM4 historical     # -> LPJmL .clm inputs
# Run from this dir; work_dir = getwd().
```

Beyond the package, the pipeline R scripts also use: `abind`, `foreach`, `zoo`, and
`ncdf4`. Toolchain setup is centralized in **`env.sh`** (`load_r_env`: R + packages from
the PIK `piam` module set). To run elsewhere, edit that function body.

## Running outside the PIK cluster

The package layer needs nothing special. The **pipeline** assumes a PIK environment in a
few concrete places — to run it elsewhere, replace each:

- **Job scheduler.** The `*.sh` wrappers (stages `01`–`04`) submit with `sbatch`
  (`-A $ACCOUNT`, `--qos`, `--chdir`). Without SLURM, run the `.R` files directly with
  `Rscript` (see above), or adapt the wrappers to your scheduler.
- **Environment modules.** Toolchain loading lives in `env.sh` (`load_r_env`).
  Off-cluster, replace the function body with however you provide R + the packages
  (e.g. conda), or empty it if R is already on `PATH`. (No NCO/CDO needed any more —
  the DRS NetCDF is written directly in stage 02.)
- **Input datasets**, currently expected at the paths in `settings.sh` / `00_config.R`:
  - `CLIMATE_DIR` — colon-separated search list of climate roots (ISIMIP layout
    `<root>/<scenario>/<gcm>/*_<var>_global_daily_<y0>_<y1>.nc`): daily `tas`, `pr`,
    `rsds`, `rlds`, `huss`, `sfcwind`, `ps`. Discovered by globbing.
  - `ISIMIP3B_PATH` — ISIMIP3b `.clm` binary climate, read by `get.isimip.tas()` in the
    PHU stage. This reader assumes the LPJmL `.clm` format and file naming.
  - `AGMIP_DIR` — AgMIP reference crop calendars (NetCDF), used by stage 2.
  - `GRID_BIN` — LPJmL `grid.bin` defining the land cells, read **only by stage 03** (via
    `lpjmlkit::read_io`) to map the gridded product onto the LPJmL cell order for the `.clm`
    output. Stages 1a/1b/2 don't use it (they work on the climate land mask). Point it at your
    grid, or replace `grid_df` in 03 with your own `lon`/`lat` table.
- **Grid / resolution.** The 0.5° global grid (360 lat × 720 lon) is assumed by the
  NetCDF/CLM writers (stages 02/04). Change for a different grid.

In short: the **science** is portable (the package); the **GGCMI driver** is an
ISIMIP3b/PIK harness that you re-point via `settings.sh` plus the data-layout assumptions
listed above.

## Contact

- Sara Minoli (sara.minoli@pik-potsdam.de)

```
Potsdam Institute for Climate Impact Research (PIK)
Member of the Leibniz Association
14412, Potsdam, Germany
```

## References

```
@article{Waha2012,
  title={Climate-driven simulation of global crop sowing dates},
  author={Waha, K and Van Bussel, LGJ and M{\"u}ller, C and Bondeau, Alberte},
  journal={Global Ecology and Biogeography},
  volume={21},
  number={2},
  pages={247--259},
  year={2012},
  doi={https://doi.org/10.1111/j.1466-8238.2011.00678.x},
  publisher={Wiley Online Library}
}

@article{Minoli2019,
  title={Modelling cropping periods of grain crops at the global scale},
  author={Minoli, Sara and Egli, Dennis B and Rolinski, Susanne and M{\"u}ller, Christoph},
  journal={Global and Planetary Change},
  volume={174},
  pages={35--46},
  year={2019},
  doi={https://doi.org/10.1016/j.gloplacha.2018.12.013},
  publisher={Elsevier}
}
```