#' @title Calculate vector of possible harvest dates (Minoli et al., 2019)
#'
#' @description Harvest reasons are the events that can trigger the harvest
#' of a crop within an agro-climatic zone:
#' Earliest-maturing cultivar (GPmin);
#' Cultivar with longest grain filling (GPmaxrp);
#' Latest-maturing cultivar (GPmax);
#' Escape terminal water stress (w. lim);
#' Grain filling in warmest period (mid. t.);
#' Escape high temperature (high t.).
#'
#' The temperature- and water-driven harvest dates (\code{hd_wetseas},
#' \code{hd_temp_base}, \code{hd_temp_opt}) are evaluated on the \emph{daily}
#' climatology, consistent with \code{calcSowingDate}. Evaluating them on the
#' 12 monthly values instead quantises these dates to whole months, which makes
#' the resulting harvest dates flip by ~30 days between climate windows (e.g.
#' when the warmest month alternates between July and August).
#'
#' @param croppar data.frame with crop parematers as returned by getCropParam
#' @param sowing_date numeric value as day of the year (DOY). This can be either
#' caculated with calcSowingDate or prescribed.
#' @param sowing_season character value. Can be either "winter" or "spring". See
#' calcSowingDate.
#' @param monthly_temp numeric vector of length 12. Mean monthly air temperature
#' (degree Celsius).
#' @param monthly_ppet numeric vestor of length 12. Mean Potential Evapotranspiration (mm). See calcClimatology.
#' @param monthly_ppet_diff DEPRECATED and ignored. Was the monthly P/PET trend feeding the
#' retired \code{doy_wet2} trend wet-end estimate (see NEWS). Kept in the signature so existing
#' \code{calcCropCalendars} calls do not break.
#' @param daily_temp numeric vector of length 365. Climatological daily mean
#' temperature (deg C), one value per DOY (the \code{dtemp} element from
#' \code{calcClimatology}). If \code{NULL}, it is interpolated from
#' \code{monthly_temp}.
#' @param daily_prec numeric vector of length 365. Climatological daily
#' precipitation (mm) per DOY (the \code{dprec} element from
#' \code{calcClimatology}). Used with \code{daily_pet} to form the spike-free
#' daily P/PET (ratio of per-DOY means) for the wet-season-end crossing. If
#' \code{NULL}, the daily P/PET is interpolated from \code{monthly_ppet} instead.
#' @param daily_pet numeric vector of length 365. Climatological daily PET (mm)
#' per DOY (the \code{dpet} element from \code{calcClimatology}).
#' @param cross_min_duration integer minimum sustained-excursion length (days)
#' forwarded to \code{calcDoyCrossThreshold} for the hot-day temperature crossings
#' (default 1 = off). The wet-season-end LEVEL crossing (\code{doy_wet1})
#' is guarded by \code{cross_min_area} alone (its \code{min_duration} is retired, since a
#' deficit-days budget >= ~2 already implies multi-day persistence). See \code{?calcDoyCrossThreshold}.
#' @param cross_min_area Integrated-excursion (deficit-days) budget for the wet-season-end LEVEL
#' crossing (\code{doy_wet1}, \code{daily_ppet} vs \code{ppet_ratio}; also the tier-2 \code{ppet_min}
#' floor crossing) -- magnitude-weights persistence so a grazing P/PET crossing must persist far
#' longer to count. A length-2 \code{c(lo, hi)} HYSTERETIC (Schmitt) pair: a previously-FOUND wet-end
#' (\code{prev_wetend_found}) uses the lenient \code{lo}, a previously-ABSENT one the strict \code{hi},
#' so a dip whose area grazes the budget does not flip the wet-end's existence year to year. A scalar
#' means \code{lo == hi} (no hysteresis); default 0 = off. See \code{?calcDoyCrossThreshold}.
#' @param smooth_window Integer day-window for the daily-climatology smoothing -- the single
#' global window applied to BOTH the crossing inputs (the hot-day \code{daily_temp} crossing,
#' and \code{daily_prec} / \code{daily_pet} before the wet-season-end P/PET ratio) AND the
#' reductions (\code{warmest_day}, driest-window P/PET).
#' Default 31 (odd). See \code{calcCropCalendars}.
#' @param prev_rw1 Integer level wet-end EXISTENCE regime from last year (0 = absent-DRY,
#' 1 = found, 2 = absent-WET), or \code{NA}. Used only on the \code{cc_regime} path when
#' \code{harv_exist_eps > 0}.
#' @param harv_exist_eps Non-negative relative directional deadband (default 0 = off) on the
#' wet-end EXISTENCE boundary (the \code{ppet_ratio} level crossing, \code{doy_wet1}). Leaving
#' the absent-DRY regime needs the peak to clear \code{ppet_ratio * (1 + eps)}; leaving
#' absent-WET needs the trough below \code{ppet_ratio * (1 - eps)}. Experimental, gated by
#' \code{options(cc_regime = TRUE)}.
#' @param prev_wet_near Logical hysteresis state from last year (or \code{NA}): was the cell wet
#' near sowing? Drives the always-on WET-NEAR GATE -- the wet-season escape (and the no-wet-end
#' \code{hd_last}/\code{hd_first} decision) is taken only when the cell is wet within a hysteretic
#' window after the (temperature-set) sowing; otherwise it falls to the aridity-floor tier and, if
#' that also fails, to \code{hd_first}. Previously-wet widens the window (\code{options(cc_wet_window_hi)},
#' default 20), previously-dry narrows it (\code{cc_wet_window_lo}, default 10). The resolved value is
#' carried forward packed into \code{attr(., "harv_hi")} (12x bit).
#' @param prev_wetend_found Logical hysteresis state from last year (or \code{NA}): did a wet-end
#' (\code{doy_wet1}) exist? Selects the Schmitt bound of \code{cross_min_area} (found -> lenient
#' \code{lo}, absent -> strict \code{hi}). The resolved value is carried forward in the
#' \code{attr(., "harv_hi")} 6x bit. \code{NA} on the first year uses the strict bound.
#' @param temp_cross_min_area Integrated-excursion (degree-days) budget for the reproductive-phase
#' hot-day temperature crossing (\code{hd_temp_opt}: \code{daily_temp} vs \code{temp_opt_rphase}) --
#' the temperature analogue of \code{cross_min_area}, magnitude-weighting the crossing so a smoothed
#' temperature that merely grazes \code{temp_opt_rphase} must persist far longer to register a hot
#' season. Damps the \code{hd_temp_opt} EXISTENCE flicker in marginal cells whose warm plateau sits at
#' the threshold (e.g. subtropical winter wheat). A length-2 \code{c(lo, hi)} HYSTERETIC pair like
#' \code{cross_min_area}: a previously-FOUND crossing (\code{prev_topt_found}) uses the lenient
#' \code{lo}, a previously-ABSENT one the strict \code{hi}. Scalar = no hysteresis; default 0 = off.
#' @param prev_topt_found Logical hysteresis state from last year (or \code{NA}): did the reproductive
#' hot-day crossing (\code{doy_opt_rp}) exist? Selects the Schmitt bound of \code{temp_cross_min_area}.
#' Carried forward in the \code{attr(., "harv_hi")} 24x bit. \code{NA} on the first year = strict.
#' @param wet_near_min_area Non-negative integrated-excursion budget (default 0 = off) for the wet-near
#' gate: instead of \code{any(daily_ppet[window] >= threshold)} (a brittle single-day MAX that flips on
#' the P/PET peak grazing the threshold), require \code{sum(daily_ppet[window] - threshold)} over the
#' above-threshold days to reach the budget -- so a thin touch (area ~0) reads as NOT wet. A length-2
#' \code{c(lo, hi)} HYSTERETIC pair like \code{cross_min_area}: a previously-wet cell stays wet at the
#' lenient \code{lo}, a previously-dry one needs the strict \code{hi}. Reuses \code{prev_wet_near} (no
#' extra state). Chiefly affects Rice (\code{ppet_ratio = 1.0}, sown on the descending limb of a
#' winter-wet climate so the window straddles the 1.0 crossing).
#' @param prev_floor_found Logical hysteresis state from last year (or \code{NA}): did the TIER-2
#' \code{ppet_min} floor dry-down (\code{floor_end}) exist? Selects the Schmitt bound of
#' \code{cross_min_area} for the floor crossing (the full parallel of \code{prev_wetend_found} for the
#' \code{ppet_ratio} crossing). Carried forward in the \code{attr(., "harv_hi")} 48x bit. Only the floor
#' tier (Rice, \code{ppet_min < ppet_ratio}) exercises it. \code{NA} on the first year = strict.
#'
#' @seealso getCropParam, calcClimatology, calcSowingDate, calcCropCalendars
#' @export
calcHarvestDateVector <- function(croppar,
                                  sowing_date,
                                  sowing_season,
                                  monthly_temp,
                                  monthly_ppet,
                                  monthly_ppet_diff,
                                  daily_temp = NULL,
                                  daily_prec = NULL,
                                  daily_pet  = NULL,
                                  cross_min_duration = 1L,
                                  cross_min_area = 0,
                                  smooth_window = 31L,
                                  prev_rw1 = NA_integer_,
                                  harv_exist_eps = 0,
                                  prev_wet_near = NA,
                                  prev_wetend_found = NA,
                                  temp_cross_min_area = 0,
                                  prev_topt_found = NA,
                                  wet_near_min_area = 0,
                                  prev_floor_found = NA
                                  ) {

  # Extract individual parameter names and values
  list2env(croppar, environment())  # 1-row data.frame: columns -> scalar params

  ndays_year <- 365

  # Daily climatologies (one value per DOY 1:365). dtemp/dprec/dpet from the cache
  # are already on this grid; without a daily temperature series, interpolate the
  # monthly means onto the grid for the hot-day crossings (which have no pure-monthly
  # form). have_dtemp also selects the legacy mid-day for warmest_day below.
  have_dtemp <- !is.null(daily_temp)
  if (!have_dtemp) daily_temp <- .monthlyToDoy365(monthly_temp)
  # Spike-free daily P/PET for the wet-season-end crossing: the ratio of the
  # per-DOY mean P and mean PET (never blows up — mean PET on a DOY is never ~0),
  # unlike the mean of daily P/PET ratios. This feeds a threshold CROSSING, so P and
  # PET are smoothed (smooth_window) BEFORE the ratio is formed -- the daily
  # climatology itself is kept raw, and only the crossing input is conditioned here
  # (0 = no-op). Fall back to the interpolated monthly ratio if daily P/PET absent.
  if (!is.null(daily_prec) && !is.null(daily_pet)) {
    daily_ppet <- .circRoll(daily_prec, smooth_window, "center", mean = TRUE) /
                  pmax(.circRoll(daily_pet, smooth_window, "center", mean = TRUE), 1e-6)
  } else {
    daily_ppet <- .monthlyToDoy365(monthly_ppet)
  }
  # NB: the wet-season-end crossing EXISTENCE is deliberately NOT deadbanded by nudging ppet_ratio. A
  # one-directional level nudge cannot keep a band-membership condition (min(daily_ppet) < ppet_ratio <
  # max) sticky -- raising ppet_ratio to keep a found crossing instead eliminates it whenever the wet
  # peak sits just above ppet_ratio, forcing a found/not-found 2-cycle. Existence is instead stabilised
  # by the cross_min_area deficit-days Schmitt below (nudging the area, not the level).

  # Shortest cycle: crop lower biological limit
  hd_first <- sowing_date + min_growingseason
  # Medium cycle: best trade-off vegetative and reproductive growth
  hd_maxrp <- sowing_date + maxrp_growingseason
  # Longest cycle: crop upper biological limit
  hd_last <- ifelse(sowing_season == "winter",
                    sowing_date + max_growingseason_wt,
                    sowing_date + max_growingseason_st)

  # End of wet season ----
  # Anchor for the harvest threshold-crossing scans (the wet-end P/PET crossings here and the
  # hot-day temperature crossings below). The Jan-1 default (from = 1L) returns the EARLIEST-DOY
  # crossing, so a marginal year whose signal grazes the threshold mid-season registers a spurious
  # early crossing that PRE-EMPTS the genuine one. Anchoring the scan at sowing returns the first
  # crossing AFTER sowing -- the physically meaningful one -- mirroring the warmest/coldest-day anchors
  # already used for the sowing crossings. This is a general fix, always on (the Jan-1 scan is a bug).
  sow_anchor <- as.integer(sowing_date)
  # HYSTERETIC deficit-days budget for the wet-end LEVEL crossing (doy_wet1). cross_min_area is the
  # Schmitt pair c(lo, hi) (a scalar means lo == hi, no hysteresis): a previously-FOUND wet-end keeps
  # the lenient bound area_lo (stay found unless the dip's integrated deficit drops below it), a
  # previously-ABSENT one needs the strict bound area_hi to newly qualify. This damps the existence
  # flicker when the dip's area grazes the budget year to year. Nudging the AREA is monotonic in dip
  # depth/duration, so -- unlike nudging the threshold LEVEL -- it cannot destroy the crossing it tries
  # to keep. min_duration is not used here: min_area >= ~2 already implies multi-day persistence (a
  # 1-2 day dip cannot accumulate that deficit at realistic P/PET). (The hot-day temperature crossings
  # have no area analogue and keep cross_min_duration.)
  area_lo <- cross_min_area[1L]
  area_hi <- cross_min_area[length(cross_min_area)]
  min_area_eff <- if (isTRUE(prev_wetend_found)) area_lo else area_hi   # NA / FALSE -> strict (hi)
  doy_wet1 <- calcDoyCrossThreshold(
    daily_ppet,
    ppet_ratio,
    min_duration = 1L,
    from = sow_anchor,
    min_area = min_area_eff
    )[["doy_cross_down"]]
  # Wet-season-end estimate (P/PET level crossing doy_wet1); the crop escapes terminal water
  # stress at it, mapped to forward-from-sowing and clamped to [hd_first, hd_last] (see the
  # wetseas_escape branch). The escape uses this single level crossing only -- an earlier trend-based
  # candidate (doy_wet2) that could pull the escape earlier was dropped as the noisier of the two
  # series. (cc_regime path may re-detect doy_wet1 at a deadbanded threshold first.)
  # Escape harvest for a FOUND wet-end. Each wet-end is placed by its CIRCULAR forward distance from
  # sowing, fwd = (wet_end - sowing_date) %% 365 (days AFTER sowing), and the escape harvest is
  # sowing + fwd + rphase, clamped to [hd_first, hd_last] IN PLACE: the lower clamp sends a
  # sub-minimum escape to hd_first continuously, the upper clamp holds a late wet-end (large fwd) at
  # hd_last. This is exactly the reference (master) wrap rule -- master adds 365 to any wet-end <
  # sowing_date, algebraically identical to mapping it to forward-from-sowing (sowing + fwd); the
  # in-place clamp reproduces master's downstream max(hd_first,.)/min(.,hd_last).
  wetseas_escape <- function(wet_ends) {
    fwd <- (wet_ends - sowing_date) %% ndays_year
    esc <- pmin(hd_last, pmax(hd_first, sowing_date + fwd + rphase_duration))
    min(esc)
  }
  # WET-NEAR GATE (always on): are we in, or imminently entering, an active wet season? TRUE iff any
  # DOY in [sowing, sowing + w_eff] is at/above the threshold. The sowing date is temperature-set in
  # TEMPPREC cells (cold-winter monsoon) and the monsoon can open a week or two AFTER the thermal
  # spring, so a strict at-sowing test reads "dry" and collapses a real season to hd_first; the window
  # admits an imminent onset. Because the onset lag grazes the window year to year, the window is
  # HYSTERETIC (Schmitt): previously-wet uses the generous bound w_hi, previously-dry the strict bound
  # w_lo; an onset lag in (w_lo, w_hi] keeps last year's verdict. Bounds via options (default 10/20).
  # Resolved wet_near is carried forward (harv_hi 12x bit). The gate also SELF-HEALS a wet-end (or
  # onset) oscillating around sowing: an end just AFTER sowing -> wet at sowing -> wet_near -> tier 1 ->
  # tiny fwd -> hd_first; an end just BEFORE sowing -> dry, no re-onset in the window -> !wet_near ->
  # hd_first. Both land on hd_first, so the sowing/harvest mismatch can't flip hd_last<->hd_first.
  w_lo <- as.integer(getOption("cc_wet_window_lo", 10L))
  w_hi <- as.integer(getOption("cc_wet_window_hi", 20L))
  # The wet-near gate's window WIDTH (w_lo/w_hi) deadbands the onset TIMING -- a wet day grazing the edge
  # of the window. But the gate's verdict, `any(daily_ppet[window] >= threshold)`, is a single-day MAX:
  # it flips on the post-sowing P/PET PEAK grazing the threshold (chiefly Rice, ppet_ratio = 1.0, sown on
  # the descending limb of a winter-wet climate so the window straddles the 1.0 crossing). wet_near_min_area
  # replaces that brittle peak with an INTEGRATED test: the window's excursion above the threshold must
  # accumulate sum(ppet - threshold) >= a budget, so a thin touch (peak just clears the line for a day or
  # two -> area ~0) reads as NOT wet, while a real wet season qualifies. HYSTERETIC (Schmitt) like
  # cross_min_area: a previously-wet cell stays wet at the lenient lo budget, a previously-dry one needs
  # the strict hi to become wet. wet_near_min_area = 0 (default) -> the original single-day max test.
  wn_area_eff <- if (isTRUE(prev_wet_near)) wet_near_min_area[1L]
                 else                       wet_near_min_area[length(wet_near_min_area)]   # NA/FALSE -> strict hi
  # The window WIDTH and the area budget are COMPLEMENTARY, not redundant: the width hysteresis
  # deadbands the onset TIMING (a wet day grazing the edge of the window, as in a lagged monsoon
  # onset), while the area budget deadbands the wet-season MAGNITUDE (a thin P/PET touch vs a real
  # season). Both axes are kept.
  wetnear_test <- function(threshold) {
    w_eff <- if (isTRUE(prev_wet_near)) w_hi else w_lo                         # NA / FALSE -> strict
    widx  <- ((as.integer(sowing_date) - 1L + 0:w_eff) %% ndays_year) + 1L
    if (wn_area_eff <= 0) any(daily_ppet[widx] >= threshold)                   # off: original peak test
    else sum(pmax(daily_ppet[widx] - threshold, 0)) >= wn_area_eff             # integrated-area gate
  }
  wet_near <- wetnear_test(ppet_ratio)

  # TWO-TIER wet-season decision, organised by STATE AT SOWING (not by wet-end found / not found):
  #   TIER 1 -- wet_near at ppet_ratio: we are in an active wet season. The wet-end is the first
  #     persistent down-crossing after sowing (doy_wet1 level crossing) -> escape, clamped.
  #     If NO wet-end is found the season never ends -> hd_last (the "always-wet" case).
  #   TIER 2 -- else (dry at sowing, onset too far): fall back to the aridity FLOOR ppet_min and run
  #     the SAME machinery one threshold lower -- wet near sowing at the floor AND no persistent floor
  #     dry-down -> still wet enough, hd_last; otherwise truly arid -> hd_first. For the 6 crops with
  #     ppet_min == ppet_ratio this is a no-op (the floor test is the failed gate again -> hd_first);
  #     only Rice (ppet_ratio = 1.0, ppet_min = 0.5) exercises it, reproducing the old always-wet
  #     outcome (above floor, no floor dry-down -> hd_last), now persistence-guarded (cross_min_area).
  # floor_found: did the TIER-2 ppet_min dry-down (floor_end) exist? Captured for next year's Schmitt
  # (prev_floor_found). NA when the floor branch is not exercised (TIER 1, or ppet_min == ppet_ratio).
  floor_found <- NA
  floor_area_eff <- if (isTRUE(prev_floor_found)) area_lo else area_hi   # NA/FALSE -> strict hi
  decide_wetseas <- function(doy_wet1) {
    if (wet_near) {
      if (doy_wet1 == -9999) hd_last else wetseas_escape(doy_wet1)
    } else if (ppet_min < ppet_ratio) {
      # FULL PARALLEL with the ppet_ratio doy_wet1 crossing: the floor dry-down gets the SAME hysteretic
      # cross_min_area Schmitt (prev_floor_found -> lenient lo, else strict hi). A grazing floor dry-down
      # (area near the budget) then cannot flip found<->absent year to year -- which would toggle
      # hd_last<->hd_first (-> first<->topt selection) in tier-2 Rice cells.
      floor_end <- calcDoyCrossThreshold(daily_ppet, ppet_min,
                     min_duration = 1L, from = sow_anchor,
                     min_area = floor_area_eff)[["doy_cross_down"]]
      floor_found <<- (floor_end != -9999)
      if (wetnear_test(ppet_min) && floor_end == -9999) hd_last else hd_first
    } else {
      hd_first
    }
  }

  if (isTRUE(getOption("cc_regime", FALSE))) {
    # EXPERIMENTAL directional Schmitt deadband (harv_exist_eps) on the wet-end EXISTENCE: re-detect
    # doy_wet1 at a regime-shifted threshold so a grazing signal must move decisively to flip the
    # crossing's existence (without a single nudged threshold that would destroy the crossing it tries
    # to keep). The two-tier decide_wetseas (wet_near gate + ppet_min floor fallback) is unchanged;
    # rw1 (0 absent-DRY / 1 found / 2 absent-WET) is carried forward for diagnostics.
    eps <- harv_exist_eps
    thr1 <- if (is.na(prev_rw1) || eps == 0) ppet_ratio
            else if (prev_rw1 == 0L) ppet_ratio * (1 + eps)
            else if (prev_rw1 == 2L) ppet_ratio * (1 - eps)
            else                     ppet_ratio
    doy_wet1 <- calcDoyCrossThreshold(daily_ppet, thr1,
                                      min_duration = 1L,
                                      from = sow_anchor,
                                      min_area = min_area_eff)[["doy_cross_down"]]
    rw1 <- if (max(daily_ppet) < thr1) 0L else if (min(daily_ppet) >= thr1) 2L else 1L
    hd_wetseas <- decide_wetseas(doy_wet1)   # sets floor_found via <<-
    harv_hi <- rw1 + 6L * as.integer(doy_wet1 != -9999) + 12L * as.integer(wet_near) +
               48L * as.integer(isTRUE(floor_found))
  } else {
    hd_wetseas <- decide_wetseas(doy_wet1)   # sets floor_found via <<-
    harv_hi <- 6L * as.integer(doy_wet1 != -9999) + 12L * as.integer(wet_near) +
               48L * as.integer(isTRUE(floor_found))
  }

  # Warmest period of the year ----
  # Centre DOY of the warmest 30-day window of the daily climatology (the daily
  # analogue of the previous "mid-day of the warmest month"); the legacy monthly
  # fallback is exactly that mid-day when the daily series is absent.
  warmest_day <- if (have_dtemp) .doyWarmestWindow(daily_temp, width = smooth_window) else
    c(15, 43, 74, 104, 135, 165, 196, 227, 257, 288, 318, 349)[which.max(monthly_temp)]
  hd_temp_base <- ifelse(
    sowing_season == "winter", warmest_day, warmest_day + rphase_duration
    )

  # Smoothed daily temperature for the hot-day threshold crossings, using the same global
  # smooth_window as warmest_day and all other reductions.
  daily_temp_x <- .circRoll(daily_temp, smooth_window, "center", mean = TRUE)

  # HYSTERETIC degree-days budget for the reproductive hot-day crossing (hd_temp_opt), the temperature
  # analogue of cross_min_area: a previously-FOUND crossing uses the lenient lo, a previously-ABSENT one
  # the strict hi, so a smoothed temperature that merely grazes temp_opt_rphase (subtropical warm plateau
  # sitting at the threshold) cannot flip the hot-season's existence year to year. temp_cross_min_area = 0
  # (default) leaves it off (the crossing falls back to the min_duration guard alone).
  topt_area_lo  <- temp_cross_min_area[1L]
  topt_area_hi  <- temp_cross_min_area[length(temp_cross_min_area)]
  topt_area_eff <- if (isTRUE(prev_topt_found)) topt_area_lo else topt_area_hi   # NA / FALSE -> strict (hi)

  # First hot day ---- (anchored at sowing via sow_anchor, like the wet-end crossings above, so
  # a spurious early-DOY hot-day crossing cannot pre-empt the genuine first-after-sowing one.
  # The +365 / sort below still renumbers a wrapped crossing onto the sowing..sowing+365 axis
  # BEFORE rphase is added -- not redundant with the downstream <sowing wrap -- and is a no-op
  # when sow_anchor already returns a crossing >= sowing.)
  doy_exceed_opt_rp <- calcDoyCrossThreshold(
    daily_temp_x, temp_opt_rphase, min_duration = cross_min_duration,
    from = sow_anchor, min_area = topt_area_eff
    )[["doy_cross_up"]]
  idx <- which(doy_exceed_opt_rp < sowing_date & doy_exceed_opt_rp != -9999)
  doy_exceed_opt_rp[idx] <- doy_exceed_opt_rp[idx] + ndays_year
  doy_exceed_opt_rp      <- sort(doy_exceed_opt_rp)[1]

  # Last hot day ----
  doy_below_opt_rp <- calcDoyCrossThreshold(
    daily_temp_x,
    temp_opt_rphase,
    min_duration = cross_min_duration,
    from = sow_anchor, min_area = topt_area_eff
    )[["doy_cross_down"]]
  idx <- which(doy_below_opt_rp < sowing_date & doy_below_opt_rp != -9999)
  doy_below_opt_rp[idx] <- doy_below_opt_rp[idx] + ndays_year
  doy_below_opt_rp      <- sort(doy_below_opt_rp)[1]

  # Winter type: First hot day; Spring type: Last hot day
  doy_opt_rp  <- ifelse(
    sowing_season == "winter", doy_exceed_opt_rp, doy_below_opt_rp
    )
  if (doy_opt_rp == -9999) {
    hd_temp_opt <- hd_maxrp
  } else {
    hd_temp_opt <- ifelse(
      sowing_season == "winter", doy_opt_rp, doy_opt_rp+rphase_duration
      )
  }
  # Carry the reproductive hot-day EXISTENCE for next year's temp_cross_min_area Schmitt: the 24x bit
  # of harv_hi (rw1 + 6*wetend_found + 12*wet_near + 24*topt_found).
  harv_hi <- harv_hi + 24L * as.integer(doy_opt_rp != -9999)

  # If harvest date < sowing date, it occurs the following year, so add 365 days
  hd_wetseas    <- ifelse(
    hd_wetseas < sowing_date, hd_wetseas + ndays_year, hd_wetseas
    )
  hd_temp_base  <- ifelse(
    hd_temp_base < sowing_date, hd_temp_base + ndays_year, hd_temp_base
    )
  hd_temp_opt   <- ifelse(
    hd_temp_opt < sowing_date, hd_temp_opt + ndays_year, hd_temp_opt
    )

  # hd_vector ----
  hd_vector        <- c(hd_first, hd_maxrp, hd_last,
                        hd_wetseas, hd_temp_base, hd_temp_opt)
  names(hd_vector) <- c("hd_first", "hd_maxrp", "hd_last",
                        "hd_wetseas", "hd_temp_base", "hd_temp_opt")

  # Hysteresis state carried forward by calcCropCalendars: the high part of the packed harvest state.
  # 48*floor_found (did the TIER-2 ppet_min floor dry-down exist, carried as prev_floor_found for the
  # floor cross_min_area Schmitt) + 24*topt_found (did the reproductive hot-day crossing exist, carried
  # as prev_topt_found for the temp_cross_min_area Schmitt) + 12*wet_near (the always-on wet-near gate,
  # carried as prev_wet_near for the Schmitt window) + 6*wetend_found (did doy_wet1 exist, carried as
  # prev_wetend_found for the cross_min_area Schmitt) + rw1 (0/1/2 DRY/NORMAL/WET, cc_regime path only).
  attr(hd_vector, "harv_hi") <- harv_hi
  return(hd_vector)
}

# Map 12 monthly values (referring to month mid-days) to a clean 365-element
# vector indexed by DOY 1:365, via linear interpolation with Dec->Jan wrap.
.monthlyToDoy365 <- function(monthly_value) {
  d   <- interpolateMonthlyToDaily(monthly_value)
  doy <- ((d[["x"]] - 1L) %% 365L) + 1L
  # interpolateMonthlyToDaily covers each DOY (often twice, from the two-year
  # replication); average duplicates and order by DOY.
  as.numeric(tapply(d[["y"]], doy, mean)[as.character(1:365)])
}

# Centre DOY of the warm season (warmth-weighted circular centroid). The exact MIRROR of
# .doyColdestWindow; both delegate to .doyCircularCentroid (zz_daily_reductions.R), which
# carries the shared DOY-mapping convention AND the degenerate/multi-modal argmax fallback.
.doyWarmestWindow <- function(daily_value, width = 30) .doyCircularCentroid(daily_value, width, cold = FALSE)
