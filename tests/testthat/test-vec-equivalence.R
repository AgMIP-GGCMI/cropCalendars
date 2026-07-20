# Equivalence harness for the scalar->vectorised consolidation (PR-A, and the basis for
# PR-C). Each public scalar function (calcVd, isWinterCrop, ...) now delegates to its
# vectorised core; these tests embed the ORIGINAL scalar body verbatim as ground truth and
# fuzz a wide input range to assert the wrapper reproduces it bit-for-bit at the production
# defaults. When PR-C adds calcVrf / calcPHU wrappers, extend this file the same way.

# ---- ground-truth reference implementations (the pre-refactor scalar bodies) ----

ref_calcVd <- function(temp_mean_month, max.vern.days = 70, max.vern.months = 5,
                       tv2 = 3, tv3 = 10) {
  max.vern.days.month <- max.vern.days / max.vern.months
  temp <- array(0, c(12, 2)); temp[, 1] <- temp_mean_month; temp[, 2] <- 1:12
  sort_temp      <- temp[order(temp[, 1]), ]
  coldest_months <- sort_temp[1:5, 2]            # original always takes the 5 coldest
  days_required  <- 0
  for (m in 1:max.vern.months) {
    t <- temp_mean_month[coldest_months[m]]
    if      (t <= tv2) days <- max.vern.days.month
    else if (t >= tv3) days <- 0
    else               days <- max.vern.days.month * (1 - (t - tv2) / (tv3 - tv2))
    days_required <- days_required + days
  }
  round(days_required)
}

ref_isWinterCrop <- function(start, end, tcm, lat) {
  growp <- ifelse(start <= end, end - start, 365 + end - start)
  wc <- 0
  if (!is.na(start) && start > 0 && !is.na(lat) && !is.na(tcm)) {
    if (lat > 0) {
      if (((start + growp > 365) && (growp >= 150)) && (tcm >= -10 && tcm <= 7)) wc <- 1
    } else {
      if (((start < 182) && (start + growp > 182) && (growp >= 150)) &&
          (tcm >= -10 && tcm <= 7)) wc <- 1
    }
  }
  wc
}

# ---- calcVd ----

test_that("calcVd wrapper reproduces the original scalar (production max.vern.months = 5)", {
  set.seed(20260617)
  for (i in 1:2000) {
    tmm <- runif(12, -25, 35)
    mvd <- sample(40:90, 1)
    tv2 <- sample(0:5, 1); tv3 <- tv2 + sample(3:10, 1)
    expect_equal(
      cropCalendars::calcVd(tmm, max.vern.days = mvd, max.vern.months = 5,
                            tv2 = tv2, tv3 = tv3),
      ref_calcVd(tmm, max.vern.days = mvd, max.vern.months = 5, tv2 = tv2, tv3 = tv3)
    )
  }
})

test_that("calcVd wrapper matches the original for max.vern.months in {1,3,4,5}", {
  # The original picks the 5 coldest months but loops max.vern.months, so it is only
  # self-consistent for max.vern.months <= 5; the vec keeps exactly max.vern.months.
  set.seed(11)
  for (mvm in c(1L, 3L, 4L, 5L)) for (i in 1:300) {
    tmm <- runif(12, -25, 35)
    expect_equal(
      cropCalendars::calcVd(tmm, max.vern.days = 70, max.vern.months = mvm, tv2 = 3, tv3 = 10),
      ref_calcVd(tmm, max.vern.days = 70, max.vern.months = mvm, tv2 = 3, tv3 = 10)
    )
  }
})

test_that("calcVd boundary temperatures (exactly tv2 / tv3) match", {
  tmm <- c(rep(3, 5), rep(20, 7))           # five months exactly at tv2
  expect_equal(cropCalendars::calcVd(tmm, 70, 5, 3, 10), ref_calcVd(tmm, 70, 5, 3, 10))
  tmm <- c(rep(10, 5), rep(20, 7))          # five months exactly at tv3
  expect_equal(cropCalendars::calcVd(tmm, 70, 5, 3, 10), ref_calcVd(tmm, 70, 5, 3, 10))
})

# ---- isWinterCrop ----

test_that("isWinterCrop wrapper reproduces the original scalar over valid inputs", {
  set.seed(99)
  for (i in 1:4000) {
    start <- sample(1:365, 1); end <- sample(1:365, 1)
    tcm   <- runif(1, -25, 25); lat <- runif(1, -60, 60)
    expect_equal(
      as.numeric(cropCalendars::isWinterCrop(start, end, tcm, lat)),
      ref_isWinterCrop(start, end, tcm, lat)
    )
  }
})

test_that("isWinterCrop wrapper matches near the rule boundaries", {
  cases <- expand.grid(
    start = c(1, 181, 182, 183, 215, 366),   # SH mid-winter pivot at 182
    growp = c(149, 150, 151),                # length threshold
    tcm   = c(-11, -10, 7, 8),               # cold-but-not-killing band edges
    lat   = c(-1, 1))                        # hemisphere
  for (r in seq_len(nrow(cases))) {
    s <- cases$start[r]; e <- ((s + cases$growp[r] - 1) %% 365) + 1
    expect_equal(
      as.numeric(cropCalendars::isWinterCrop(s, e, cases$tcm[r], cases$lat[r])),
      ref_isWinterCrop(s, e, cases$tcm[r], cases$lat[r]),
      info = sprintf("start=%d growp=%d tcm=%g lat=%g", s, cases$growp[r], cases$tcm[r], cases$lat[r])
    )
  }
})

test_that("isWinterCrop guard cases return 0 (NA/0 start, NA lat, NA tcm)", {
  expect_equal(as.numeric(cropCalendars::isWinterCrop(NA, 200, 0, 45)), 0)
  expect_equal(as.numeric(cropCalendars::isWinterCrop(0,  200, 0, 45)), 0)
  expect_equal(as.numeric(cropCalendars::isWinterCrop(100, 300, NA, 45)), 0)
  expect_equal(as.numeric(cropCalendars::isWinterCrop(100, 300, 0, NA)), 0)
})

test_that("isWinterCrop is graceful on NA end (returns NA, the old scalar errored)", {
  expect_true(is.na(cropCalendars::isWinterCrop(100, NA, 0, 45)))
})

# ============================ PR-C: calcPHU / calcVrf ============================

ref_calcPHU <- function(sdate, hdate, mdt, vern_factor = rep(1, 365), basetemp = 0,
                        phen_model = "t") {
  husum <- 0
  if (is.na(sdate) | is.na(hdate) | sdate == 0 | hdate == 0) return(husum)
  hdate <- ifelse(sdate < hdate, hdate, hdate + 365)
  if (hdate <= 365) days_no_gp <- c(1:(sdate - 1), hdate:365)
  if (hdate >  365) days_no_gp <- c((hdate - 365):(sdate - 1))
  if (phen_model == "t") {
    teff <- mdt - basetemp; teff[teff < 0] <- 0; teff[days_no_gp] <- 0
    as.integer(sum(teff))
  } else {
    teff <- mdt - basetemp; teff[teff < 0] <- 0; teff <- teff * vern_factor
    teff[days_no_gp] <- 0
    as.integer(sum(teff)) * (-1L)
  }
}

ref_calcVrf <- function(sdate, hdate, mdt, vd, vd_b = 0.2,
                        tv1 = -4, tv2 = 3, tv3 = 10, tv4 = 17) {
  veff <- array(0, 365); vrf <- array(1.0, 365)
  for (k in 1:365) {
    if      (mdt[k] >= tv1 && mdt[k] <  tv2) veff[k] <- (mdt[k] - tv1) / (tv2 - tv1)
    else if (mdt[k] >= tv2 && mdt[k] <= tv3) veff[k] <- 1
    else if (mdt[k] >  tv3 && mdt[k] <  tv4) veff[k] <- (tv4 - mdt[k]) / (tv4 - tv3)
    else if (mdt[k] >= tv4)                  veff[k] <- 0
    else if (mdt[k] <  tv1)                  veff[k] <- 0
  }
  veff[veff > 1] <- 1; veff[veff < 0] <- 0; veff <- c(veff, veff)
  vdsum <- 0; k <- sdate; hd <- ifelse(sdate < hdate, hdate, hdate + 365)
  while (vdsum < vd && k < hd) { vdsum <- vdsum + veff[k]; if (vdsum < vd) k <- k + 1 }
  endday <- if (vdsum >= vd) k else Inf
  vdsum <- 0
  for (k in sdate:min(endday, hd)) {
    vdsum <- vdsum + veff[k]
    if (vdsum < (vd * vd_b)) vrf[ifelse(k > 365, k - 365, k)] <- 0
    else vrf[ifelse(k > 365, k - 365, k)] <- max(0, min(1, (vdsum - vd * vd_b) / (vd - vd * vd_b)))
  }
  if (vd == 0) vrf <- rep(1, 365)
  vrf
}

# ---- calcPHU ----

test_that("calcPHU wrapper reproduces the original scalar (sdate != hdate, sdate,hdate > 1)", {
  set.seed(424242)
  for (i in 1:3000) {
    sdate <- sample(2:365, 1); hdate <- sample(2:365, 1)
    if (sdate == hdate) next
    mdt <- runif(365, -20, 35); bt <- sample(0:8, 1); vrf <- runif(365, 0, 1)
    expect_equal(cropCalendars::calcPHU(sdate, hdate, mdt, basetemp = bt, phen_model = "t"),
                 ref_calcPHU(sdate, hdate, mdt, basetemp = bt, phen_model = "t"))
    expect_equal(cropCalendars::calcPHU(sdate, hdate, mdt, vern_factor = vrf, basetemp = bt,
                                        phen_model = "tv"),
                 ref_calcPHU(sdate, hdate, mdt, vern_factor = vrf, basetemp = bt, phen_model = "tv"))
  }
})

test_that("calcPHU corrects the two scalar edge bugs (matches the production vec core)", {
  mdt <- rep(20, 365); bt <- 0    # every day warm, so a dropped day visibly changes the sum
  # sdate == 1: old scalar dropped day 1; wrapper (= vec) includes it.
  expect_equal(cropCalendars::calcPHU(1, 200, mdt, basetemp = bt),
               .calc_phu_thermal_vec(1, 200, matrix(mdt, 1), bt))
  expect_false(isTRUE(cropCalendars::calcPHU(1, 200, mdt, basetemp = bt) ==
                      ref_calcPHU(1, 200, mdt, basetemp = bt)))
  # sdate == hdate: wrapper (= vec) treats it as a full-year growing window.
  expect_equal(cropCalendars::calcPHU(100, 100, mdt, basetemp = bt),
               .calc_phu_thermal_vec(100, 100, matrix(mdt, 1), bt))
  expect_false(isTRUE(cropCalendars::calcPHU(100, 100, mdt, basetemp = bt) ==
                      ref_calcPHU(100, 100, mdt, basetemp = bt)))
})

test_that("calcPHU guards: NA / 0 sdate or hdate return 0; bad phen_model errors", {
  expect_equal(cropCalendars::calcPHU(NA, 200, rep(10, 365)), 0L)
  expect_equal(cropCalendars::calcPHU(100, 0, rep(10, 365)), 0L)
  expect_error(cropCalendars::calcPHU(100, 200, rep(10, 365), phen_model = "tp"))
})

# ---- calcVrf ----

test_that("calcVrf wrapper reproduces the original scalar over all inputs (incl. edges)", {
  set.seed(2024)
  for (i in 1:3000) {
    sdate <- sample(1:365, 1); hdate <- sample(1:365, 1)
    mdt <- runif(365, -20, 35); vd <- sample(0:70, 1)
    # as.numeric strips attributes: the wrapper returns a plain vector while the old scalar
    # returned a 1-D array (array() carries a dim attribute). Values agree to ~1e-14 (the
    # cumsum core reorders the summation), well within the default tolerance.
    expect_equal(as.numeric(cropCalendars::calcVrf(sdate, hdate, mdt, vd)),
                 as.numeric(ref_calcVrf(sdate, hdate, mdt, vd)))
  }
})

test_that("calcVrf with vd = 0 returns all ones", {
  expect_equal(cropCalendars::calcVrf(100, 300, runif(365, -10, 20), vd = 0), rep(1, 365))
})

# ============================ PR-B: .monthlyFromDaily (vector + matrix) ============================

# Original .monthly_temps_vec body (matrix path ground truth, now removed from the package).
ref_monthly_temps_vec <- function(temp_mat) {
  sday <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  eday <- c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
  m_mat <- matrix(0.0, nrow(temp_mat), 12L)
  for (m in 1:12) m_mat[, m] <- rowMeans(temp_mat[, sday[m]:eday[m]])
  m_mat
}
# Original vector .monthlyFromDaily body (per-pixel path ground truth).
ref_mfd_vec <- function(daily, agg = c("mean", "sum")) {
  agg   <- match.arg(agg)
  ndays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  end   <- cumsum(ndays); start <- end - ndays + 1L
  f     <- if (agg == "mean") mean else sum
  vapply(seq_len(12L), function(m) f(daily[start[m]:end[m]]), numeric(1))
}

test_that(".monthlyFromDaily vector path is unchanged (mean and sum)", {
  set.seed(808)
  for (i in 1:500) {
    v <- runif(365, -15, 35); p <- rgamma(365, 1, 0.2)
    expect_equal(.monthlyFromDaily(v, "mean"), ref_mfd_vec(v, "mean"))
    expect_equal(.monthlyFromDaily(p, "sum"),  ref_mfd_vec(p, "sum"))
  }
  expect_null(dim(.monthlyFromDaily(runif(365), "mean")))   # vector in -> vector out
})

test_that(".monthlyFromDaily matrix path reproduces the removed .monthly_temps_vec", {
  set.seed(909)
  for (i in 1:200) {
    M <- matrix(runif(sample(2:40, 1) * 365, -20, 35), ncol = 365)  # ref needs >=2 rows
    expect_equal(.monthlyFromDaily(M, "mean"), ref_monthly_temps_vec(M))
  }
  # 1-row matrix: ref errors (original lacked drop=FALSE); the generalized helper handles it
  # and keeps the [1 x 12] shape. Check against a direct row-wise mean.
  M1  <- matrix(runif(365, -20, 35), 1, 365)
  end <- cumsum(c(31,28,31,30,31,30,31,31,30,31,30,31)); start <- end - c(31,28,31,30,31,30,31,31,30,31,30,31) + 1L
  exp1 <- matrix(vapply(seq_len(12L), function(j) mean(M1[1, start[j]:end[j]]), numeric(1)), 1, 12)
  expect_equal(.monthlyFromDaily(M1, "mean"), exp1)
})
