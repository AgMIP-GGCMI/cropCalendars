test_that("calcPET_FAO56 preserves input shape (vector and matrix) and NAs", {
  # Vector in -> vector out
  pv <- cropCalendars::calcPET_FAO56(
    temp = c(20, 25), windspeed = c(2, 2), humid = c(0.01, 0.01),
    swdown = c(200, 200), lwdown = c(300, 300), ps = c(101325, 101325)
  )
  expect_null(dim(pv))
  expect_length(pv, 2)

  # Matrix in -> matrix out with same dim (cell-vectorised callers rely on this)
  mk <- function(x) matrix(x, 2, 2)
  pm <- cropCalendars::calcPET_FAO56(
    matrix(c(20, 25, 15, 30), 2, 2), mk(2), mk(0.01), mk(200), mk(300), mk(101325)
  )
  expect_equal(dim(pm), c(2L, 2L))

  # NA in -> NA out (clamp must not error or fill NAs)
  mna <- matrix(c(20, 25, 15, 30), 2, 2); mna[1, 2] <- NA
  pna <- cropCalendars::calcPET_FAO56(mna, mk(2), mk(0.01), mk(200), mk(300), mk(101325))
  expect_equal(dim(pna), c(2L, 2L))
  expect_true(is.na(pna[1, 2]))

  # ET0 is non-negative where defined
  expect_true(all(pm >= 0))
})
