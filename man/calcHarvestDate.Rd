% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcHarvestDate.R
\name{calcHarvestDate}
\alias{calcHarvestDate}
\title{Calculate harvest date (Minoli et al., 2019)}
\usage{
calcHarvestDate(
  croppar,
  monthly_temp,
  sowing_date,
  sowing_month,
  sowing_season,
  seasonality,
  harvest_rule,
  hd_vector
)
}
\arguments{
\item{croppar}{data.frame with crop parematers as returned by getCropParam}

\item{monthly_temp}{numeric vector of length 12. Mean monthly air temperature
(degree Celsius).}

\item{sowing_date}{numeric value as day of the year (DOY). This can be either
caculated with calcSowingDate or prescribed.}

\item{sowing_month}{numeric value between 0 and 12. 0 represents the "default
month" if no sowing date is found by calcSowingDate, 1:12 represent months
from January to December.}

\item{sowing_season}{character value. Can be either "winter" or "spring". See
calcSowingDate.}

\item{seasonality}{seasonality type as calculated by calcSeasonality}

\item{harvest_rule}{harvest rule as calculated by calcHarvestRule}

\item{hd_vector}{vector of possible harvest dates as calculated by
calcHarvestDateVector}
}
\description{
Rule-based estimation of the end of the crop growing period
(date of physiological maturity), here called harvest date for simplicity.
The assumption behind these rules is that farmers select growing
seasons based on the mean climatic characteristics of the location in
which they operate and on the physiological limitations (base and optimum
temperatures for reproductive growth; sensitivity to terminal water stress) of
the respective crop species.
}
