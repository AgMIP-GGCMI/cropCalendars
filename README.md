# cropCalendars

R-package for simulating crop calendars and their adaptation following the approaches from Waha et al. (2012) and Minoli et al. (2019).

## Installation

```bash
git clone https://gitlab.pik-potsdam.de/lpjml/crop_calendars.git .
cd ..
R CMD build cropCalendars
# this generates e.g. cropCalendars_0.1.0.tar.gz
R CMD INSTALL [-l my/Rlib/path] cropCalendars_0.1.0.tar.gz
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
climate <- calcMonthlyClimate()
crop_calendars <- calcCropCalendars()
phenological_heat_units <- calcPHU()
```

