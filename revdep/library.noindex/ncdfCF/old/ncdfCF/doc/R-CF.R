## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ncdfCF-------------------------------------------------------------------
library(ncdfCF)

fn <- system.file("extdata", "tasmax_NAM-44_day_20410701-vncdfCF.nc", package = "ncdfCF")
ds <- open_ncdf(fn)

# The data variable
names(ds)

# The details of the data variable
tmax <- ds[["tasmax"]]
tmax$print(width = 25)

# The "time" axis
ds[["time"]]

# Parameters in the grid mapping variable
ds[["Lambert_Conformal"]]

## ----matrix-------------------------------------------------------------------
matrix(1:12, nrow = 3, ncol = 4)

## ----matrix-by-row------------------------------------------------------------
matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)

## ----cf-matrix----------------------------------------------------------------
matrix(c(9, 5, 1, 10, 6, 2, 11, 7, 3, 12, 8, 4), nrow = 3, ncol = 4)

## ----read-array---------------------------------------------------------------
# The raw data using the ordering in the netCDF file (or as modified by
# a processing method such as `summarise()`)
tmax_raw <- tmax$data()$raw()
str(tmax_raw)

# The same data but now in standard R ordering
tmax_R <- tmax$data()$array()
str(tmax_R)

