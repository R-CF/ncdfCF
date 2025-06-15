## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic_example------------------------------------------------------------
library(ncdfCF)
  
# Get a netCDF file, here hourly data for 2016-01-01 over Rwanda
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
  
# Open the file, all metadata is read
(ds <- open_ncdf(fn))
  
# Variables can be accessed through standard list-type extraction syntax
(t2m <- ds[["t2m"]])
  
# Same with dimensions, but now without first assigning the object to a symbol
ds[["longitude"]]
  
# Regular base R operations simplify life further
dimnames(ds[["pev"]]) # A variable: list of dimension names
  
dimnames(ds[["longitude"]]) # A dimension: vector of dimension element values
  
# Access attributes
ds[["pev"]]$attribute("long_name")

## ----extract------------------------------------------------------------------
# Extract a time series for a specific location
ts <- t2m[5, 4, ]
str(ts)
  
# Extract the full spatial extent for one time step
ts <- t2m[, , 12]
str(ts)

## ----subset-------------------------------------------------------------------
# Extract a specific region, full time dimension
(ts <- t2m$subset(X = 29:30, Y = -1:-2))

# Extract specific time slices for a specific region
# Note that the axes are specified out of order and using alternative
# specifications: only the extreme values are used.
(ts <- t2m$subset(T = c("2016-01-01 09:00", "2016-01-01 15:00"),
                  X = c(29.6, 28.8),
                  Y = seq(-2, -1, by = 0.05)))

## ----cfarray------------------------------------------------------------------
# Open a file and read the data from a variable into a CFArray instance
fn <- system.file("extdata", "tasmax_NAM-44_day_20410701-vncdfCF.nc", package = "ncdfCF")
ds <- open_ncdf(fn)
(tx <- ds[["tasmax"]]$data())

# Use the terra package for plotting
# install.packages("terra")
library(terra)

# Get the data in exactly the way it is stored in the file, using `raw()`
tx_raw <- tx$raw()
str(tx_raw)

# Plot the data
(r <- rast(tx_raw))
plot(r)

## ----cfarray2-----------------------------------------------------------------
tx_array <- tx$array()
str(tx_array)
r <- rast(tx_array)
plot(r)

## ----terra--------------------------------------------------------------------
(r <- tx$terra())
plot(r)

