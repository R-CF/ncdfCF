pkgname <- "CFtime"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('CFtime')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CFfactor")
### * CFfactor

flush(stderr()); flush(stdout())

### Name: CFfactor
### Title: Create a factor from the offsets in a 'CFTime' instance
### Aliases: CFfactor

### ** Examples

t <- CFtime("days since 1949-12-01", "360_day", 19830:54029)

# Create a dekad factor for the whole time series
f <- CFfactor(t, "dekad")

# Create three monthly factors for early, mid and late 21st century eras
ep <- CFfactor(t, era = list(early = 2021:2040, mid = 2041:2060, late = 2061:2080))



cleanEx()
nameEx("CFfactor_coverage")
### * CFfactor_coverage

flush(stderr()); flush(stdout())

### Name: CFfactor_coverage
### Title: Coverage of time elements for each factor level
### Aliases: CFfactor_coverage

### ** Examples

t <- CFtime("days since 2001-01-01", "365_day", 0:364)
f <- CFfactor(t, "dekad")
CFfactor_coverage(t, f, "absolute")



cleanEx()
nameEx("CFfactor_units")
### * CFfactor_units

flush(stderr()); flush(stdout())

### Name: CFfactor_units
### Title: Number of base time units in each factor level
### Aliases: CFfactor_units

### ** Examples

t <- CFtime("days since 2001-01-01", "365_day", 0:364)
f <- CFfactor(t, "dekad")
CFfactor_units(t, f)



cleanEx()
nameEx("CFtime-function")
### * CFtime-function

flush(stderr()); flush(stdout())

### Name: CFtime-function
### Title: Create a CFTime object
### Aliases: CFtime-function CFtime

### ** Examples

CFtime("days since 1850-01-01", "julian", 0:364)

CFtime("hours since 2023-01-01", "360_day", "2023-01-30T23:00")



cleanEx()
nameEx("as.character.CFTime")
### * as.character.CFTime

flush(stderr()); flush(stdout())

### Name: as.character.CFTime
### Title: Return the timestamps contained in the 'CFTime' instance.
### Aliases: as.character.CFTime

### ** Examples

t <- CFtime("days since 1850-01-01", "julian", 0:364)
as.character(t)



cleanEx()
nameEx("as_timestamp")
### * as_timestamp

flush(stderr()); flush(stdout())

### Name: as_timestamp
### Title: Create a vector that represents CF timestamps
### Aliases: as_timestamp

### ** Examples

t <- CFtime("hours since 2020-01-01", "standard", seq(0, 24, by = 0.25))
as_timestamp(t, "timestamp")

t2 <- CFtime("days since 2002-01-21", "standard", 0:20)
tail(as_timestamp(t2, asPOSIX = TRUE))

tail(as_timestamp(t2))

tail(as_timestamp(t2 + 1.5))



cleanEx()
nameEx("bounds")
### * bounds

flush(stderr()); flush(stdout())

### Name: bounds
### Title: Bounds of the time offsets
### Aliases: bounds bounds<-

### ** Examples

t <- CFtime("days since 2024-01-01", "standard", seq(0.5, by = 1, length.out = 366))
as_timestamp(t)[1:3]
bounds(t) <- rbind(0:365, 1:366)
bounds(t)[, 1:3]
bounds(t, "%d-%b-%Y")[, 1:3]



cleanEx()
nameEx("cut.CFTime")
### * cut.CFTime

flush(stderr()); flush(stdout())

### Name: cut.CFTime
### Title: Create a factor for a 'CFTime' instance
### Aliases: cut.CFTime cut

### ** Examples

x <- CFtime("days since 2021-01-01", "365_day", 0:729)
breaks <- c("2022-02-01", "2021-12-01", "2023-01-01")
cut(x, breaks)



cleanEx()
nameEx("equals-.CFTime")
### * equals-.CFTime

flush(stderr()); flush(stdout())

### Name: ==.CFTime
### Title: Equivalence of CFTime objects
### Aliases: ==.CFTime CFtime-equivalent

### ** Examples

e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
e2 <- CFtime("days since 1850-01-01 00:00:00", "standard", 0:364)
e1 == e2



cleanEx()
nameEx("indexOf")
### * indexOf

flush(stderr()); flush(stdout())

### Name: indexOf
### Title: Find the index of timestamps in the time series
### Aliases: indexOf

### ** Examples

cf <- CFtime("days since 2020-01-01", "360_day", 1440:1799 + 0.5)
as_timestamp(cf)[1:3]
x <- c("2024-01-01", "2024-01-02", "2024-01-03")
indexOf(x, cf)
indexOf(x, cf, method = "linear")

bounds(cf) <- TRUE
indexOf(x, cf)

# Non-existent calendar day in a `360_day` calendar
x <- c("2024-03-30", "2024-03-31", "2024-04-01")
indexOf(x, cf)

# Numeric x
indexOf(c(29, 30, 31), cf)



cleanEx()
nameEx("is_complete")
### * is_complete

flush(stderr()); flush(stdout())

### Name: is_complete
### Title: Indicates if the time series is complete
### Aliases: is_complete

### ** Examples

t <- CFtime("days since 1850-01-01", "julian", 0:364)
is_complete(t)



cleanEx()
nameEx("length.CFTime")
### * length.CFTime

flush(stderr()); flush(stdout())

### Name: length.CFTime
### Title: The length of the offsets contained in the 'CFTime' instance.
### Aliases: length.CFTime

### ** Examples

t <- CFtime("days since 1850-01-01", "julian", 0:364)
length(t)



cleanEx()
nameEx("month_days")
### * month_days

flush(stderr()); flush(stdout())

### Name: month_days
### Title: Return the number of days in a month given a certain CF calendar
### Aliases: month_days

### ** Examples

dates <- c("2021-11-27", "2021-12-10", "2022-01-14", "2022-02-18")
t <- CFtime("days since 1850-01-01", "standard")
month_days(t, dates)

t <- CFtime("days since 1850-01-01", "360_day")
month_days(t, dates)

t <- CFtime("days since 1850-01-01", "all_leap")
month_days(t, dates)

month_days(t)



cleanEx()
nameEx("parse_timestamps")
### * parse_timestamps

flush(stderr()); flush(stdout())

### Name: parse_timestamps
### Title: Parse series of timestamps in CF format to date-time elements
### Aliases: parse_timestamps

### ** Examples

t <- CFtime("days since 0001-01-01", "proleptic_gregorian")

# This will have `NA`s on output and generate a warning
timestamps <- c("2012-01-01T12:21:34Z", "12-1-23", "today",
                "2022-08-16T11:07:34.45-10", "2022-08-16 10.5+04")
parse_timestamps(t, timestamps)



cleanEx()
nameEx("plus-.CFTime")
### * plus-.CFTime

flush(stderr()); flush(stdout())

### Name: +.CFTime
### Title: Extend a CFTime object
### Aliases: +.CFTime CFtime-merge

### ** Examples

e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
e2 <- CFtime("days since 1850-01-01 00:00:00", "standard", 365:729)
e1 + e2



cleanEx()
nameEx("properties")
### * properties

flush(stderr()); flush(stdout())

### Name: definition
### Title: Properties of a CFTime object
### Aliases: definition properties calendar unit origin timezone offsets
###   resolution

### ** Examples

t <- CFtime("days since 1850-01-01", "julian", 0:364)
definition(t)
calendar(t)
unit(t)
timezone(t)
origin(t)
offsets(t)
resolution(t)



cleanEx()
nameEx("range.CFTime")
### * range.CFTime

flush(stderr()); flush(stdout())

### Name: range.CFTime
### Title: Extreme time series values
### Aliases: range.CFTime

### ** Examples

cf <- CFtime("days since 1850-01-01", "julian", 0:364)
range(cf)
range(cf, "%Y-%b-%e")



cleanEx()
nameEx("slice")
### * slice

flush(stderr()); flush(stdout())

### Name: slice
### Title: Which time steps fall within extreme values
### Aliases: slice

### ** Examples

t <- CFtime("hours since 2023-01-01 00:00:00", "standard", 0:23)
slice(t, c("2022-12-01", "2023-01-01 03:00"))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
