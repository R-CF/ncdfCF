# Package sample data
test_that("sample data", {
  # ERA5land: Y-axis runs from top to bottom
  fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
  ds <- open_ncdf(fn)
  expect_equal(names(ds), c("t2m", "pev", "tp"))
  t2m <- ds[["t2m"]]
  expect_equal(names(t2m$axes), c("longitude", "latitude", "time"))
  lat <- t2m$axes[["latitude"]]
  lat_crds <- lat$coordinates
  expect_length(lat_crds, 21)
  expect_equal(lat_crds[1], -1.0)
  expect_equal(lat_crds[21], -3.0)

  raw <- t2m$raw()
  dn <- dimnames(raw)
  expect_equal(names(dn), c("longitude", "latitude", "time"))
  expect_equal(as.numeric(dn[["latitude"]][1]), lat_crds[1])
  expect_equal(as.numeric(dn[["latitude"]][21]), lat_crds[21])

  arr <- t2m$array()
  dn <- dimnames(arr)
  expect_equal(names(dn), c("latitude", "longitude", "time"))
  expect_equal(as.numeric(dn[["latitude"]][1]), lat_crds[1])
  expect_equal(as.numeric(dn[["latitude"]][21]), lat_crds[21])

  # CORDEX NAM-44 data for a single day; auxiliary grids
  fn <- system.file("extdata", "tasmax_NAM-44_day_20410701-vncdfCF.nc", package = "ncdfCF")
  ds <- open_ncdf(fn)
  tm <- ds[["tasmax"]]
  expect_equal(sapply(tm$axes, function (x) x$length), c(x = 148, y = 140, time = 1, height = 1))
  aux_names <- tm$auxiliary_names
  expect_equal(aux_names, c("lon", "lat"))
  lst <- list(-90:-70, 40:50)
  names(lst) <- aux_names
  sub <- tm$subset(lst)
  expect_equal(sapply(sub$axes, function (x) x$length), c(lon = 31, lat = 23, time = 1, height = 1))
  expect_equal(sub$attribute("coordinates"), "height")
})

# Offline data, not available to CRAN or Github
test_that("CRU data", {
  cru <- "../testdata/cru_ts4.08.2011.2020.tmp.dat.nc"
  if (file.exists(cru)) {
    ds <- open_ncdf(cru)

    # Test basic properties of data variables and axes
    expect_equal(names(ds), c("tmp", "stn", "mae", "maea"))
    tmp <- ds[["tmp"]]
    expect_equal(class(tmp)[1L], "CFVariable")
    expect_equal(names(tmp$axes), c("lon", "lat", "time"))

    lon <- tmp$axes[["lon"]]
    expect_equal(class(lon)[1L], "CFAxisLongitude")
    expect_equal(lon$length, 720L)
    expect_equal(lon$range(), c(-179.75, 179.75))
    expect_equal(lon$coordinates, seq(from = -179.75, by = 0.5, length = 720))

    lat <- tmp$axes[["lat"]]
    expect_equal(class(lat)[1L], "CFAxisLatitude")
    expect_equal(lat$length, 360L)
    expect_equal(lat$range(), c(-89.75, 89.75))
    expect_equal(lat$coordinates, seq(from = -89.75, by = 0.5, length = 360))

    time <- tmp$axes[["time"]]
    expect_equal(class(time)[1L], "CFAxisTime")
    expect_equal(time$length, 120L)
    tm <- time$time
    expect_equal(class(tm)[1L], "CFTime")
    expect_equal(tm$range(), c("2011-01-16", "2020-12-16"))

    # Extract raw data
    raw <- tmp[]
    expect_equal(dim(raw), c(720, 360, 120))
    dn <- dimnames(raw)
    expect_equal(names(dn), c("lon", "lat", "time"))
    ax <- attr(raw, "axis")
    expect_equal(ax, c(lon = "X", lat = "Y", time = "T"))
    t <- attr(raw, "time")[[1L]]
    expect_true(inherits(t, "CFTime"))
    expect_equal(t$calendar$origin, data.frame(year = 1900, month = 1, day = 1, hour = 0, minute = 0, second = 0, tz = "+0000", offset = 0))
    expect_length(t, 120)
    expect_equal(range(t$offsets), c(40557, 44179))
    expect_equal(t$range(), range(dn$time))
    expect_null(t$bounds)

    raw <- tmp[361:380, 271:300, ]
    expect_equal(dim(raw), c(20, 30, 120))
    dn <- dimnames(raw)
    expect_equal(names(dn), c("lon", "lat", "time"))
    ax <- attr(raw, "axis")
    expect_equal(ax, c(lon = "X", lat = "Y", time = "T"))
    t <- attr(raw, "time")[[1L]]
    expect_true(inherits(t, "CFTime"))
    expect_equal(range(as.numeric(dn$lon)), c(0.25, 9.75))
    expect_equal(range(as.numeric(dn$lat)), c(45.25, 59.75))

    raw <- tmp[, 271:300, ]
    expect_equal(dim(raw), c(720, 30, 120))
    raw <- tmp[361:380, 271:300, 25:36]
    expect_equal(dim(raw), c(20, 30, 12))
    raw <- tmp[, , 4]
    expect_equal(dim(raw), c(720, 360, 1))

    # Subsetting
    sub <- tmp$subset(lon = -65:-45, lat = -40:-30)
    expect_true(inherits(sub, "CFVariable"))
    axes <- sub$axes
    expect_length(axes, 3)
    expect_equal(names(axes), c("lon", "lat", "time"))
    expect_equal(range(axes$lon$coordinates), c(-64.75, -45.25))
    expect_equal(range(axes$lat$coordinates), c(-39.75, -30.25))
    expect_equal(range(axes$time$coordinates), c("2011-01-16", "2020-12-16"))
    expect_true(sub$has_resource)
    sub$raw()
    actual_range <- sub$attribute("actual_range")
    expect_length(actual_range, 2)

    raw <- sub$raw()
    expect_equal(dim(raw), c(40, 20, 120))
    dn <- dimnames(raw)
    expect_equal(names(dn), c("lon", "lat", "time"))
    expect_equal(range(as.numeric(dn$lon)), c(-64.75, -45.25))
    expect_equal(range(as.numeric(dn$lat)), c(-39.75, -30.25))
    expect_equal(range(dn$time), c("2011-01-16", "2020-12-16"))

    arr <- sub$array()
    expect_equal(dim(arr), c(20, 40, 120))
    dn <- dimnames(arr)
    expect_equal(names(dn), c("lat", "lon", "time"))
    expect_equal(dn$lat[1], "-30.25")
    expect_equal(range(as.numeric(dn$lat)), c(-39.75, -30.25))
    expect_equal(range(as.numeric(dn$lon)), c(-64.75, -45.25))
    expect_equal(range(dn$time), c("2011-01-16", "2020-12-16"))

    # Profile data
    prof <- tmp$profile(lon = 5, lat = 50)
    expect_true(inherits(prof, "CFVariable"))
    expect_equal(prof$name, "location_1")
    expect_equal(prof$axes[["lon"]]$length, 1)
    expect_equal(prof$axes[["lat"]]$length, 1)
    expect_equal(prof$axes[["time"]]$length, 120)
    expect_equal(prof$axes[["lon"]]$coordinates, 5)
    expect_equal(prof$axes[["lat"]]$coordinates, 50)
    expect_equal(prof$attribute("coordinates"), "lon lat")

    prof <- tmp$profile(lat = c(5, 10, 15), lon = c(30, 30, 30), .names = c("South_Sudan", "Sudan__South_Kordofan"))
    expect_true(is.list(prof))
    expect_true(all(sapply(prof, inherits, "CFVariable")))
    expect_equal(names(prof), c("South_Sudan", "Sudan__South_Kordofan", "location_3"))

    prof <- tmp$profile(lon = 5, .names = "Longitude_5_degrees")
    expect_true(inherits(prof, "CFVariable"))
    expect_equal(prof$name, "Longitude_5_degrees")
    expect_equal(prof$axes[["lon"]]$length, 1)
    expect_equal(prof$axes[["lat"]]$length, 360)
    expect_equal(prof$axes[["time"]]$length, 120)
    expect_equal(prof$axes[["lon"]]$coordinates, 5)
    expect_equal(prof$attribute("coordinates"), "lon")

    # Summarise data
    summ <- tmp$summarise("Tmean", mean, "season")
    expect_equal(summ$name, "Tmean")
    expect_equal(summ$axes[["time"]]$length, 41)
    expect_length(summ$attribute("actual_range"), 2)

    summ <- sub$summarise("Tmean", mean, "season")
    expect_equal(summ$name, "Tmean")
    expect_equal(summ$axes[["time"]]$length, 41)
    expect_length(summ$attribute("actual_range"), 2)

    summ <- prof$summarise("Tmean", mean, "quarter")
    expect_equal(summ$axes[["time"]]$length, 40)
    expect_length(summ$attribute("actual_range"), 2)

    summ <- sub$summarise(c("Tmin", "Tmax"), range, "year", era = list(early = 2011:2016, late = 2017:2020))
    expect_true(is.list(summ))
    expect_equal(names(summ), c("early", "late"))
    expect_equal(names(summ$early), c("Tmin", "Tmax"))
    expect_equal(names(summ$late), c("Tmin", "Tmax"))
    Tearly <- summ$early$Tmin$axes[["time"]]
    expect_equal(Tearly$length, 6)
    expect_equal(summ$late$Tmin$axes[["time"]]$length, 4)
    expect_length(Tearly$attribute("actual_range"), 2)
    expect_true(inherits(Tearly$time, "CFClimatology"))

    # Save to file, read back in
    fn <- tempfile(fileext = ".nc")
    Tmin <- summ$early$Tmin
    Tmin$save(fn)
    ds2 <- open_ncdf(fn)
    expect_equal(names(ds2), "Tmin")
    T2 <- ds2[["Tmin"]]
    expect_equal(T2$raw(), aperm(Tmin$raw(), c(2, 3, 1)))
    expect_equal(T2$axes[["lon"]]$coordinates, Tmin$axes[["lon"]]$coordinates)
    expect_equal(T2$axes[["lat"]]$coordinates, Tmin$axes[["lat"]]$coordinates)
    expect_equal(T2$axes[["time"]]$coordinates, Tmin$axes[["time"]]$coordinates)
    ds2 <- NULL
    unlink(fn)
  }
})

test_that("CMEMS example online", {
  cmems <- "../testdata/cmems_mod_med_phy-tem_anfc_4.2km_P1M-m_1748594717636.nc"
  if (file.exists(cmems)) {
    suppressWarnings(ds <- open_ncdf(cmems))

    thetao <- ds[["thetao"]]
    tc <- thetao$axes[["time"]]$coordinates
    dc <- thetao$axes[["depth"]]$coordinates
    expect_length(tc, 12)
    expect_length(dc, 141)

    surfaceT <- thetao$subset(depth = c(0, 2))
    expect_length(surfaceT$axes, 4)
    expect_length(surfaceT$axes[["depth"]]$coordinates, 1)

    # Get the longitude and latitude coordinates
    longitude <- thetao$axes[["longitude"]]$coordinates
    latitude <- thetao$axes[["latitude"]]$coordinates

    # Make a map of the first time slice: January
    # library(RColorBrewer)
    # image(longitude, latitude, surfaceT$raw()[,,1], col = rev(brewer.pal(10, "RdBu")))

    # library(lattice)
    # grid <- expand.grid(lon=longitude, lat=latitude)
    # cutpts <- c(12,13,14,15,16,17,18,19,20)
    # levelplot(surfaceT$raw()[,,1] ~ lon * lat,
    #           data=grid, region=TRUE,
    #           pretty=T, at=cutpts, cuts=9,
    #           col.regions=(rev(brewer.pal(9,"RdBu"))), contour=0,
    #           xlab = "Longitude", ylab = "Latitude",
    #           main = "Sea Water Potential Temperature (°C)"
    # )

    # Make a vertical profile of temperature at a single location for the month of July 2024
    tprof <- thetao$profile(longitude = 12.71, latitude = 40.33, time = "2024-07-01", .names = "Tyrrhenian_Sea")
    expect_equal(sapply(tprof$axes, function(ax) ax$length), c(depth = 141, longitude = 1, latitude = 1, time = 1))
    Tjuly <- tprof$data.table()
    expect_length(Tjuly, 5)
    expect_equal(nrow(Tjuly), 141)

    # suppressPackageStartupMessages(library(ggplot2))
    # ggplot(Tjuly) + geom_line(aes(depth, Tyrrhenian_Sea)) + theme_bw() + xlim(0, 4000) +
    #  labs(x = "Depth (m)", y = "Temperature (°C)", title =  "Tyrrhenian Sea", subtitle = "12.71E - 40.33N")
  }
})

test_that("Auxiliary grids", {
  aux <- "../testdata/pr_EUR-11_MOHC-HadGEM2-ES_rcp45_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20060101-20101230.nc"
  if (file.exists(aux)) {
    ds <- open_ncdf(aux)
    pr <- ds[["pr"]]
    expect_equal(pr$attribute("coordinates"), "lat lon")
    expect_equal(pr$attribute("grid_mapping"), "rotated_latitude_longitude")
    sel <- pr$subset(lon = -10:5, lat = 45:60, time = c("2009-01-01", "2010-01-01"))
    expect_true(is.na(sel$attribute("coordinates")))
    expect_true(is.na(sel$attribute("grid_mapping")))
    lon_crds <- sel$axes[["lon"]]$coordinates
    lon_res <- diff(lon_crds[1:2])
    lat_crds <- sel$axes[["lat"]]$coordinates
    lat_res <- diff(lat_crds[1:2])
    expect_true(abs(lon_crds[1] - -10) < lon_res)
    expect_true(abs(lat_crds[1] - 45) < lat_res)
    expect_true(abs(lon_crds[length(lon_crds)] - 5) < lon_res)
    expect_true(abs(lat_crds[length(lat_crds)] - 60) < lat_res)

    bnds <- sel$axes[["lon"]]$bounds
    expect_equal(bnds$values[1, 1], -10)
    # expect_equal(bnds$values[2, 75], 5)
    bnds <- sel$axes[["lat"]]$bounds
    expect_equal(bnds$values[1, 1], 45)
    # expect_equal(bnds$values[2, 75], 60)
  }
})

test_that("Multiple label sets", {
  mme <- "../testdata/t_CMIP6_ssp245_mon_201501-210012.nc"
  if (file.exists(mme)) {
    ds <- open_ncdf(mme)
    t <- ds[["t"]]
    expect_length(t$axes, 5)
    member <- t$axes[["member"]]
    expect_equal(member$coordinate_names, c("member", "member_id", "gcm_institution", "gcm_model", "gcm_variant"))
    expect_equal(member$active_coordinates, "member")
    sel <- t$subset(time = "2020-01-01", member = 4)
    expect_true(inherits(sel, "CFVariable"))
    expect_equal(sapply(sel$axes, function(x) x$length), c(lon = 360, lat = 180, time = 1, member = 1, height2m = 1))

    member$active_coordinates <- "gcm_model"
    expect_equal(member$coordinates[1], "ACCESS-CM2")
    sel <- t$subset(time = "2020-01-01", member = c("CAMS-CSM1-0", "CESM2"))
    expect_true(inherits(sel, "CFVariable"))
    expect_equal(sapply(sel$axes, function(x) x$length), c(lon = 360, lat = 180, member = 3, time = 1, height2m = 1))
    expect_equal(sel$axes[["member"]]$coordinates, c("CAMS-CSM1-0", "CESM2-WACCM", "CESM2"))
  }
})

test_that("Merge CFVariable", {
  esm4 <- list.files("../testdata", "^pr_day_GFDL.*nc$", full.names = TRUE)
  if (length(esm4)) {
    expect_length(esm4, 4) # Just to make sure the files are captured
    ds <- open_ncdf(esm4[1])
    combi <- ds[["pr"]]$subset(lon = 0:10, lat = 0:20)
    expect_equal(names(combi$axes), c("lon", "lat", "time"))
    crds_lon <- combi$axes[["lon"]]$coordinates
    crds_lat <- combi$axes[["lat"]]$coordinates
    len_time <- combi$axes[["time"]]$length

    for (i in 2:4) {
      tmp <- open_ncdf(esm4[i])
      pr <- tmp[["pr"]]$subset(lon = 0:10, lat = 0:20)
      expect_equal(pr$axes[["lon"]]$coordinates, crds_lon)
      expect_equal(pr$axes[["lat"]]$coordinates, crds_lat)
      len_time <- len_time + pr$axes[["time"]]$length
      combi$append(pr, "time")
    }

    expect_equal(combi$axes[["time"]]$length, len_time)
    expect_equal(range(diff(combi$axes[["time"]]$values)), c(1, 1))
  } else cat("\nCheck esm4\n")
})

test_that("L3b", {
  l3b <- "../testdata/AQUA_MODIS.20030101.L3b.DAY.CHL.nc"
  if (file.exists(l3b)) {
    ds <- open_ncdf(l3b)
    expect_equal(ds$file_type, "NASA level-3 binned data")
    chl <- ds[["/level-3_binned_data/chlor_a"]]
    expect_true(inherits(chl, "CFVariable"))
    expect_equal(chl$name, "chlor_a")
    expect_equal(chl$crs$attribute("grid_mapping_name"), "latitude_longitude")
    expect_equal(names(chl$axes), c("latitude", "longitude", "time"))
    expect_equal(sapply(chl$axes, function(ax) ax$length), c(latitude = 3001, longitude = 8432, time = 1))

    raw <- chl$raw()
    expect_equal(dim(raw), c(3001, 8432))
    expect_equal(names(dimnames(raw)), c("latitude", "longitude"))

    sub <- chl$subset(latitude = 0:30, longitude = 30:50)
    expect_true(inherits(sub, "CFVariable"))
    expect_equal(sub$name, "chlor_a")
    expect_equal(sub$crs$attribute("grid_mapping_name"), "latitude_longitude")
    expect_equal(names(sub$axes), c("latitude", "longitude", "time"))
    expect_equal(sapply(sub$axes, function(ax) ax$length), c(latitude = 722, longitude = 482, time = 1))

    raw <- sub$raw()
    expect_equal(dim(raw), c(722, 482))
    expect_equal(names(dimnames(raw)), c("latitude", "longitude"))
  }
})

test_that("external_variables", {
  esm1 <- "../testdata/chl_Omon_MPI-ESM1-2-HR_ssp245_r1i1p1f1_gn_201501-201912.nc"
  if (file.exists(esm1)) {
    ds <- open_ncdf(esm1)
    expect_equal(ds$attribute("external_variables"), "areacello volcello")
    chl <- ds[["chl"]]
    expect_equal(chl$attribute("cell_measures"), "area: areacello volume: volcello")
    cm <- chl$cell_measures
    expect_length(cm, 2)
    expect_equal(names(cm), c("areacello", "volcello"))
    expect_equal(sapply(cm, function(m) m$measure), c(areacello = "area", volcello = "volume"))
    expect_null(cm[[1L]]$data())
    area_file <- "../testdata/areacello_Ofx_MPI-ESM1-2-HR_ssp245_r1i1p1f1_gn.nc"
    cm[[1L]]$link(area_file)
    area_var <- cm[[1L]]$data()
    expect_true(inherits(area_var, "CFVariable"))
  }
})
