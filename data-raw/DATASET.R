# Unit-of-measure
epsg_uom <- read.csv("./data-raw/epsg-uom.csv")
epsg_uom$factor_b[epsg_uom$factor_b == "NULL"] <- NA
epsg_uom$factor_b <- as.numeric(epsg_uom$factor_b)
epsg_uom$factor_c[epsg_uom$factor_c == "NULL"] <- NA
epsg_uom$factor_c <- as.numeric(epsg_uom$factor_c)

# Prime meridian
epsg_pm <- read.csv("./data-raw/epsg-pm.csv")

# Ellipsoid
epsg_ell <- read.csv("./data-raw/epsg-ell.csv")
epsg_ell$semi_minor_axis[epsg_ell$semi_minor_axis == "NULL"] <- NA
epsg_ell$semi_minor_axis <- as.numeric(epsg_ell$semi_minor_axis)
epsg_ell$inv_flattening[epsg_ell$inv_flattening == "NULL"] <- NA
epsg_ell$inv_flattening <- as.numeric(epsg_ell$inv_flattening)

# Datum
epsg_datum <- read.csv("./data-raw/epsg-datum.csv")
epsg_datum$ellipsoid_code[epsg_datum$ellipsoid_code == "NULL"] <- NA
epsg_datum$ellipsoid_code <- as.integer(epsg_datum$ellipsoid_code)
epsg_datum$prime_meridian_code[epsg_datum$prime_meridian_code == "NULL"] <- NA
epsg_datum$prime_meridian_code <- as.integer(epsg_datum$prime_meridian_code)
epsg_datum$frame_reference_epoch[epsg_datum$frame_reference_epoch == "NULL"] <- NA_character_
epsg_datum$cf_name <- trimws(gsub("_+", "_", str_replace_all(epsg_datum$datum_name, "[^A-Za-z0-9]", "_")), whitespace = "_")

# Datum ensemble
epsg_datum_ensemble <- read.csv("./data-raw/epsg-datumensemble.csv")

# Axis
epsg_axes <- read.csv("./data-raw/epsg-axis.csv")

# Coordinate system
epsg_coordsys <- read.csv("./data-raw/epsg-cs.csv")

# Coordinate operation methods and values
epsg_proj_ops <- read.csv("./data-raw/epsg-projcoordops.csv")
epsg_proj_op_values <- read.csv("./data-raw/epsg-projcoordopsvalues.csv")

# Coordinate reference systems
epsg_geo_crs <- read.csv("./data-raw/epsg-geocrs.csv")
epsg_geo_crs$base_crs_code[epsg_geo_crs$base_crs_code == "NULL"] <- NA
epsg_geo_crs$base_crs_code <- as.integer(epsg_geo_crs$base_crs_code)
epsg_geo_crs$projection_conv_code[epsg_geo_crs$projection_conv_code == "NULL"] <- NA
epsg_geo_crs$projection_conv_code <- as.integer(epsg_geo_crs$projection_conv_code)

epsg_proj_crs <- read.csv("./data-raw/epsg-projcrs.csv")
epsg_proj_crs$base_crs_code[epsg_proj_crs$base_crs_code == "NULL"] <- NA
epsg_proj_crs$base_crs_code <- as.integer(epsg_proj_crs$base_crs_code)
epsg_proj_crs$projection_conv_code[epsg_proj_crs$projection_conv_code == "NULL"] <- NA
epsg_proj_crs$projection_conv_code <- as.integer(epsg_proj_crs$projection_conv_code)

epsg_vert_crs <- read.csv("./data-raw/epsg-vertcrs.csv")
epsg_vert_crs$datum_code[epsg_vert_crs$datum_code == "NULL"] <- NA
epsg_vert_crs$datum_code <- as.integer(epsg_vert_crs$datum_code)
epsg_vert_crs$base_crs_code[epsg_vert_crs$base_crs_code == "NULL"] <- NA
epsg_vert_crs$base_crs_code <- as.integer(epsg_vert_crs$base_crs_code)
epsg_vert_crs$projection_conv_code[epsg_vert_crs$projection_conv_code == "NULL"] <- NA
epsg_vert_crs$projection_conv_code <- as.integer(epsg_vert_crs$projection_conv_code)

epsg_cmpd_crs <- read.csv("./data-raw/epsg-cmpdcrs.csv")

epsg_uom_alias <- read.csv("./data-raw/epsg-uom-alias.csv")

usethis::use_data(epsg_uom, epsg_pm, epsg_ell, epsg_datum, epsg_datum_ensemble,
                  epsg_axes, epsg_coordsys, epsg_proj_ops, epsg_proj_op_values,
                  epsg_geo_crs, epsg_proj_crs, epsg_vert_crs, epsg_cmpd_crs,
                  epsg_uom_alias,
                  internal = TRUE, overwrite = TRUE, version = 3)
