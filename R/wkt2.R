.wkt2_id <- function(epsg_code) {
  paste0('ID["EPSG",', epsg_code, ']')
}

.wkt2_uom <- function(epsg_code, id = FALSE) {
  details <- epsg_uom[epsg_uom$uom_code == epsg_code, ]
  auth <- if (id) paste0(',', .wkt2_id(epsg_code)) else ''
  paste0(toupper(details$unit_of_meas_type), 'UNIT["', details$unit_of_meas_name, '",',
         details$factor_b / details$factor_c, auth, ']')
}

.wkt2_ellipsoid = function(epsg_code) {
  details <- epsg_ell[epsg_ell$ellipsoid_code == epsg_code, ]

  invf <- if (is.numeric(details$inv_flattening))
    details$inv_flattening
  else if (isTRUE(all.equal(details$semi_major_axis, details$semi_minor_axis)))
    0
  else
    details$semi_major_axis / (details$semi_major_axis - details$semi_minor_axis)

  paste0('ELLIPSOID["', details$ellipsoid_name, '",',
         details$semi_major_axis, ',', invf, ',',
         .wkt2_uom(details$uom_code), ',',
         .wkt2_id(details$ellipsoid_code), ']')
}

.wkt2_pm <- function(epsg_code) {
  details <- epsg_pm[epsg_pm$prime_meridian_code == epsg_code, ]
  paste0('PRIMEM["', details$prime_meridian_name, '",',
         details$greenwich_longitude, ',',
         .wkt2_uom(details$uom_code), ',',
         .wkt2_id(details$prime_meridian_code), ']')
}

.wkt2_datum_ensemble <- function(details) {
  members <- epsg_datum_ensemble[epsg_datum_ensemble$datum_ensemble_code == details$datum_code, ]

  mem <- vector("character", nrow(members))
  for (m in 1:nrow(members))
    mem[m] <- paste0('MEMBER["', members[m, "member_name"], '",',
                     .wkt2_id(members[m, "member_code"]), ']')

  m1 <- epsg_datum[epsg_datum$datum_code == members[1L, "member_code"], ]
  if (m1$datum_type == "vertical") {
    ell <- ''
    pm <- ''
  } else {
    ell <- paste0(.wkt2_ellipsoid(m1$ellipsoid_code), ',')
    pm <- paste0(',', .wkt2_pm(m1$prime_meridian_code))
  }

  paste0('ENSEMBLE["', details$datum_name, '",', paste(mem, collapse = ","), ',',
         ell, 'ENSEMBLEACCURACY[', members[1L, "ensemble_accuracy"], '],',
         .wkt2_id(details$datum_code), ']', pm)
}

.wkt2_datum_geo <- function(epsg_code = NULL, name = NULL) {
  details <- epsg_datum[epsg_datum$datum_code == epsg_code, ]
  if (details$datum_type == "ensemble") return(.wkt2_datum_ensemble(details))

  epoch <- if (is.na(details$frame_reference_epoch)) ''
  else paste0('ANCHOREPOCH[', details$frame_reference_epoch, '],')

  paste0('DATUM["', details$datum_name, '",',
         .wkt2_ellipsoid(details$ellipsoid_code), ',', epoch,
         .wkt2_id(details$datum_code), '],',
         .wkt2_pm(details$prime_meridian_code))
}

.wkt2_datum_vert <- function(epsg_code) {
  details <- epsg_datum[epsg_datum$datum_code == epsg_code, ]
  if (details$datum_type == "ensemble") return(.wkt2_datum_ensemble(details))

  paste0('VDATUM["', details$datum_name, '",', .wkt2_id(epsg_code), ']')
}

.wkt2_operation_parameters <- function(epsg_code) {
  details <- epsg_proj_op_values[epsg_proj_op_values$coord_op_code == epsg_code, ]

  params <- vector("character", nrow(details))
  for (p in 1:nrow(details)) {
    params[p] <- paste0('PARAMETER["', details[p, "parameter_name"], '",',
                        details[p, "parameter_value"], ',',
                        .wkt2_uom(details[p, "uom_code"]), ',',
                        .wkt2_id(details[p, "parameter_code"]), ']')
  }
  paste0(params, collapse = ",")
}

# Coordinate system
.wkt2_coordsys <- function(epsg_code) {
  details <- epsg_coordsys[epsg_coordsys$coord_sys_code == epsg_code, ]
  ax_details <- epsg_axes[epsg_axes$coord_sys_code == details$coord_sys_code, ]

  cs <- paste0('CS[', details$coord_sys_type, ',', details$dimension, '],')

  # if uom_code is not unique, add axis units to every axis
  add_axis_units <- length(unique(ax_details$uom_code)) > 1L

  # Describe axes
  axes <- vector("character", nrow(ax_details))
  for (ax in 1:nrow(ax_details)) {
    x <- paste0('AXIS["', ax_details[ax, "coord_axis_name"], ' (',
                ax_details[ax, "coord_axis_abbreviation"], ')",',
                ax_details[ax, "coord_axis_orientation"],
                ',ORDER[', ax, ']')
    if (add_axis_units)
      x <- paste0(x, ',', .wkt2_uom(ax_details[ax, "uom_code"]))
    axes[ax] <- paste0(x, ']')
  }
  axes <- paste(axes, collapse = ',')

  if (add_axis_units) paste0(cs, axes)
  else paste0(cs, axes, ',', .wkt2_uom(ax_details[1L, ]$uom_code))
}

# Conversion, aka projection
.wkt2_conversion <- function(epsg_code) {
  details <- epsg_proj_ops[epsg_proj_ops$coord_op_code == epsg_code, ]
  paste0('CONVERSION["', details$coord_op_name, '",METHOD["',
         details$coord_op_method_name, '",',
         .wkt2_id(details$coord_op_method_code), '],',
         .wkt2_operation_parameters(epsg_code), ',',
         .wkt2_id(epsg_code), ']')
}

.wkt2_crs_base <- function(epsg_code) {
  details <- epsg_geo_crs[epsg_geo_crs$coord_ref_sys_code == epsg_code, ]
  keyword <- if (details$coord_ref_sys_kind == "geocentric") 'BASEGEODCRS["' else 'BASEGEOGCRS["'

  paste0(keyword, details$coord_ref_sys_name, '",',
         .wkt2_datum_geo(details$datum_code), ',',
         .wkt2_id(details$coord_ref_sys_code), ']')
}

.wkt2_crs_geo <- function(epsg_code) {
  details <- epsg_geo_crs[epsg_geo_crs$coord_ref_sys_code == epsg_code, ]
  if (!nrow(details)) return('')

  keyword <- if (details$coord_ref_sys_kind == "geocentric") 'GEODCRS["' else 'GEOGCRS["'

  paste0(keyword, details$coord_ref_sys_name, '",',
         .wkt2_datum_geo(details$datum_code), ',',
         .wkt2_coordsys(details$coord_sys_code), ',',
         .wkt2_id(details$coord_ref_sys_code), ']')
}

.wkt2_crs_proj <- function(epsg_code) {
  details <- epsg_proj_crs[epsg_proj_crs$coord_ref_sys_code == epsg_code, ]
  if (!nrow(details)) return('')

  paste0('PROJCRS["', details$coord_ref_sys_name, '",',
         .wkt2_crs_base(details$base_crs_code), ',',
         .wkt2_conversion(details$projection_conv_code), ',',
         .wkt2_coordsys(details$coord_sys_code), ',',
         .wkt2_id(details$coord_ref_sys_code), ']')
}

.wkt2_crs_vert <- function(epsg_code) {
  details <- epsg_vert_crs[epsg_vert_crs$coord_ref_sys_code == epsg_code, ]
  if (!nrow(details)) return('')

  datum_code <- if (is.na(details$datum_code)) {
    dd <- epsg_vert_crs[epsg_vert_crs$coord_ref_sys_code == details$base_crs_code, ]
    if (is.na(dd$datum_code)) {
      epsg_vert_crs[epsg_vert_crs$coord_ref_sys_code == dd$base_crs_code, "datum_code"]
    } else dd$datum_code
  } else details$datum_code

  paste0('VERTCRS["', details$coord_ref_sys_name, '",',
         .wkt2_datum_vert(datum_code), ',',
         .wkt2_coordsys(details$coord_sys_code), ',',
         .wkt2_id(epsg_code), ']')
}

.wkt2_crs_compound <- function(epsg_code) {
  details <- epsg_cmpd_crs[epsg_cmpd_crs$coord_ref_sys_code == epsg_code, ]
  if (!nrow(details)) return('')

  horz <- .wkt2_crs_geo(details$cmpd_horizcrs_code)
  if (!nzchar(horz))
    horz <- .wkt2_crs_proj(details$cmpd_horizcrs_code)
  vert <- .wkt2_crs_vert(details$cmpd_vertcrs_code)

  paste0('COMPOUNDCRS["', details$coord_ref_sys_name, '",', horz, ',',
         vert, ',', .wkt2_id(details$coord_ref_sys_code), ']')
}
