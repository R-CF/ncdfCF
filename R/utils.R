# NetCDF data types
# Additionally, UDTs are allowable as data type
netcdf_data_types <- c("NC_BYTE", "NC_UBYTE", "NC_CHAR", "NC_SHORT",
                       "NC_USHORT", "NC_INT", "NC_UINT", "NC_INT64",
                       "NC_UINT64", "NC_FLOAT", "NC_DOUBLE", "NC_STRING")

# Standard names used for parametric Z axes
Z_parametric_standard_names <- c("atmosphere_ln_pressure_coordinate",
  "atmosphere_sigma_coordinate", "atmosphere_hybrid_sigma_pressure_coordinate",
  "atmosphere_hybrid_height_coordinate", "atmosphere_sleve_coordinate",
  "ocean_sigma_coordinate", "ocean_s_coordinate", "ocean_s_coordinate_g1",
  "ocean_s_coordinate_g2", "ocean_sigma_z_coordinate", "ocean_double_sigma_coordinate")

#' Make a data.frame slimmer by shortening long strings. List elements are
#' pasted together.
#'
#' @param df A data.frame
#' @param width Maximum width of character entries. If entries are longer than
#' width - 3, they are truncated and then '...' added.
#'
#' @return data.frame with slim columns
#' @noRd
.slim.data.frame <- function(df, width = 50L) {
  maxw <- width - 3L
  out <- as.data.frame(lapply(df, function(c) {
    if (is.list(c)) c <- sapply(c, paste0, collapse = ", ")
    if (!is.character(c)) c
    else
      sapply(c, function(e)
        if (nchar(e) > width) paste0(substr(e, 1, maxw), "...") else e
      )
  }))
  names(out) <- names(df)
  out
}

#' Flags if the supplied name is a valid name according to the CF Metadata
#' Conventions.
#'
#' @param nm The name of a variable, group or attribute to test. Group names
#' should be plain, i.e. no preceding path.
#' @returns `TRUE` if `nm` is valid, `FALSE` otherwise.
#' @noRd
.is_valid_name <- function(nm) {
  grepl("^[a-zA-Z][a-zA-Z0-9_]{0,254}$", nm)
}

.cache_dir <- function() {
  if (as.integer(R.version$major) >= 4)
    tools::R_user_dir("ncdfCF", "cache")
  else {
    if (nzchar(p <- Sys.getenv("R_USER_CACHE_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CACHE_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
    else if (Sys.info()["sysname"] == "Darwin") file.path(normalizePath("~"), "Library", "Caches", "org.R-project.R")
    else file.path(normalizePath("~"), ".cache")
  }
}

# Update or set the "history" attribute in a data.frame of attributes
.make_history <- function(atts, history) {
  h <- atts[atts$name == "history", "value"]
  if (length(h)) {
    h <- paste0(history, "; ", h)
    atts[atts$name == "history", "value"] <- h
    atts[atts$name == "history", "length"] <- nchar(h)
  } else
    atts <- rbind(atts, data.frame(id = max(atts$id) + 1L, name = "history", type = "NC_CHAR", length = nchar(h), value = h))
  atts
}

unused_imports <- function() {
  stringr::word
}
