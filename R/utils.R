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
#' @returns data.frame with slim columns
#' @noRd
.slim.data.frame <- function(df, width = 50) {
  maxw <- width - 3
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

unused_imports <- function() {
  stringr::word
}
