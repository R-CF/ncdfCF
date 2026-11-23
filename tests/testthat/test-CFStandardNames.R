# CFStandardNames: the CF standard names table. The table is not downloaded in
# these tests: a small table is put in a temporary cache directory instead.

.sn_table <- function(version = 99L) {
  paste0('<?xml version="1.0"?>\n<standard_name_table>\n<version_number>', version, '</version_number>',
         '<entry id="air_temperature"><canonical_units>K</canonical_units>',
         '<description>Air temperature</description></entry>',
         '<entry id="air_pressure"><canonical_units>Pa</canonical_units>',
         '<description>Air pressure</description></entry></standard_name_table>')
}

# Run `code` with the cache directory of the package in a temporary directory
# that holds a fresh standard names table.
.with_cached_table <- function(code) {
  skip_if_not_installed("xml2")
  old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  tmp <- tempfile()
  dir.create(file.path(tmp, "R", "ncdfCF"), recursive = TRUE)
  writeLines(.sn_table(), file.path(tmp, "R", "ncdfCF", "cf-standard-name-table.xml"))
  Sys.setenv(R_USER_CACHE_DIR = tmp)
  on.exit(if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old))
  code
}

test_that("A new standard names table is not loaded", {
  sn <- CFStandardNames$new()
  expect_false(sn$is_loaded)
  expect_output(print(sn), "(not loaded)", fixed = TRUE)
})

test_that("The standard names table is read from the cache", {
  .with_cached_table({
    sn <- CFStandardNames$new()
    expect_identical(sn$load(), sn)
    expect_true(sn$is_loaded)
    expect_output(print(sn), "version 99")
  })
})

test_that("Standard names are found in the table", {
  .with_cached_table({
    sn <- CFStandardNames$new()
    res <- sn$find(c("air_pressure", "no_such_name", "air_temperature"))
    expect_equal(res$name, c("air_pressure", "air_temperature"))
    expect_equal(res$units, c("Pa", "K"))
    expect_equal(res$description, c("Air pressure", "Air temperature"))
    expect_null(sn$find("no_such_name"))
  })
})
