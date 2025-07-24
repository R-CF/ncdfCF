#' CF Standard names table
#'
#' @description The CF Metadata Conventions define a large number of standard
#'   names for physical parameters, including axes and data variables. This
#'   class accesses the standard names table. For each of the entries in the
#'   table two properties are provided: the canonical unit and a description.
#'   These properties are retrieved when searching for a given name.
#'
#'   Access to this class is through the `CF` environment. Use the
#'   `CF$standard_names$find("name_of_interest")` method to access a particular
#'   standard name. It is strongly recommended not to instantiate this class
#'   manually as that may introduce problems with accessing the underlying XML
#'   file.
#'
#'   The XML table is retrieved from the CF Metadata Conventions web site
#'   [here](https://cfconventions.org/vocabularies.html) and stored locally in
#'   the cache of the `ncdfCF` package. A check is performed periodically for an
#'   updated version, which will then be downloaded automatically. The frequency
#'   of the update check can be controlled with the
#'   `CF.options$cache_stale_days` option.
#'
#' @references
#' https://cfconventions.org/cf-conventions/cf-conventions.html#standard-name
#'
#' @docType class
CFStandardNames <- R6::R6Class("CFStandardNames",
  private = list(
    # The `xml_document` that contains all the names.
    names = NULL,

    # Open the standard names table in XML format. Returns the XML document as
    # a `xml_document` produced by package `xml2` (which must be installed). If
    # there is any error, returns `NULL`.
    # The XML file is stored locally in the package cache. Periodically there
    # will be a check to see if a new version of the table is available.
    retrieve = function() {
      if (!requireNamespace("xml2")) {
        stop("Must install package `xml2` to use the standard names table", call. = FALSE)
      }

      res <- try({
        src <- "https://cfconventions.org/Data/cf-standard-names/current/src/cf-standard-name-table.xml"
        cache <- tools::R_user_dir("ncdfCF", "cache")
        if (!dir.exists(cache)) dir.create(cache, mode = "755")

        fn <- file.path(cache, "cf-standard-name-table.xml")
        if (file.exists(fn)) {
          snx <- xml2::read_xml(fn)
          if (file.mtime(fn) < Sys.time() - CF.options$cache_stale_days * 86400) {
            # Check if there is an update to the standard_name table
            con <- url(src)
            header <- readLines(con, n = 3)  # read first 3 lines only
            close(con)
            remote_version <- as.integer(regmatches(header[3], regexpr("[0-9]+", header[3])))

            local_version <- xml2::xml_integer(xml2::xml_find_first(snx, "/standard_name_table/version_number"))

            if (local_version < remote_version) {
              snx <- NULL # Drop the reference to the file so it can be overwritten
              xml2::download_xml(src, fn)
              snx <- xml2::read_xml(fn)
            } else Sys.setFileTime(fn, Sys.time()) # Reset the clock for checking
          }
        } else {
          xml2::download_xml(src, fn)
          snx <- xml2::read_xml(fn)
        }
        snx
      }, silent = TRUE)
      private$names <- if (inherits(res, "try-error")) NULL else res
      invisible(self)
    }
  ),
  public = list(
    #' @description Initialize an instance of this class. This is done
    #'   automatically when the package is loaded.
    initialize = function() {
      # Empty function for quick start-up.
    },

    #' @description Print the version number of the standard names table in use,
    #' if it is loaded. The table is loaded automatically when it is first used.
    print = function() {
      cat("<CF standard names table> ")
      if (is.null(private$names))
        cat("(not loaded)\n")
      else
        cat("version", xml2::xml_text(xml2::xml_find_first(snx, "/standard_name_table/version_number"), "\n"))
    },

    #' @description Retrieve the information on the specified names.
    #' @param names A character vector with the names to search the standard
    #'   names table for.
    #' @return If an entry with a value in `names` is found, returns a
    #'   `data.frame` with with with the canonical units and a description of
    #'   the name. If no `names` are found in the table `NULL` is returned.
    find = function(names) {
      if (is.null(private$names)) private$retrieve()
      if (is.null(private$names))
        stop("Standard names table is not available", call. = FALSE)

      xp <- paste0("//entry[@id='", names, "']")
      nodes <- lapply(xp, function(nm) xml2::xml_find_first(private$names, nm))
      found <- which(sapply(nodes, class) == "xml_node")
      if (length(found)) {
        nodes <- nodes[found]
        cu <- sapply(nodes, function(n) xml2::xml_text(xml2::xml_find_first(n, "canonical_units")))
        desc <- sapply(nodes, function(n) xml2::xml_text(xml2::xml_find_first(n, "description")))
        data.frame(name = names[found], units = cu, description = desc)
      } else NULL
    },

    #' @description Load the standard names table so that it's contents may be
    #' used in display and analysis. Note that the table may be downloaded
    #' (4.3MB at version 91) if not available or stale.
    #' @return Self, invisibly.
    load = function() {
      private$retrieve()
      invisible(self)
    }
  ),
  active = list(
    #' @field is_loaded (read-only) Flag to determine if the standard names
    #'   table is available.
    is_loaded = function(value) {
      if (missing(value))
        !is.null(names)
    }
  )
)
