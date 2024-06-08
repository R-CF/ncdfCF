#' Make a data.frame slimmer by shortening long strings. List elements are
#' pasted together.
#'
#' @param df A data.frame
#' @param width Maximum width of character entries. If entries are longer than
#' width - 3, they are truncated and then '...' added.
#'
#' @returns data.frame with slim columns
#' @noRd
.slim.data.frame <- function(df, width = globals$df_column_width) {
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

ignore_unused_imports <- function() {
  stringr::word
}
