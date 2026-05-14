#' Translate a list of filter specs into an R post-filtering function
#'
#' Used by connectors that cannot push predicates natively. Returns a function
#' that takes a data.frame and returns a filtered data.frame.
#'
#' @param filters A list of filter specs (property, op, value).
#' @return A function(df) -> df.
#' @export
translate_filters_to_r <- function(filters) {
  if (length(filters) == 0L) {
    return(identity)
  }

  fns <- lapply(filters, make_filter_fn)

  function(df) {
    for (fn in fns) {
      df <- fn(df)
      if (nrow(df) == 0L) break
    }
    df
  }
}

make_filter_fn <- function(f) {
  prop  <- f$property
  op    <- f$op
  value <- f$value

  switch(op,
    eq         = function(df) df[!is.na(df[[prop]]) & df[[prop]] == value, , drop = FALSE],
    neq        = function(df) df[!is.na(df[[prop]]) & df[[prop]] != value, , drop = FALSE],
    gt         = function(df) df[!is.na(df[[prop]]) & df[[prop]] >  value, , drop = FALSE],
    gte        = function(df) df[!is.na(df[[prop]]) & df[[prop]] >= value, , drop = FALSE],
    lt         = function(df) df[!is.na(df[[prop]]) & df[[prop]] <  value, , drop = FALSE],
    lte        = function(df) df[!is.na(df[[prop]]) & df[[prop]] <= value, , drop = FALSE],
    contains   = function(df) df[!is.na(df[[prop]]) &
                                   grepl(value, df[[prop]], fixed = TRUE), , drop = FALSE],
    starts_with = function(df) df[!is.na(df[[prop]]) &
                                    startsWith(df[[prop]], value), , drop = FALSE],
    ends_with  = function(df) df[!is.na(df[[prop]]) &
                                   endsWith(df[[prop]], value), , drop = FALSE],
    `in`       = function(df) df[!is.na(df[[prop]]) & df[[prop]] %in% value, , drop = FALSE],
    not_in     = function(df) df[is.na(df[[prop]])  | !df[[prop]] %in% value, , drop = FALSE],
    is_null    = function(df) df[is.na(df[[prop]]), , drop = FALSE],
    not_null   = function(df) df[!is.na(df[[prop]]), , drop = FALSE],
    cli::cli_abort("Unknown filter op: {.val {op}}")
  )
}

#' Apply a select list to a data.frame
#' @param df data.frame
#' @param select character vector or NULL
#' @return data.frame with only selected columns (if select is not NULL)
apply_select <- function(df, select) {
  if (is.null(select) || length(select) == 0L) return(df)
  cols <- intersect(select, names(df))
  df[, cols, drop = FALSE]
}

#' Apply a limit (and offset) to a data.frame
#' @param df data.frame
#' @param limit integer or NULL
#' @param offset integer
#' @return data.frame
apply_limit <- function(df, limit, offset = 0L) {
  n <- nrow(df)
  start <- min(offset + 1L, n + 1L)
  if (is.null(limit)) {
    df[seq(start, n), , drop = FALSE]
  } else {
    end <- min(offset + limit, n)
    if (start > end) df[integer(0), , drop = FALSE] else df[seq(start, end), , drop = FALSE]
  }
}
