#' Translate a list of filter specs into an R post-filtering function
#'
#' Used as a fallback when a connector cannot push a predicate to the source.
#' Each filter spec is \code{list(property, op, value)}.
#'
#' @param filters A list of filter specs.
#' @return A function \code{function(df) -> df}.
#' @export
translate_filters_to_r <- function(filters) {
  if (!length(filters)) return(identity)
  fns <- lapply(filters, make_filter_fn)
  function(df) {
    for (fn in fns) {
      df <- fn(df)
      if (!nrow(df)) break
    }
    df
  }
}

make_filter_fn <- function(f) {
  prop  <- f$property %||% f$column %||% f$source_column
  op    <- f$op
  value <- f$value

  switch(op,
    "eq"         = , "=" =
      function(df) df[!is.na(df[[prop]]) & df[[prop]] == value,  , drop = FALSE],
    "neq"        = , "!=" =
      function(df) df[!is.na(df[[prop]]) & df[[prop]] != value,  , drop = FALSE],
    "gt"         = , ">" =
      function(df) df[!is.na(df[[prop]]) & df[[prop]] >  value,  , drop = FALSE],
    "gte"        = , ">=" =
      function(df) df[!is.na(df[[prop]]) & df[[prop]] >= value,  , drop = FALSE],
    "lt"         = , "<" =
      function(df) df[!is.na(df[[prop]]) & df[[prop]] <  value,  , drop = FALSE],
    "lte"        = , "<=" =
      function(df) df[!is.na(df[[prop]]) & df[[prop]] <= value,  , drop = FALSE],
    "contains"   =
      function(df) df[!is.na(df[[prop]]) &
                        grepl(value, df[[prop]], fixed = TRUE), , drop = FALSE],
    "starts_with" =
      function(df) df[!is.na(df[[prop]]) & startsWith(df[[prop]], value), , drop = FALSE],
    "ends_with"  =
      function(df) df[!is.na(df[[prop]]) & endsWith(df[[prop]], value),   , drop = FALSE],
    "in"         = , "IN" =
      function(df) df[!is.na(df[[prop]]) & df[[prop]] %in% value, , drop = FALSE],
    "not_in"     = , "NOT IN" =
      function(df) df[is.na(df[[prop]])  | !df[[prop]] %in% value, , drop = FALSE],
    "is_null"    = , "IS NULL" =
      function(df) df[is.na(df[[prop]]), , drop = FALSE],
    "not_null"   = , "IS NOT NULL" =
      function(df) df[!is.na(df[[prop]]), , drop = FALSE],
    "LIKE"       = {
      pat <- gsub("%", ".*", gsub("_", ".", value, fixed = TRUE), fixed = TRUE)
      function(df) df[grepl(pat, df[[prop]]), , drop = FALSE]
    },
    identity  # RAW_SQL or unknown — skip
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
