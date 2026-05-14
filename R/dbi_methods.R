#' @include live_connection.R execute.R
NULL

# Connection validity and lifecycle ---------------------------------------

#' @export
setMethod("dbIsValid", "LiveConnection", function(dbObj, ...) TRUE)

#' @export
setMethod("dbDisconnect", "LiveConnection", function(conn, ...) invisible(NULL))

#' @export
setMethod("dbDisconnect", "LiveIcijConnection", function(conn, ...) {
  if (!is.null(conn@duckdb_con)) {
    try(DBI::dbDisconnect(conn@duckdb_con), silent = TRUE)
  }
  invisible(NULL)
})

#' @export
setMethod("dbGetInfo", "LiveConnection", function(dbObj, ...) {
  list(
    db.version    = NA_character_,
    dbname        = dbObj@source_type,
    username      = NA_character_,
    host          = NA_character_,
    port          = NA_integer_
  )
})

# Table enumeration -------------------------------------------------------

#' @export
setMethod("dbListTables", "LiveConnection", function(conn, ...) {
  names(conn@schema_map)
})

#' @export
setMethod("dbExistsTable", "LiveConnection", function(conn, name, ...) {
  name %in% names(conn@schema_map)
})

# Query execution — the key integration point ----------------------------

#' @export
setMethod("dbGetQuery", "LiveConnection", function(conn, statement, ...) {
  live_execute(conn, statement)
})

#' @export
setMethod("dbSendQuery", "LiveConnection", function(conn, statement, ...) {
  df <- live_execute(conn, statement)
  new("LiveResult", data = df, fetched = FALSE)
})

# LiveResult methods ------------------------------------------------------

#' @export
setMethod("dbFetch", "LiveResult", function(res, n = -1, ...) {
  if (n < 0L || n >= nrow(res@data)) res@data else head(res@data, n)
})

#' @export
setMethod("dbClearResult", "LiveResult", function(res, ...) invisible(NULL))

#' @export
setMethod("dbHasCompleted", "LiveResult", function(res, ...) TRUE)

#' @export
setMethod("dbColumnInfo", "LiveResult", function(res, ...) {
  data.frame(
    name  = names(res@data),
    type  = vapply(res@data, function(col) class(col)[1], character(1)),
    stringsAsFactors = FALSE
  )
})

# SQL quoting ------------------------------------------------------------

#' @export
setMethod(
  "dbQuoteIdentifier",
  signature("LiveConnection", "character"),
  function(conn, x, ...) {
    DBI::SQL(paste0('"', gsub('"', '""', x), '"'))
  }
)

#' @export
setMethod(
  "dbQuoteString",
  signature("LiveConnection", "character"),
  function(conn, x, ...) {
    x[is.na(x)] <- "NULL"
    DBI::SQL(paste0("'", gsub("'", "''", x), "'"))
  }
)

#' @export
setMethod(
  "dbDataType",
  signature("LiveConnection", "ANY"),
  function(dbObj, obj, ...) {
    if (is.integer(obj))   return("INT")
    if (is.numeric(obj))   return("FLOAT")
    if (is.logical(obj))   return("BOOLEAN")
    if (inherits(obj, "Date")) return("DATE")
    if (inherits(obj, "POSIXct")) return("TIMESTAMP")
    "TEXT"
  }
)

# show / print -----------------------------------------------------------

#' @export
setMethod("show", "LiveConnection", function(object) {
  cli::cli_h2("{.cls {class(object)[1]}} [{object@source_type}]")
  cli::cli_bullets(c(
    "*" = "Object types: {.val {names(object@schema_map)}}",
    "*" = "Cache: {if (is.null(object@cache)) 'disabled' else 'enabled'}"
  ))
  invisible(object)
})

#' @export
setMethod("show", "LiveIcijConnection", function(object) {
  cli::cli_h2("{.cls LiveIcijConnection} [icij]")
  cli::cli_bullets(c(
    "*" = "Object types: {.val {names(object@schema_map)}}",
    "*" = "DuckDB: {if (!is.null(object@duckdb_con)) 'connected' else 'disconnected'}"
  ))
  invisible(object)
})
