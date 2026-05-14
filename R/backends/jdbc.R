#' Create a JDBC live connection
#'
#' Connects to JDBC-accessible databases using RJDBC. The dbplyr-generated
#' SQL (DuckDB dialect) is rewritten using the schema_map and optionally
#' translated to the target dialect via \code{sqlglotR::sg_translate()} if
#' that package is installed.
#'
#' @param schema_map Named list (same format as \code{live_rest}).
#' @param driver_class Java JDBC driver class name.
#' @param jdbc_url JDBC connection URL.
#' @param credentials List with \code{username} and \code{password_env_var}.
#' @param driver_path Path to the JDBC driver JAR, or NULL.
#' @param dialect Target SQL dialect for \code{sg_translate()}.
#'   Common values: \code{"tsql"}, \code{"oracle"}, \code{"postgres"}.
#'   Defaults to \code{"duckdb"} (no translation).
#' @param cache_ttl Integer. Cache TTL in seconds.
#' @return A \code{LiveJdbcConnection} object.
#' @export
live_jdbc <- function(schema_map,
                       driver_class,
                       jdbc_url,
                       credentials = list(username = NULL, password_env_var = NULL),
                       driver_path = NULL,
                       dialect     = "duckdb",
                       cache_ttl   = 0L) {
  validate_live_schema_map(schema_map)
  new_live_connection(
    "LiveJdbcConnection",
    source_type = "jdbc",
    schema_map  = schema_map,
    cache_ttl   = cache_ttl,
    config = list(
      driver_class = driver_class,
      jdbc_url     = jdbc_url,
      credentials  = credentials,
      driver_path  = driver_path,
      dialect      = dialect
    )
  )
}

#' @export
setMethod("dispatch_query", "LiveJdbcConnection", function(conn, source_query, ...) {
  if (!requireNamespace("RJDBC", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg RJDBC} is required for JDBC connections.")
  }

  # Rebuild SQL from mapped source fields
  sql <- reconstruct_sql(source_query)

  # Optionally translate to target dialect
  dialect <- conn@config$dialect %||% "duckdb"
  if (dialect != "duckdb" && requireNamespace("sqlglotR", quietly = TRUE)) {
    sql <- sqlglotR::sg_translate(sql, from = "duckdb", to = dialect)
  }

  jdbc_con <- open_jdbc_con(conn@config)
  on.exit(try(RJDBC::dbDisconnect(jdbc_con), silent = TRUE), add = TRUE)
  RJDBC::dbGetQuery(jdbc_con, sql)
})

open_jdbc_con <- function(cfg) {
  drv <- RJDBC::JDBC(cfg$driver_class, cfg$driver_path)
  pw  <- if (!is.null(cfg$credentials$password_env_var)) {
    Sys.getenv(cfg$credentials$password_env_var, unset = NA_character_)
  } else {
    NA_character_
  }
  if (!is.na(pw) && !is.null(cfg$credentials$username)) {
    RJDBC::dbConnect(drv, cfg$jdbc_url, cfg$credentials$username, pw)
  } else {
    RJDBC::dbConnect(drv, cfg$jdbc_url)
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a
