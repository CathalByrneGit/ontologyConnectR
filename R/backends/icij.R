#' Create an ICIJ Offshore Leaks live connection
#'
#' Loads ICIJ CSV files into an in-memory DuckDB database and returns a
#' \code{LiveIcijConnection}. Requires the \code{duckdb} package. If both
#' \code{csv_dir} and \code{db_path} are NULL, uses the bundled sample fixture.
#'
#' The connection exposes four object types: Entity, Officer, Intermediary,
#' Address. An additional \code{edges} table is loaded for traversal.
#'
#' @param csv_dir Path to directory with ICIJ CSV files, or NULL.
#' @param db_path Path to a pre-built DuckDB file, or NULL.
#' @param cache_ttl Integer. Cache TTL in seconds.
#' @return A \code{LiveIcijConnection} object.
#' @export
live_icij <- function(csv_dir = NULL, db_path = NULL, cache_ttl = 0L) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg duckdb} is required for {.fn live_icij}. ",
      "Install with {.code install.packages('duckdb')}."
    )
  }

  if (is.null(csv_dir) && is.null(db_path)) {
    csv_dir <- system.file("fixtures", "icij_sample", package = "ontologyConnectR")
    if (!nchar(csv_dir)) {
      cli::cli_abort("Bundled ICIJ fixture not found. Provide {.arg csv_dir}.")
    }
  }

  ddb <- if (!is.null(db_path)) {
    duckdb::dbConnect(duckdb::duckdb(), db_path)
  } else {
    duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  }

  # Load CSV files into DuckDB
  if (!is.null(csv_dir)) {
    table_files <- list(
      entities       = "entities.csv",
      officers       = "officers.csv",
      intermediaries = "intermediaries.csv",
      addresses      = "addresses.csv",
      edges          = "edges.csv"
    )
    for (tbl in names(table_files)) {
      path <- file.path(csv_dir, table_files[[tbl]])
      if (file.exists(path)) {
        DBI::dbExecute(ddb, sprintf(
          "CREATE OR REPLACE TABLE %s AS SELECT * FROM read_csv_auto('%s')",
          tbl, path
        ))
      }
    }
  }

  sm <- icij_schema_map()
  new_live_connection(
    "LiveIcijConnection",
    source_type = "icij",
    schema_map  = sm,
    cache_ttl   = cache_ttl,
    config      = list(csv_dir = csv_dir, db_path = db_path),
    duckdb_con  = ddb
  )
}

#' @export
setMethod("dispatch_query", "LiveIcijConnection", function(conn, source_query, ...) {
  sql <- reconstruct_sql(source_query)
  DBI::dbGetQuery(conn@duckdb_con, sql)
})

#' Pre-built schema map for all ICIJ entity types
#' @keywords internal
icij_schema_map <- function() {
  list(
    Entity = list(
      resource = "entities",
      pk       = "entity_id",
      columns  = list(
        entity_id          = "node_id",
        name               = "name",
        jurisdiction       = "jurisdiction",
        jurisdiction_desc  = "jurisdiction_description",
        company_type       = "company_type",
        address            = "address",
        internal_id        = "internal_id",
        incorporation_date = "incorporation_date",
        inactivation_date  = "inactivation_date",
        struck_off_date    = "struck_off_date",
        closed_date        = "closed_date",
        status             = "status",
        country_codes      = "country_codes",
        countries          = "countries",
        source_id          = "source_id",
        valid_until        = "valid_until",
        note               = "note"
      )
    ),
    Officer = list(
      resource = "officers",
      pk       = "officer_id",
      columns  = list(
        officer_id    = "node_id",
        name          = "name",
        countries     = "countries",
        country_codes = "country_codes",
        source_id     = "source_id",
        valid_until   = "valid_until",
        note          = "note"
      )
    ),
    Intermediary = list(
      resource = "intermediaries",
      pk       = "intermediary_id",
      columns  = list(
        intermediary_id = "node_id",
        name            = "name",
        address         = "address",
        internal_id     = "internal_id",
        source_id       = "source_id",
        status          = "status",
        country_codes   = "country_codes",
        countries       = "countries",
        note            = "note"
      )
    ),
    Address = list(
      resource = "addresses",
      pk       = "address_id",
      columns  = list(
        address_id    = "node_id",
        address       = "address",
        icij_id       = "icij_id",
        country_codes = "country_codes",
        countries     = "countries",
        source_id     = "source_id",
        note          = "note"
      )
    )
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
