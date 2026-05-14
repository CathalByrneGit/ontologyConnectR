#' Create an ICIJ Offshore Leaks connector
#'
#' Provides access to the ICIJ Offshore Leaks database through the
#' OntologyConnector interface. Data can be loaded from local CSV files or a
#' pre-built DuckDB file. If both are NULL, loads the bundled sample fixture.
#'
#' Supports four entity types: Entity, Officer, Intermediary, Address.
#' Edge traversal via `os_traverse()` uses the included edges.csv.
#'
#' @param entity_type One of "Entity", "Officer", "Intermediary", "Address".
#' @param csv_dir Path to directory containing ICIJ CSV files, or NULL.
#' @param db_path Path to a pre-built DuckDB database file, or NULL.
#' @return A list of class `conn_icij`.
#' @export
conn_icij <- function(entity_type = "Entity",
                       csv_dir     = NULL,
                       db_path     = NULL) {
  valid_types <- c("Entity", "Officer", "Intermediary", "Address")
  if (!entity_type %in% valid_types) {
    cli::cli_abort(
      "{.arg entity_type} must be one of {.val {valid_types}}, not {.val {entity_type}}."
    )
  }

  schema_map <- icij_schema_map(entity_type)

  # Resolve data source
  data <- load_icij_data(entity_type, csv_dir, db_path)

  conn <- list(
    entity_type  = entity_type,
    schema_map   = schema_map,
    pk_property  = "node_id",
    data         = data,          # data.frame loaded in memory
    csv_dir      = csv_dir,
    db_path      = db_path,
    connector_id = paste0("icij:", entity_type)
  )
  class(conn) <- c("conn_icij", "OntologyConnector")
  conn
}

#' @export
conn_fetch.conn_icij <- function(connector, filters = list(),
                                  select = NULL, limit = NULL, offset = 0L) {
  df <- connector$data

  # Rename data columns to property IDs via schema_map (data already mapped)
  filter_fn <- translate_filters_to_r(filters)
  df        <- filter_fn(df)
  df        <- apply_select(df, select)
  df        <- apply_limit(df, limit, offset)
  df
}

#' @export
conn_count.conn_icij <- function(connector, filters = list()) {
  df <- connector$data
  filter_fn <- translate_filters_to_r(filters)
  nrow(filter_fn(df))
}

#' @export
conn_get_one.conn_icij <- function(connector, pk_value) {
  df <- connector$data
  result <- df[!is.na(df$node_id) & df$node_id == pk_value, , drop = FALSE]
  if (nrow(result) == 0L) NULL else result[1, , drop = FALSE]
}

#' Load ICIJ edges for traversal
#' @param csv_dir Directory with ICIJ CSVs.
#' @param db_path Pre-built DuckDB path.
#' @return data.frame with columns: node_id_start, node_id_end, rel_type
#' @export
icij_load_edges <- function(csv_dir = NULL, db_path = NULL) {
  if (is.null(csv_dir) && is.null(db_path)) {
    csv_dir <- system.file("fixtures", "icij_sample", package = "ontologyConnectR")
  }
  if (!is.null(csv_dir)) {
    edges_path <- file.path(csv_dir, "edges.csv")
    if (!file.exists(edges_path)) {
      cli::cli_abort("edges.csv not found in {.path {csv_dir}}.")
    }
    return(utils::read.csv(edges_path, stringsAsFactors = FALSE))
  }
  # DuckDB path would use DBI here; simplified to CSV fallback
  cli::cli_abort("db_path not yet supported; provide csv_dir instead.")
}

# ICIJ schema maps --------------------------------------------------------

icij_schema_map <- function(entity_type) {
  switch(entity_type,
    Entity = list(
      node_id        = "node_id",
      name           = "name",
      jurisdiction   = "jurisdiction",
      jurisdiction_description = "jurisdiction_description",
      company_type   = "company_type",
      address        = "address",
      internal_id    = "internal_id",
      incorporation_date = "incorporation_date",
      inactivation_date  = "inactivation_date",
      struck_off_date    = "struck_off_date",
      closed_date        = "closed_date",
      ibcRUC         = "ibcRUC",
      status         = "status",
      country_codes  = "country_codes",
      countries      = "countries",
      source_id      = "source_id",
      valid_until    = "valid_until",
      note           = "note"
    ),
    Officer = list(
      node_id        = "node_id",
      name           = "name",
      countries      = "countries",
      country_codes  = "country_codes",
      source_id      = "source_id",
      valid_until    = "valid_until",
      note           = "note"
    ),
    Intermediary = list(
      node_id        = "node_id",
      name           = "name",
      address        = "address",
      internal_id    = "internal_id",
      source_id      = "source_id",
      valid_until    = "valid_until",
      status         = "status",
      country_codes  = "country_codes",
      countries      = "countries",
      note           = "note"
    ),
    Address = list(
      node_id        = "node_id",
      address        = "address",
      icij_id        = "icij_id",
      valid_until    = "valid_until",
      country_codes  = "country_codes",
      countries      = "countries",
      source_id      = "source_id",
      note           = "note"
    )
  )
}

# Data loading ------------------------------------------------------------

load_icij_data <- function(entity_type, csv_dir, db_path) {
  if (is.null(csv_dir) && is.null(db_path)) {
    csv_dir <- system.file("fixtures", "icij_sample",
                            package = "ontologyConnectR")
    if (!nchar(csv_dir)) {
      cli::cli_abort(
        "Could not find bundled ICIJ fixture. ",
        "Provide {.arg csv_dir} or {.arg db_path}."
      )
    }
  }

  if (!is.null(csv_dir)) {
    file_name <- paste0(tolower(entity_type), "s.csv")
    # Special case: plural of "Intermediary" is "intermediaries"
    if (entity_type == "Intermediary") file_name <- "intermediaries.csv"

    csv_path <- file.path(csv_dir, file_name)
    if (!file.exists(csv_path)) {
      cli::cli_abort("CSV not found: {.path {csv_path}}")
    }
    df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
    # Ensure node_id is character
    df$node_id <- as.character(df$node_id)
    # Keep only schema_map columns that exist
    schema_map <- icij_schema_map(entity_type)
    available  <- intersect(names(schema_map), names(df))
    return(df[, available, drop = FALSE])
  }

  cli::cli_abort("db_path not yet supported; provide {.arg csv_dir}.")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
