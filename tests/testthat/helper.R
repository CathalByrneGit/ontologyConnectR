# Test helper: shared fixtures and utilities for ontologyConnectR tests

# Path to the bundled ICIJ sample fixture
icij_fixture_dir <- function() {
  system.file("fixtures", "icij_sample", package = "ontologyConnectR")
}

# Minimal ontology bundle for testing
make_test_bundle <- function(object_type_ids = c("Patient", "Encounter"),
                              link_type_ids  = character(0)) {
  object_types <- stats::setNames(
    lapply(object_type_ids, function(id) {
      list(id = id, table_name = tolower(id), extensions = list())
    }),
    object_type_ids
  )
  link_types <- if (length(link_type_ids) > 0) {
    stats::setNames(
      lapply(link_type_ids, function(id) {
        parts <- strsplit(id, "_to_")[[1]]
        list(
          id             = id,
          source_type_id = parts[1],
          target_type_id = parts[2],
          source_pk      = "id",
          target_pk      = "id"
        )
      }),
      link_type_ids
    )
  } else {
    list()
  }
  list(object_types = object_types, link_types = link_types)
}

# A trivial in-memory connector backed by a fixed data.frame
make_mock_connector <- function(data, pk_property = "id",
                                 supported_ops = character(0),
                                 cache_ttl = 0L) {
  conn <- list(
    data         = data,
    pk_property  = pk_property,
    schema_map   = stats::setNames(as.list(names(data)), names(data)),
    connector_id = "mock",
    cache_ttl    = as.integer(cache_ttl),
    .cache       = if (cache_ttl > 0L) cachem::cache_mem(max_age = cache_ttl) else NULL,
    .supported_ops = supported_ops
  )
  class(conn) <- c("conn_mock", "OntologyConnector")
  conn
}

conn_fetch.conn_mock <- function(connector, filters = list(),
                                  select = NULL, limit = NULL, offset = 0L) {
  df        <- connector$data
  filter_fn <- translate_filters_to_r(filters)
  df        <- filter_fn(df)
  df        <- apply_select(df, select)
  df        <- apply_limit(df, limit, offset)
  df
}

conn_count.conn_mock <- function(connector, filters = list()) {
  nrow(conn_fetch(connector, filters = filters))
}

conn_supports_filter.conn_mock <- function(connector, op) {
  op %in% connector$.supported_ops
}

# Register mock connector S3 methods
registerS3method("conn_fetch",           "conn_mock", conn_fetch.conn_mock)
registerS3method("conn_count",           "conn_mock", conn_count.conn_mock)
registerS3method("conn_supports_filter", "conn_mock", conn_supports_filter.conn_mock)
