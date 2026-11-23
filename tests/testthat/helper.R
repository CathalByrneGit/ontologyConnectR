# Test helpers for ontologyConnectR

# Build one spec object type with an `id` primary key and a `name` property.
# `source_table` defaults to tolower(id); pass `source_sql` for a query-backed
# type. Live-source settings go in `live_source` (stored in
# extensions$live_source).
make_test_object_type <- function(id,
                                  source_table = tolower(id),
                                  source_sql   = NULL,
                                  source_schema = NULL,
                                  live_source  = NULL) {
  extensions <- if (is.null(live_source)) list() else list(live_source = live_source)
  if (!is.null(source_sql)) {
    ontologySpecR::object_type(
      id,
      properties  = list(ontologySpecR::property_def("id", "integer"),
                         ontologySpecR::property_def("name", "string")),
      primary_key = "id",
      source_kind = "query",
      source_sql  = source_sql,
      extensions  = extensions
    )
  } else {
    ontologySpecR::object_type(
      id,
      properties    = list(ontologySpecR::property_def("id", "integer"),
                           ontologySpecR::property_def("name", "string")),
      primary_key   = "id",
      source_kind   = "table",
      source_table  = source_table,
      source_schema = source_schema,
      extensions    = extensions
    )
  }
}

# Minimal spec bundle for tests. Link type ids are "<From>_to_<To>"; each
# joins From.<to>_id to To.id.
make_test_bundle <- function(object_type_ids = c("Patient"),
                             link_type_ids  = character(0),
                             objects        = NULL) {
  objects <- objects %||%
    lapply(object_type_ids, make_test_object_type)
  links <- lapply(link_type_ids, function(id) {
    parts <- strsplit(id, "_to_")[[1]]
    ontologySpecR::link_type(
      id,
      from           = parts[1],
      to             = parts[2],
      cardinality    = "many-to-one",
      join_from_keys = paste0(tolower(parts[2]), "_id"),
      join_to_keys   = "id"
    )
  })
  ontologySpecR::bundle("test-bundle", "0.0.1", objects = objects, links = links)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Build a live_rest schema_map for a single object type
make_rest_schema_map <- function(type_id, resource, columns,
                                  pk = names(columns)[1]) {
  stats::setNames(
    list(list(resource = resource, pk = pk, columns = columns)),
    type_id
  )
}

# Path to bundled ICIJ fixture
icij_fixture_dir <- function() {
  system.file("fixtures", "icij_sample", package = "ontologyConnectR")
}
