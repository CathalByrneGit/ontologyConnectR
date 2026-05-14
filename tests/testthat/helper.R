# Test helpers for ontologyConnectR

# Minimal bundle for tests
make_test_bundle <- function(object_type_ids = c("Patient"),
                              link_type_ids  = character(0)) {
  object_types <- stats::setNames(
    lapply(object_type_ids, function(id) {
      list(id = id, table_name = tolower(id), extensions = list())
    }),
    object_type_ids
  )
  link_types <- if (length(link_type_ids)) {
    stats::setNames(
      lapply(link_type_ids, function(id) {
        parts <- strsplit(id, "_to_")[[1]]
        list(id = id, source_type_id = parts[1], target_type_id = parts[2],
             source_pk = "id", target_pk = "id")
      }),
      link_type_ids
    )
  } else list()
  list(object_types = object_types, link_types = link_types)
}

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
