make_rest_conn <- function(type_id = "Patient") {
  sm <- make_rest_schema_map(type_id, tolower(type_id), list(id = "id"))
  methods::new("LiveRestConnection",
    source_type = "rest", schema_map = sm, cache = NULL, config = list())
}

test_that("connect_context creates a ConnectContext with live_connections", {
  bundle <- make_test_bundle("Patient")
  ctx <- connect_context(bundle, live_connections = list(Patient = make_rest_conn()))
  expect_s3_class(ctx, "ConnectContext")
  expect_s3_class(ctx$bundle, "ontology_bundle")
  expect_named(ctx$live_connections, "Patient")
})

test_that("connect_context lists every object type of spec_example_bundle()", {
  b   <- ontologySpecR::spec_example_bundle()
  ids <- names(ontologySpecR::bundle_object_types(b))
  expect_gt(length(ids), 0)
  expect_warning(
    ctx <- connect_context(b),
    "no live connection and no DBI connection"
  )
  out <- cli::cli_fmt(print(ctx))
  expect_true(any(grepl(paste0("Object types: ", length(ids)), out)))
  expect_true(any(grepl("DBI-backed", out)))
  for (id in ids) expect_true(any(grepl(id, out, fixed = TRUE)))
})

test_that("connect_context accepts a bundle JSON file", {
  b    <- make_test_bundle(c("Patient", "Encounter"))
  path <- withr::local_tempfile(fileext = ".json")
  ontologySpecR::write_bundle(b, path)
  ctx <- connect_context(path, connection = "placeholder")
  expect_setequal(names(ontologySpecR::bundle_object_types(ctx$bundle)),
                  c("Patient", "Encounter"))
})

test_that("connect_context rejects the old object_types layout", {
  old <- list(object_types = list(Patient = list(id = "Patient")))
  expect_error(connect_context(old), "old .*object_types.* layout")
})

test_that("strict = TRUE rejects a live connection for a type not in the bundle", {
  bundle <- make_test_bundle("Patient")
  expect_error(
    connect_context(bundle,
                    live_connections = list(Patient = make_rest_conn(),
                                            X = make_rest_conn("X")),
                    strict = TRUE),
    "Unknown object type IDs.*X"
  )
  # Not strict: allowed
  expect_s3_class(
    connect_context(bundle, live_connections = list(Patient = make_rest_conn(),
                                                    X = make_rest_conn("X"))),
    "ConnectContext"
  )
})

test_that("print.ConnectContext prints informative output", {
  bundle <- make_test_bundle(c("Patient", "Encounter"))
  expect_warning(
    ctx <- connect_context(bundle, live_connections = list(Patient = make_rest_conn())),
    "no live connection"
  )
  out <- cli::cli_fmt(print(ctx))
  expect_true(any(grepl("ConnectContext", out)))
  expect_true(any(grepl("Encounter", out)))
})

test_that("connect_object_set routes a live type to dplyr::tbl via LiveConnection", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("duckdb")

  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  ctx  <- connect_context(make_test_bundle("Entity"),
                          live_connections = list(Entity = conn))
  os   <- connect_object_set(ctx, "Entity")
  expect_true(inherits(os, "tbl"))
})

test_that("connect_object_set errors on unknown object types", {
  bundle <- make_test_bundle("Patient")
  ctx <- connect_context(bundle, connection = "placeholder")
  expect_error(connect_object_set(ctx, "Nope"), "Nope")
  expect_error(connect_object_set(list(), "Patient"), "ConnectContext")
})

# ---- Removed names -----------------------------------------------------------

test_that("the old names are not exported, so objectSetsR is not masked", {
  exports <- getNamespaceExports("ontologyConnectR")
  expect_false(any(c("ontology_context", "object_set", "os_collect") %in% exports))
})
