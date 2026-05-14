test_that("ontology_context creates OntologyContext with live_connections", {
  bundle <- make_test_bundle("Patient")
  sm     <- make_rest_schema_map("Patient", "patients",
                                  list(patient_id = "id", name = "name"))
  conn   <- methods::new("LiveRestConnection",
    source_type = "rest", schema_map = sm, cache = NULL, config = list())
  ctx <- ontology_context(bundle, live_connections = list(Patient = conn))
  expect_s3_class(ctx, "OntologyContext")
  expect_named(ctx$live_connections, "Patient")
})

test_that("strict mode rejects unknown type IDs in live_connections", {
  bundle <- make_test_bundle("Patient")
  sm     <- make_rest_schema_map("X", "x", list(id = "id"))
  conn   <- methods::new("LiveRestConnection",
    source_type = "rest", schema_map = sm, cache = NULL, config = list())
  expect_error(
    ontology_context(bundle, live_connections = list(X = conn), strict = TRUE),
    class = "rlang_error"
  )
})

test_that("print.OntologyContext prints informative output", {
  bundle <- make_test_bundle(c("Patient", "Encounter"))
  sm     <- make_rest_schema_map("Patient", "patients", list(id = "id"))
  conn   <- methods::new("LiveRestConnection",
    source_type = "rest", schema_map = sm, cache = NULL, config = list())
  ctx <- ontology_context(bundle, live_connections = list(Patient = conn))
  out <- capture.output(print(ctx))
  expect_true(any(grepl("OntologyContext", out)))
})

test_that("object_set routes live type to dplyr::tbl via LiveConnection", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("duckdb")

  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn   <- live_icij(csv_dir = fixture_dir)
  bundle <- list(
    object_types = list(Entity = list(id = "Entity", extensions = list())),
    link_types   = list()
  )
  ctx <- ontology_context(bundle, live_connections = list(Entity = conn))
  os  <- object_set(ctx, "Entity")
  expect_true(inherits(os, "tbl"))
})
