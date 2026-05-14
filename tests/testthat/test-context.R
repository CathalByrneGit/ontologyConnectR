test_that("ontology_context creates OntologyContext with connector", {
  bundle <- make_test_bundle(c("Patient", "Encounter"))
  mock   <- make_mock_connector(data.frame(id = 1))
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))

  expect_s3_class(ctx, "OntologyContext")
  expect_named(ctx$connectors, "Patient")
})

test_that("ontology_context with NULL connection and all connectors is valid", {
  bundle <- make_test_bundle("Patient")
  mock   <- make_mock_connector(data.frame(id = 1))
  expect_no_error(
    ontology_context(bundle, connection = NULL,
                     connectors = list(Patient = mock))
  )
})

test_that("object_set routes to ConnectorObjectSet for connector types", {
  bundle <- make_test_bundle(c("Patient", "Encounter"))
  mock   <- make_mock_connector(data.frame(id = 1))
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))

  os <- object_set(ctx, "Patient")
  expect_s3_class(os, "ConnectorObjectSet")
})

test_that("object_set routes to DbiObjectSet for non-connector types", {
  bundle <- make_test_bundle(c("Patient", "Encounter"))
  mock   <- make_mock_connector(data.frame(id = 1))
  con    <- list()  # fake DBI connection
  ctx    <- ontology_context(bundle, connection = con,
                              connectors = list(Patient = mock))

  os <- object_set(ctx, "Encounter")
  expect_s3_class(os, "DbiObjectSet")
})

test_that("object_set errors for unknown type without connection", {
  bundle <- make_test_bundle("Patient")
  mock   <- make_mock_connector(data.frame(id = 1))
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))

  expect_error(object_set(ctx, "Unknown"), class = "rlang_error")
})

test_that("strict mode errors on unknown connector IDs", {
  bundle <- make_test_bundle("Patient")
  mock   <- make_mock_connector(data.frame(id = 1))
  expect_error(
    ontology_context(bundle, connectors = list(Unknown = mock), strict = TRUE),
    class = "rlang_error"
  )
})

test_that("print.OntologyContext outputs informative text", {
  bundle <- make_test_bundle(c("Patient", "Encounter"))
  mock   <- make_mock_connector(data.frame(id = 1))
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))
  out    <- capture.output(print(ctx))
  expect_true(any(grepl("OntologyContext", out)))
})
