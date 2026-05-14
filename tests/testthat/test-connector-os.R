test_that("connector_object_set returns ConnectorObjectSet", {
  bundle <- make_test_bundle("Patient")
  data   <- data.frame(id = 1:5, name = letters[1:5], stringsAsFactors = FALSE)
  mock   <- make_mock_connector(data, pk_property = "id")
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))

  os <- object_set(ctx, "Patient")
  expect_s3_class(os, "ConnectorObjectSet")
  expect_s3_class(os, "ObjectSet")
})

test_that("os_filter accumulates filters without executing", {
  bundle <- make_test_bundle("Patient")
  data   <- data.frame(id = 1:5, name = c("Alice","Bob","Alice","Dave","Eve"),
                        stringsAsFactors = FALSE)
  mock <- make_mock_connector(data)
  ctx  <- ontology_context(bundle, connectors = list(Patient = mock))

  os <- object_set(ctx, "Patient")
  expect_length(os$pending_filters, 0)

  os2 <- os_filter(os, "name", "eq", "Alice")
  expect_length(os2$pending_filters, 1)
  expect_equal(os2$pending_filters[[1]]$property, "name")
  expect_equal(os2$pending_filters[[1]]$op,       "eq")
  expect_equal(os2$pending_filters[[1]]$value,    "Alice")

  # Chaining adds a second filter
  os3 <- os_filter(os2, "id", "gt", 1)
  expect_length(os3$pending_filters, 2)
})

test_that("os_collect executes fetch exactly once", {
  bundle <- make_test_bundle("Patient")
  data   <- data.frame(id = 1:5, name = c("Alice","Bob","Alice","Dave","Eve"),
                        stringsAsFactors = FALSE)
  mock <- make_mock_connector(data)
  ctx  <- ontology_context(bundle, connectors = list(Patient = mock))

  os <- object_set(ctx, "Patient") |>
    os_filter("name", "eq", "Alice") |>
    os_filter("id", "gt", 1L)

  result <- os_collect(os)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Alice")
  expect_equal(result$id, 3L)
})

test_that("os_collect with no filters returns all rows", {
  bundle <- make_test_bundle("Patient")
  data   <- data.frame(id = 1:10, val = rnorm(10))
  mock   <- make_mock_connector(data)
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))

  result <- os_collect(object_set(ctx, "Patient"))
  expect_equal(nrow(result), 10)
})

test_that("os_count returns correct count", {
  bundle <- make_test_bundle("Patient")
  data   <- data.frame(id = 1:5, name = c("A","B","A","C","A"),
                        stringsAsFactors = FALSE)
  mock <- make_mock_connector(data)
  ctx  <- ontology_context(bundle, connectors = list(Patient = mock))

  n <- os_count(os_filter(object_set(ctx, "Patient"), "name", "eq", "A"))
  expect_equal(n, 3L)
})

test_that("os_show_query returns human-readable string", {
  bundle <- make_test_bundle("Patient")
  mock   <- make_mock_connector(data.frame(id = 1))
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))

  os  <- os_filter(object_set(ctx, "Patient"), "id", "gt", 5)
  out <- capture.output(os_show_query(os))
  expect_true(any(grepl("ConnectorObjectSet", out)))
  expect_true(any(grepl("id gt 5", out)))
})

test_that("os_limit sets pending_limit", {
  bundle <- make_test_bundle("Patient")
  mock   <- make_mock_connector(data.frame(id = 1:100))
  ctx    <- ontology_context(bundle, connectors = list(Patient = mock))

  os     <- os_limit(object_set(ctx, "Patient"), 10L)
  result <- os_collect(os)
  expect_equal(nrow(result), 10)
})

test_that("in-memory filtering handles all operators", {
  bundle <- make_test_bundle("X")
  data   <- data.frame(
    val = c(1, 2, 3, 4, 5),
    tag = c("alpha", "beta", "alpha", "gamma", "beta"),
    stringsAsFactors = FALSE
  )
  mock <- make_mock_connector(data)
  ctx  <- ontology_context(bundle, connectors = list(X = mock))
  os   <- object_set(ctx, "X")

  expect_equal(nrow(os_collect(os_filter(os, "val", "gt",  3))), 2)
  expect_equal(nrow(os_collect(os_filter(os, "val", "gte", 3))), 3)
  expect_equal(nrow(os_collect(os_filter(os, "val", "lt",  3))), 2)
  expect_equal(nrow(os_collect(os_filter(os, "val", "lte", 3))), 3)
  expect_equal(nrow(os_collect(os_filter(os, "val", "neq", 3))), 4)
  expect_equal(nrow(os_collect(os_filter(os, "tag", "in",  c("alpha", "gamma")))), 3)
  expect_equal(nrow(os_collect(os_filter(os, "tag", "contains", "lph"))), 2)
})
