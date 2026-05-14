test_that("validate_live_schema_map accepts correct schema_map", {
  sm <- make_rest_schema_map("Item", "items", list(item_id = "id"))
  expect_invisible(ontologyConnectR:::validate_live_schema_map(sm))
})

test_that("validate_live_schema_map rejects non-list", {
  expect_error(
    ontologyConnectR:::validate_live_schema_map("not a list"),
    class = "rlang_error"
  )
})

test_that("validate_live_schema_map rejects missing resource", {
  bad <- list(X = list(columns = list(id = "id")))
  expect_error(
    ontologyConnectR:::validate_live_schema_map(bad),
    class = "rlang_error"
  )
})

test_that("validate_live_schema_map rejects missing columns", {
  bad <- list(X = list(resource = "x"))
  expect_error(
    ontologyConnectR:::validate_live_schema_map(bad),
    class = "rlang_error"
  )
})

test_that("conn_from_bundle constructs live_rest from bundle extension", {
  ot <- list(
    id = "Airport",
    extensions = list(
      live_source = list(
        connector_type = "rest",
        schema_map     = list(airport_id = "id", name = "name"),
        params         = list(
          base_url     = "https://api.example.com",
          resource     = "airports",
          pk_property  = "airport_id",
          cache_ttl    = 0L
        )
      )
    )
  )
  conn <- conn_from_bundle(ot)
  expect_s4_class(conn, "LiveRestConnection")
  expect_equal(conn@config$base_url, "https://api.example.com")
})

test_that("conn_from_bundle errors on missing live_source extension", {
  ot <- list(id = "X", extensions = list())
  expect_error(conn_from_bundle(ot), class = "rlang_error")
})

test_that("conn_from_bundle errors on unknown connector_type", {
  ot <- list(id = "X", extensions = list(
    live_source = list(connector_type = "kafka", schema_map = list(), params = list())
  ))
  expect_error(conn_from_bundle(ot), class = "rlang_error")
})
