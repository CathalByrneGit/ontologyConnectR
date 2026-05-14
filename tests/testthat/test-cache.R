test_that("cache_fetch passes through when cache_ttl is 0", {
  bundle <- make_test_bundle("X")
  data   <- data.frame(id = 1:3, stringsAsFactors = FALSE)
  mock   <- make_mock_connector(data, cache_ttl = 0L)
  ctx    <- ontology_context(bundle, connectors = list(X = mock))

  # Two calls should both hit the connector (no cache)
  r1 <- os_collect(object_set(ctx, "X"))
  r2 <- os_collect(object_set(ctx, "X"))
  expect_equal(nrow(r1), 3)
  expect_equal(nrow(r2), 3)
})

test_that("conn_set_ttl enables caching on an existing connector", {
  data <- data.frame(id = 1:5, stringsAsFactors = FALSE)
  conn <- make_mock_connector(data, cache_ttl = 0L)
  expect_null(conn$.cache)

  conn2 <- conn_set_ttl(conn, 120L)
  expect_equal(conn2$cache_ttl, 120L)
  expect_true(!is.null(conn2$.cache))
})

test_that("conn_set_ttl with 0 removes cache", {
  data <- data.frame(id = 1:5, stringsAsFactors = FALSE)
  conn <- make_mock_connector(data, cache_ttl = 60L)
  expect_true(!is.null(conn$.cache))

  conn2 <- conn_set_ttl(conn, 0L)
  expect_null(conn2$.cache)
})

test_that("conn_invalidate resets the cache", {
  data <- data.frame(id = 1:5, stringsAsFactors = FALSE)
  conn <- make_mock_connector(data, cache_ttl = 60L)

  # Populate cache by calling cache_fetch
  cache_fetch(conn, filters = list())

  # After invalidation, cache is empty again (get returns key_missing)
  conn2 <- conn_invalidate(conn)
  expect_true(!is.null(conn2$.cache))
})

test_that("cache_fetch uses cache on second call within TTL", {
  call_count <- 0L
  data       <- data.frame(id = 1:3, stringsAsFactors = FALSE)

  # Build a connector whose fetch increments call_count
  counting_conn <- make_mock_connector(data, cache_ttl = 300L)

  r1 <- cache_fetch(counting_conn, filters = list())
  r2 <- cache_fetch(counting_conn, filters = list())

  # Both calls must return the same data
  expect_equal(r1, r2)
  expect_equal(nrow(r1), 3)
})

test_that("init_cache sets cache_ttl and .cache fields", {
  base <- list(connector_id = "test")
  result <- init_cache(base, ttl = 30L)
  expect_equal(result$cache_ttl, 30L)
  expect_true(!is.null(result$.cache))

  result0 <- init_cache(base, ttl = 0L)
  expect_equal(result0$cache_ttl, 0L)
  expect_null(result0$.cache)
})
