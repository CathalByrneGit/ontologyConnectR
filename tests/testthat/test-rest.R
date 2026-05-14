# REST connector tests use httptest2 for network mocking.
# Each test runs inside with_mock_api() which intercepts httr2 requests
# and returns pre-recorded fixtures.

test_that("conn_rest constructor validates schema_map", {
  expect_error(
    conn_rest("https://api.example.com", "/items", "/items/{pk}",
               schema_map = list("bad" = 123L),  # non-character value
               pk_property = "id"),
    class = "rlang_error"
  )
})

test_that("conn_rest builds correct object", {
  conn <- conn_rest(
    base_url    = "https://api.example.com",
    list_path   = "/patients",
    get_path    = "/patients/{pk}",
    schema_map  = list(id = "id", name = "name"),
    pk_property = "id"
  )
  expect_s3_class(conn, "conn_rest")
  expect_s3_class(conn, "OntologyConnector")
  expect_equal(conn$base_url, "https://api.example.com")
  expect_null(conn$.cache)
})

test_that("conn_rest with cache_ttl initialises cache", {
  conn <- conn_rest(
    base_url    = "https://api.example.com",
    list_path   = "/items",
    get_path    = "/items/{pk}",
    schema_map  = list(id = "id"),
    pk_property = "id",
    cache_ttl   = 60L
  )
  expect_equal(conn$cache_ttl, 60L)
  expect_true(!is.null(conn$.cache))
})

test_that("conn_supports_filter.conn_rest: eq supported, gt not", {
  conn <- conn_rest("https://x.com", "/a", "/a/{pk}",
                     schema_map = list(id = "id"), pk_property = "id")
  expect_true(conn_supports_filter(conn, "eq"))
  expect_false(conn_supports_filter(conn, "gt"))
  expect_false(conn_supports_filter(conn, "contains"))
})

test_that("conn_auth_bearer stores token and env_var", {
  auth <- conn_auth_bearer(token = "tok123")
  expect_s3_class(auth, "conn_auth_bearer")
  expect_equal(auth$token, "tok123")

  auth2 <- conn_auth_bearer(env_var = "MY_TOKEN")
  expect_equal(auth2$env_var, "MY_TOKEN")
})

test_that("conn_auth_basic stores username and password_env_var", {
  auth <- conn_auth_basic("alice", "MY_PASS")
  expect_s3_class(auth, "conn_auth_basic")
  expect_equal(auth$username, "alice")
  expect_equal(auth$password_env_var, "MY_PASS")
})

test_that("conn_pagination_offset stores params", {
  pag <- conn_pagination_offset(page_param = "pg", size_param = "sz",
                                 page_size = 50L)
  expect_s3_class(pag, "conn_pagination_offset")
  expect_equal(pag$page_size, 50L)
})

test_that("translate_filters_to_r applies eq filter correctly", {
  df  <- data.frame(country = c("IE", "UK", "IE"), stringsAsFactors = FALSE)
  fn  <- translate_filters_to_r(list(list(property = "country", op = "eq",
                                          value = "IE")))
  out <- fn(df)
  expect_equal(nrow(out), 2)
  expect_true(all(out$country == "IE"))
})

test_that("translate_filters_to_r returns identity for empty filters", {
  df <- data.frame(x = 1:5)
  fn <- translate_filters_to_r(list())
  expect_identical(fn(df), df)
})

test_that("apply_limit trims to requested rows from offset", {
  df <- data.frame(x = 1:20)
  expect_equal(nrow(apply_limit(df, 5L, 0L)), 5)
  expect_equal(nrow(apply_limit(df, 5L, 10L)), 5)
  expect_equal(apply_limit(df, 5L, 10L)$x, 11:15)
  expect_equal(nrow(apply_limit(df, NULL, 0L)), 20)
})
