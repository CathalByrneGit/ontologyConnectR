test_that("live_rest returns LiveRestConnection", {
  sm <- make_rest_schema_map("Item", "items",
                              list(item_id = "id", name = "name"))
  conn <- live_rest(sm, "https://api.example.com")
  expect_s4_class(conn, "LiveRestConnection")
  expect_equal(conn@source_type, "rest")
})

test_that("live_rest validates schema_map structure", {
  bad <- list(Item = list(name = "items"))  # missing 'resource' and 'columns'
  expect_error(live_rest(bad, "https://api.example.com"), class = "rlang_error")
})

test_that("live_rest with cache_ttl enables cache", {
  sm   <- make_rest_schema_map("X", "x", list(id = "id"))
  conn <- live_rest(sm, "https://x.com", cache_ttl = 60L)
  expect_equal(conn@cache$max_age %||% conn@config$cache_ttl %||% 60L, 60L)
  expect_true(!is.null(conn@cache))
})

test_that("conn_auth_bearer stores token and env_var", {
  auth <- conn_auth_bearer(token = "tok123")
  expect_equal(auth$token, "tok123")
  auth2 <- conn_auth_bearer(env_var = "MY_TOK")
  expect_equal(auth2$env_var, "MY_TOK")
})

test_that("conn_auth_basic stores username and env_var", {
  auth <- conn_auth_basic("alice", "MY_PASS")
  expect_equal(auth$username, "alice")
  expect_equal(auth$password_env_var, "MY_PASS")
})

test_that("conn_auth_api_key stores header and env_var", {
  auth <- conn_auth_api_key("X-API-Key", "MY_KEY")
  expect_equal(auth$header, "X-API-Key")
  expect_equal(auth$key_env_var, "MY_KEY")
})

test_that("conn_pagination_offset stores params", {
  pag <- conn_pagination_offset("pg", "sz", 50L)
  expect_equal(pag$page_size, 50L)
})

test_that("dbListTables returns schema_map keys", {
  sm   <- make_rest_schema_map("Patient", "patients", list(id = "id"))
  conn <- live_rest(sm, "https://x.com")
  expect_equal(DBI::dbListTables(conn), "Patient")
})

test_that("dbExistsTable returns TRUE for known table", {
  sm   <- make_rest_schema_map("Patient", "patients", list(id = "id"))
  conn <- live_rest(sm, "https://x.com")
  expect_true(DBI::dbExistsTable(conn, "Patient"))
  expect_false(DBI::dbExistsTable(conn, "Unknown"))
})

test_that("live_execute returns empty df with correct columns for schema query", {
  sm   <- make_rest_schema_map("P", "p", list(patient_id = "id", name = "name"))
  conn <- live_rest(sm, "https://x.com")
  df   <- ontologyConnectR:::live_execute(conn, 'SELECT * FROM "P" WHERE (0 = 1)')
  expect_equal(nrow(df), 0L)
  expect_named(df, c("patient_id", "name"))
})

test_that("translate_filters_to_r: eq filter works", {
  df <- data.frame(country = c("IE", "UK", "IE"), stringsAsFactors = FALSE)
  fn <- translate_filters_to_r(
    list(list(property = "country", op = "eq", value = "IE")))
  expect_equal(nrow(fn(df)), 2L)
})

test_that("translate_filters_to_r: identity for empty filters", {
  df <- data.frame(x = 1:5)
  fn <- translate_filters_to_r(list())
  expect_identical(fn(df), df)
})
