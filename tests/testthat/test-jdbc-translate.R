# translate_jdbc_sql() warns instead of silently sending untranslated SQL.

sql <- 'SELECT "EMP_ID" FROM "HR.EMPLOYEES" LIMIT 5'

test_that("duckdb dialect is passed through without a warning", {
  expect_no_warning(
    out <- ontologyConnectR:::translate_jdbc_sql(sql, "duckdb")
  )
  expect_identical(out, sql)
})

test_that("missing sqlglotR warns and sends the SQL as is", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "sqlglotR")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_warning(
    out <- ontologyConnectR:::translate_jdbc_sql(sql, "tsql"),
    "Install sqlglotR to translate SQL to \"tsql\"; sending DuckDB SQL as is",
    class = "ontologyConnectR_untranslated_sql"
  )
  expect_identical(out, sql)
})

test_that("the missing-translator warning is shown once per session", {
  rlang::local_options(rlib_warning_verbosity = "default")
  rlang::reset_warning_verbosity("ontologyConnectR_no_sqlglotR_oracle")
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "sqlglotR")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_warning(ontologyConnectR:::translate_jdbc_sql(sql, "oracle"))
  expect_no_warning(ontologyConnectR:::translate_jdbc_sql(sql, "oracle"))
})

test_that("missing Python sqlglot module warns and sends the SQL as is", {
  skip_if_not_installed("sqlglotR")
  rlang::local_options(rlib_warning_verbosity = "verbose")
  local_mocked_bindings(
    sqlglot_available = function() FALSE,
    .package = "sqlglotR"
  )
  expect_warning(
    out <- ontologyConnectR:::translate_jdbc_sql(sql, "postgres"),
    "sqlglot",
    class = "ontologyConnectR_untranslated_sql"
  )
  expect_identical(out, sql)
})

test_that("sqlglotR translates when available", {
  skip_if_not_installed("sqlglotR")
  skip_if_not(sqlglotR::sqlglot_available(), "sqlglot Python package not available")
  expect_no_warning(
    out <- ontologyConnectR:::translate_jdbc_sql(sql, "tsql")
  )
  expect_match(out, "TOP 5")
})
