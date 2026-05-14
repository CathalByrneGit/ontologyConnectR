test_that("parse_live_query extracts table name", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT "id", "name" FROM "Airport" WHERE ("country" = \'Ireland\')'
  )
  expect_equal(p$table, "Airport")
})

test_that("parse_live_query extracts select columns", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT "airport_id", "name" FROM "Airport"'
  )
  expect_equal(p$select, c("airport_id", "name"))
})

test_that("parse_live_query NULL select for SELECT *", {
  p <- ontologyConnectR:::parse_live_query('SELECT * FROM "Airport"')
  expect_null(p$select)
})

test_that("parse_live_query detects schema query with 0=1", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT * FROM "Airport" WHERE (0 = 1)'
  )
  expect_true(p$is_schema_query)
  expect_equal(p$filters, list())
})

test_that("parse_live_query extracts equality filter", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT "id" FROM "T" WHERE ("country" = \'Ireland\')'
  )
  expect_length(p$filters, 1)
  expect_equal(p$filters[[1]]$column, "country")
  expect_equal(p$filters[[1]]$op,    "=")
  expect_equal(p$filters[[1]]$value, "Ireland")
})

test_that("parse_live_query extracts numeric comparison filter", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT "id" FROM "T" WHERE ("capacity" >= 1000)'
  )
  f <- p$filters[[1]]
  expect_equal(f$op,    ">=")
  expect_equal(f$value, 1000)
})

test_that("parse_live_query extracts LIMIT and OFFSET", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT "id" FROM "T" LIMIT 50 OFFSET 10'
  )
  expect_equal(p$limit,  50L)
  expect_equal(p$offset, 10L)
})

test_that("parse_live_query handles NULL limit when absent", {
  p <- ontologyConnectR:::parse_live_query('SELECT "id" FROM "T"')
  expect_null(p$limit)
  expect_equal(p$offset, 0L)
})

test_that("parse_live_query extracts multiple AND filters", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT "id" FROM "T" WHERE ("country" = \'IE\' AND "age" > 18)'
  )
  expect_length(p$filters, 2)
})

test_that("parse_live_query extracts IN filter", {
  p <- ontologyConnectR:::parse_live_query(
    "SELECT \"id\" FROM \"T\" WHERE (\"status\" IN ('active', 'pending'))"
  )
  f <- p$filters[[1]]
  expect_equal(f$op, "IN")
  expect_equal(f$value, c("active", "pending"))
})

test_that("parse_live_query extracts IS NULL filter", {
  p <- ontologyConnectR:::parse_live_query(
    'SELECT "id" FROM "T" WHERE ("deleted_at" IS NULL)'
  )
  expect_equal(p$filters[[1]]$op, "IS NULL")
})

test_that("map_to_source applies column mapping", {
  sm <- make_rest_schema_map("Airport", "airports",
                              list(airport_id = "id", name = "name",
                                   country = "country_code"))
  # Create a minimal mock connection
  conn <- methods::new("LiveRestConnection",
    source_type = "rest", schema_map = sm, cache = NULL, config = list())

  parsed <- ontologyConnectR:::parse_live_query(
    'SELECT "airport_id", "country" FROM "Airport" WHERE ("country" = \'IE\')'
  )
  sq <- ontologyConnectR:::map_to_source(conn, parsed)

  expect_equal(sq$resource,       "airports")
  expect_equal(sq$select,         c("id", "country_code"))
  expect_equal(sq$filters[[1]]$source_column, "country_code")
})

test_that("map_from_source renames source columns to property IDs", {
  sm <- make_rest_schema_map("Airport", "airports",
                              list(airport_id = "id", name = "name"))
  conn <- methods::new("LiveRestConnection",
    source_type = "rest", schema_map = sm, cache = NULL, config = list())

  df     <- data.frame(id = "JFK", name = "JFK Airport", stringsAsFactors = FALSE)
  result <- ontologyConnectR:::map_from_source(conn, "Airport", df)

  expect_named(result, c("airport_id", "name"))
})

test_that("reconstruct_sql rebuilds SELECT with WHERE and LIMIT", {
  sq <- list(
    resource = "airports",
    select   = c("id", "country_code"),
    filters  = list(list(source_column = "country_code", op = "=", value = "IE")),
    limit    = 10L,
    offset   = 0L
  )
  sql <- ontologyConnectR:::reconstruct_sql(sq)
  expect_match(sql, "SELECT")
  expect_match(sql, '"airports"')
  expect_match(sql, "WHERE")
  expect_match(sql, "LIMIT 10")
})

test_that("live_execute returns empty df with correct cols for schema query", {
  sm <- make_rest_schema_map("Airport", "airports",
                              list(airport_id = "id", country = "country_code"))
  conn <- methods::new("LiveRestConnection",
    source_type = "rest", schema_map = sm, cache = NULL, config = list())

  result <- ontologyConnectR:::live_execute(conn,
    'SELECT * FROM "Airport" WHERE (0 = 1)')

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("airport_id", "country"))
})
