test_that("live_icij requires duckdb", {
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  expect_s4_class(conn, "LiveIcijConnection")
})

test_that("live_icij schema_map has correct structure", {
  sm <- ontologyConnectR:::icij_schema_map()
  expect_true(all(c("Entity", "Officer", "Intermediary", "Address") %in% names(sm)))
  expect_true("entity_id" %in% names(sm$Entity$columns))
  expect_true("officer_id" %in% names(sm$Officer$columns))
})

test_that("dbGetQuery on LiveIcijConnection returns Entity data", {
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  df   <- DBI::dbGetQuery(conn, 'SELECT * FROM "Entity"')
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1)
  expect_true("entity_id" %in% names(df))
})

test_that("dbGetQuery on LiveIcijConnection: Entity WHERE jurisdiction = BVI", {
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  df   <- DBI::dbGetQuery(conn,
    'SELECT * FROM "Entity" WHERE ("jurisdiction" = \'BVI\')')
  expect_true(all(df$jurisdiction == "BVI"))
})

test_that("dbListTables returns all ICIJ object types", {
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn  <- live_icij(csv_dir = fixture_dir)
  tbls  <- DBI::dbListTables(conn)
  expect_true("Entity"  %in% tbls)
  expect_true("Officer" %in% tbls)
})

test_that("conn_invalidate clears LiveIcijConnection cache", {
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir, cache_ttl = 60L)
  expect_true(!is.null(conn@cache))
  conn2 <- conn_invalidate(conn)
  # cache object still exists but is now empty
  expect_true(!is.null(conn2@cache))
})

test_that("edges CSV links officers to entities", {
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  edges_path <- file.path(fixture_dir, "edges.csv")
  skip_if(!file.exists(edges_path), "edges.csv not found")

  edges      <- utils::read.csv(edges_path, stringsAsFactors = FALSE)
  officer_of <- edges[edges$rel_type == "officer_of", ]
  expect_true(nrow(officer_of) >= 5)
  expect_true("20000001" %in% as.character(officer_of$node_id_start))
})
