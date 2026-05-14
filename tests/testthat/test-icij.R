test_that("conn_icij loads Entity fixture data", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir) && !dir.exists(fixture_dir),
          "ICIJ fixture not found")

  conn <- conn_icij("Entity", csv_dir = fixture_dir)
  expect_s3_class(conn, "conn_icij")
  expect_s3_class(conn, "OntologyConnector")
  expect_true(nrow(conn$data) >= 1)
})

test_that("conn_icij rejects invalid entity_type", {
  expect_error(conn_icij("Bogus"), class = "rlang_error")
})

test_that("conn_icij Officer fixture has expected columns", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- conn_icij("Officer", csv_dir = fixture_dir)
  expect_true("node_id" %in% names(conn$data))
  expect_true("name"    %in% names(conn$data))
})

test_that("conn_fetch.conn_icij filters by name", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- conn_icij("Entity", csv_dir = fixture_dir)
  all_entities <- conn_fetch(conn)
  expect_true(nrow(all_entities) > 0)

  # Filter to a specific source
  filtered <- conn_fetch(conn,
    filters = list(list(property = "source_id", op = "eq",
                        value = "Panama Papers")))
  expect_true(all(filtered$source_id == "Panama Papers"))
})

test_that("conn_count.conn_icij returns total row count", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn  <- conn_icij("Entity", csv_dir = fixture_dir)
  total <- conn_count(conn)
  expect_type(total, "integer")
  expect_true(total >= 1)
  expect_equal(total, nrow(conn$data))
})

test_that("conn_get_one.conn_icij returns single row or NULL", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- conn_icij("Entity", csv_dir = fixture_dir)
  pk   <- conn$data$node_id[1]

  row  <- conn_get_one(conn, pk)
  expect_s3_class(row, "data.frame")
  expect_equal(nrow(row), 1)
  expect_equal(row$node_id, pk)

  missing_row <- conn_get_one(conn, "DOES_NOT_EXIST")
  expect_null(missing_row)
})

test_that("ICIJ Entity + Officer counts match fixture CSV sizes", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  entity_csv  <- utils::read.csv(file.path(fixture_dir, "entities.csv"),
                                  stringsAsFactors = FALSE)
  officer_csv <- utils::read.csv(file.path(fixture_dir, "officers.csv"),
                                  stringsAsFactors = FALSE)

  entity_conn  <- conn_icij("Entity",  csv_dir = fixture_dir)
  officer_conn <- conn_icij("Officer", csv_dir = fixture_dir)

  expect_equal(conn_count(entity_conn),  nrow(entity_csv))
  expect_equal(conn_count(officer_conn), nrow(officer_csv))
})

test_that("icij_load_edges loads edges fixture", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  edges <- icij_load_edges(csv_dir = fixture_dir)
  expect_s3_class(edges, "data.frame")
  expect_true("node_id_start" %in% names(edges))
  expect_true("node_id_end"   %in% names(edges))
  expect_true("rel_type"      %in% names(edges))
  expect_true(nrow(edges) >= 5)
})

test_that("ICIJ officer_of edges link officers to entities", {
  fixture_dir <- system.file("fixtures", "icij_sample",
                              package = "ontologyConnectR")
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  edges <- icij_load_edges(csv_dir = fixture_dir)
  officer_of <- edges[edges$rel_type == "officer_of", ]
  expect_true(nrow(officer_of) >= 5)

  # Officers 20000001-20000005 should link to entities 10000001-10000005
  expect_true("20000001" %in% as.character(officer_of$node_id_start))
})
