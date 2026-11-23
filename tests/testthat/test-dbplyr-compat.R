test_that("live_icij + dplyr::collect returns data", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  bundle <- make_test_bundle("Entity")
  ctx <- connect_context(bundle, live_connections = list(Entity = conn))
  result <- dplyr::collect(connect_object_set(ctx, "Entity"))
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("live_icij: dplyr::filter translates to SQL pushed to DuckDB", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  bundle <- make_test_bundle("Entity")
  ctx <- connect_context(bundle, live_connections = list(Entity = conn))

  result <- connect_object_set(ctx, "Entity") |>
    dplyr::filter(jurisdiction == "BVI") |>
    dplyr::collect()

  expect_true(all(result$jurisdiction == "BVI"))
})

test_that("live_icij: dplyr::show_query returns SQL string", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  bundle <- make_test_bundle("Entity")
  ctx <- connect_context(bundle, live_connections = list(Entity = conn))

  os  <- connect_object_set(ctx, "Entity") |>
    dplyr::filter(source_id == "Panama Papers")
  out <- capture.output(dplyr::show_query(os))
  expect_true(any(grepl("WHERE|where", out)))
})

test_that("live_icij Officer count matches CSV row count", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("duckdb")
  fixture_dir <- icij_fixture_dir()
  skip_if(!nzchar(fixture_dir), "ICIJ fixture not found")

  conn <- live_icij(csv_dir = fixture_dir)
  bundle <- make_test_bundle("Officer")
  ctx <- connect_context(bundle, live_connections = list(Officer = conn))

  csv_rows <- nrow(utils::read.csv(file.path(fixture_dir, "officers.csv")))
  live_rows <- dplyr::collect(connect_object_set(ctx, "Officer")) |> nrow()
  expect_equal(live_rows, csv_rows)
})
