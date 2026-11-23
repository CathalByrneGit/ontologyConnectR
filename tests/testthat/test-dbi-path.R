# DBI path of connect_object_set(), run against DuckDB and RSQLite.

dbi_backends <- list(
  duckdb  = function() DBI::dbConnect(duckdb::duckdb(), ":memory:"),
  RSQLite = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:")
)

for (backend in names(dbi_backends)) {
  local({
    backend <- backend
    connect <- dbi_backends[[backend]]

    local_test_con <- function(env = parent.frame()) {
      skip_if_not_installed("dplyr")
      skip_if_not_installed("dbplyr")
      skip_if_not_installed(backend)
      con <- connect()
      withr::defer(DBI::dbDisconnect(con), envir = env)
      DBI::dbWriteTable(con, "patient",
                        data.frame(id = 1:3, name = c("Ann", "Bob", "Cy")))
      con
    }

    test_that(paste0(backend, ": table-backed type reads source$table"), {
      con <- local_test_con()
      ctx <- connect_context(make_test_bundle("Patient"), connection = con)
      res <- dplyr::collect(connect_object_set(ctx, "Patient"))
      expect_equal(nrow(res), 3L)
      expect_setequal(names(res), c("id", "name"))
    })

    test_that(paste0(backend, ": dplyr verbs are pushed down"), {
      con <- local_test_con()
      ctx <- connect_context(make_test_bundle("Patient"), connection = con)
      res <- connect_object_set(ctx, "Patient") |>
        dplyr::filter(id > 1L) |>
        dplyr::collect()
      expect_equal(sort(res$name), c("Bob", "Cy"))
    })

    test_that(paste0(backend, ": query-backed type uses object_source_sql()"), {
      con <- local_test_con()
      ot  <- make_test_object_type(
        "AdultPatient",
        source_sql = "SELECT id, name FROM patient WHERE id >= 2"
      )
      ctx <- connect_context(make_test_bundle(objects = list(ot)), connection = con)
      res <- dplyr::collect(connect_object_set(ctx, "AdultPatient"))
      expect_equal(sort(res$id), 2:3)
    })

    test_that(paste0(backend, ": schema-qualified source table"), {
      con <- local_test_con()
      ot  <- make_test_object_type("Patient", source_schema = "main")
      ctx <- connect_context(make_test_bundle(objects = list(ot)), connection = con)
      res <- dplyr::collect(connect_object_set(ctx, "Patient"))
      expect_equal(nrow(res), 3L)
    })

    test_that(paste0(backend, ": mixed live and DBI types in one context"), {
      skip_if_not_installed("duckdb")
      con  <- local_test_con()
      live <- live_icij()
      ctx  <- connect_context(make_test_bundle(c("Patient", "Entity")),
                              connection = con,
                              live_connections = list(Entity = live),
                              strict = TRUE)
      expect_equal(nrow(dplyr::collect(connect_object_set(ctx, "Patient"))), 3L)
      expect_gt(nrow(dplyr::collect(connect_object_set(ctx, "Entity"))), 0L)
    })
  })
}
