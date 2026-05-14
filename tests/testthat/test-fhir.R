test_that("live_fhir returns LiveFhirConnection", {
  sm <- make_rest_schema_map("Patient", "Patient",
                              list(patient_id = "id", family_name = "name[0].family"),
                              pk = "patient_id")
  conn <- live_fhir(sm, "https://hapi.fhir.org/baseR4")
  expect_s4_class(conn, "LiveFhirConnection")
  expect_equal(conn@source_type, "fhir")
})

test_that("live_fhir validates schema_map", {
  bad <- list(P = list(name = "Patient"))
  expect_error(live_fhir(bad, "https://x.com"), class = "rlang_error")
})

test_that("fhir_filter_value: eq returns value as-is", {
  expect_equal(ontologyConnectR:::fhir_filter_value("=", "Smith"), "Smith")
})

test_that("fhir_filter_value: gt returns gt-prefixed value", {
  expect_equal(ontologyConnectR:::fhir_filter_value(">", "1990-01-01"), "gt1990-01-01")
})

test_that("fhir_filter_value: gte returns ge-prefixed value", {
  expect_equal(ontologyConnectR:::fhir_filter_value(">=", "1990-01-01"), "ge1990-01-01")
})

test_that("fhir_filter_value: IN joins with comma", {
  expect_equal(ontologyConnectR:::fhir_filter_value("IN", c("active", "inactive")),
               "active,inactive")
})

test_that("fhir_filter_value: unknown op returns NULL", {
  expect_null(ontologyConnectR:::fhir_filter_value("RAW_SQL", "x"))
})

test_that("extract_fhir_bundle extracts resources from Bundle", {
  bundle <- list(entry = list(
    list(resource = list(id = "p1", resourceType = "Patient")),
    list(resource = list(id = "p2", resourceType = "Patient"))
  ))
  res <- ontologyConnectR:::extract_fhir_bundle(bundle)
  expect_length(res, 2)
  expect_equal(res[[1]]$id, "p1")
})

test_that("fhir_next_link: returns NULL when no next relation", {
  bundle <- list(link = list(list(relation = "self", url = "https://x.com")))
  expect_null(ontologyConnectR:::fhir_next_link(bundle))
})

test_that("fhir_next_link: returns URL for next relation", {
  bundle <- list(link = list(
    list(relation = "self", url = "https://x.com/Patient"),
    list(relation = "next", url = "https://x.com/Patient?page=2")
  ))
  expect_equal(ontologyConnectR:::fhir_next_link(bundle),
               "https://x.com/Patient?page=2")
})

test_that("eval_fhirpath: simple field", {
  node <- list(id = "p1", gender = "female")
  expect_equal(ontologyConnectR:::eval_fhirpath(node, "gender"), "female")
})

test_that("eval_fhirpath: bracket index", {
  node <- list(name = list(list(family = "Smith"), list(family = "Jones")))
  expect_equal(ontologyConnectR:::eval_fhirpath(node, "name[0].family"), "Smith")
})

test_that("eval_fhirpath: .where(use='official')", {
  node <- list(name = list(
    list(use = "nickname", family = "Smitty"),
    list(use = "official", family = "Smith")
  ))
  expect_equal(
    ontologyConnectR:::eval_fhirpath(node, "name.where(use='official').family"),
    "Smith"
  )
})

test_that("live_execute: schema query returns empty df with FHIR columns", {
  sm   <- make_rest_schema_map("Patient", "Patient",
                                list(patient_id = "id", gender = "gender"))
  conn <- methods::new("LiveFhirConnection",
    source_type = "fhir", schema_map = sm, cache = NULL, config = list())
  df <- ontologyConnectR:::live_execute(conn, 'SELECT * FROM "Patient" WHERE (0 = 1)')
  expect_equal(nrow(df), 0L)
  expect_named(df, c("patient_id", "gender"))
})
