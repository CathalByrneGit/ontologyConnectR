test_that("conn_fhir builds correct object", {
  conn <- conn_fhir(
    base_url      = "https://hapi.fhir.org/baseR4",
    resource_type = "Patient",
    schema_map    = list(patient_id = "id", family_name = "name.family")
  )
  expect_s3_class(conn, "conn_fhir")
  expect_s3_class(conn, "OntologyConnector")
  expect_equal(conn$resource_type, "Patient")
})

test_that("conn_supports_filter.conn_fhir: supported ops", {
  conn <- conn_fhir("https://x.com", "Patient",
                     schema_map = list(id = "id"))
  expect_true(conn_supports_filter(conn, "eq"))
  expect_true(conn_supports_filter(conn, "contains"))
  expect_true(conn_supports_filter(conn, "gt"))
  expect_true(conn_supports_filter(conn, "gte"))
  expect_true(conn_supports_filter(conn, "lt"))
  expect_true(conn_supports_filter(conn, "lte"))
  expect_true(conn_supports_filter(conn, "in"))
  expect_false(conn_supports_filter(conn, "starts_with"))
  expect_false(conn_supports_filter(conn, "is_null"))
})

test_that("fhir_filters_to_params maps ops correctly", {
  # Access internal function via :::
  params <- ontologyConnectR:::fhir_filters_to_params(
    list(
      list(property = "family_name", op = "eq",  value = "Smith"),
      list(property = "birth_date",  op = "gt",  value = "1980-01-01"),
      list(property = "gender",      op = "in",  value = c("male", "female"))
    ),
    schema_map = list(family_name = "name.family", birth_date = "birthDate",
                      gender = "gender")
  )
  expect_equal(params[["family"]], "Smith")
  expect_equal(params[["birthdate"]], "gt1980-01-01")
  expect_equal(params[["gender"]], "male,female")
})

test_that("fhir_filters_to_params: contains uses :contains modifier", {
  params <- ontologyConnectR:::fhir_filters_to_params(
    list(list(property = "family_name", op = "contains", value = "smi")),
    schema_map = list(family_name = "name.family")
  )
  expect_equal(params[["family:contains"]], "smi")
})

test_that("eval_fhirpath handles simple field", {
  node <- list(id = "patient-1", gender = "female")
  result <- ontologyConnectR:::eval_fhirpath(node, "gender")
  expect_equal(result, "female")
})

test_that("eval_fhirpath handles dot notation", {
  node <- list(name = list(list(use = "official", family = "Smith")))
  result <- ontologyConnectR:::eval_fhirpath(node, "name[0].family")
  expect_equal(result, "Smith")
})

test_that("eval_fhirpath handles .where(use='official')", {
  node <- list(name = list(
    list(use = "nickname", family = "Smitty"),
    list(use = "official", family = "Smith")
  ))
  result <- ontologyConnectR:::eval_fhirpath(
    node, "name.where(use='official').family"
  )
  expect_equal(result, "Smith")
})

test_that("extract_fhir_resources extracts from Bundle entry", {
  bundle <- list(
    resourceType = "Bundle",
    entry = list(
      list(resource = list(id = "p1", resourceType = "Patient")),
      list(resource = list(id = "p2", resourceType = "Patient"))
    )
  )
  resources <- ontologyConnectR:::extract_fhir_resources(bundle)
  expect_length(resources, 2)
  expect_equal(resources[[1]]$id, "p1")
})

test_that("get_fhir_next_url returns NULL when no next link", {
  bundle <- list(link = list(
    list(relation = "self", url = "https://x.com/Patient")
  ))
  expect_null(ontologyConnectR:::get_fhir_next_url(bundle))
})

test_that("get_fhir_next_url returns next URL when present", {
  bundle <- list(link = list(
    list(relation = "self", url = "https://x.com/Patient"),
    list(relation = "next", url = "https://x.com/Patient?_page=2")
  ))
  result <- ontologyConnectR:::get_fhir_next_url(bundle)
  expect_equal(result, "https://x.com/Patient?_page=2")
})
