#' Create a GraphQL connector
#'
#' Queries a GraphQL endpoint. Filter arguments are rendered into the
#' `{{filter_args}}` placeholder of the query template. Supports Hasura and
#' PostGraphile dialects for filter argument syntax.
#'
#' @param endpoint GraphQL endpoint URL.
#' @param query_template A GraphQL query string with a `{{filter_args}}`
#'   placeholder, e.g. `"query { patients{{filter_args}} { id name } }"`.
#' @param data_path JSON path to the array in the response data, e.g.
#'   `"data.patients"`.
#' @param schema_map Named list mapping property IDs to JSON field names.
#' @param auth Authentication object (conn_auth_*), or NULL.
#' @param graphql_dialect One of "hasura", "postgraphile", "generic".
#' @param cache_ttl Integer. Cache TTL in seconds.
#' @return A list of class `conn_graphql`.
#' @export
conn_graphql <- function(endpoint,
                          query_template,
                          data_path,
                          schema_map,
                          auth             = NULL,
                          graphql_dialect  = "hasura",
                          cache_ttl        = 0L) {
  validate_schema_map(schema_map)
  conn <- list(
    endpoint        = endpoint,
    query_template  = query_template,
    data_path       = data_path,
    schema_map      = schema_map,
    auth            = auth,
    graphql_dialect = graphql_dialect,
    pk_property     = names(schema_map)[1],
    connector_id    = paste0("graphql:", endpoint)
  )
  conn <- init_cache(conn, cache_ttl)
  class(conn) <- c("conn_graphql", "OntologyConnector")
  conn
}

#' @export
conn_supports_filter.conn_graphql <- function(connector, op) {
  op %in% c("eq", "neq", "gt", "gte", "lt", "lte", "in", "not_in",
             "contains", "starts_with")
}

#' @export
conn_fetch.conn_graphql <- function(connector, filters = list(),
                                     select = NULL, limit = NULL, offset = 0L) {
  filter_args <- build_graphql_filter_args(filters, connector)
  query <- sub("\\{\\{filter_args\\}\\}", filter_args, connector$query_template,
               fixed = TRUE)

  body <- list(query = query)
  req  <- httr2::request(connector$endpoint) |>
    httr2::req_body_json(body) |>
    httr2::req_method("POST") |>
    httr2::req_headers("Content-Type" = "application/json")
  req <- apply_auth(req, connector$auth)

  resp    <- httr2::req_perform(req)
  payload <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (!is.null(payload$errors)) {
    errs <- paste(sapply(payload$errors, function(e) e$message), collapse = "; ")
    cli::cli_abort("GraphQL error(s): {errs}")
  }

  records <- extract_json_path(payload, connector$data_path)
  if (is.null(records) || !is.list(records)) records <- list()

  df <- apply_schema_map(records, connector$schema_map)
  df <- apply_limit(df, limit, offset)
  df
}

#' @export
conn_count.conn_graphql <- function(connector, filters = list()) {
  nrow(conn_fetch(connector, filters = filters))
}

# GraphQL filter argument builders ----------------------------------------

build_graphql_filter_args <- function(filters, connector) {
  if (length(filters) == 0L) return("")
  dialect <- connector$graphql_dialect %||% "hasura"
  switch(dialect,
    hasura      = hasura_filter_args(filters, connector$schema_map),
    postgraphile = postgraphile_filter_args(filters, connector$schema_map),
    generic_filter_args(filters, connector$schema_map)
  )
}

hasura_filter_args <- function(filters, schema_map) {
  clauses <- lapply(filters, function(f) {
    field <- schema_map[[f$property]] %||% f$property
    op_str <- switch(f$op,
      eq          = paste0("{_eq: ",   gql_literal(f$value), "}"),
      neq         = paste0("{_neq: ",  gql_literal(f$value), "}"),
      gt          = paste0("{_gt: ",   gql_literal(f$value), "}"),
      gte         = paste0("{_gte: ",  gql_literal(f$value), "}"),
      lt          = paste0("{_lt: ",   gql_literal(f$value), "}"),
      lte         = paste0("{_lte: ",  gql_literal(f$value), "}"),
      `in`        = paste0("{_in: [",  paste(sapply(f$value, gql_literal), collapse = ", "), "]}"),
      not_in      = paste0("{_nin: [", paste(sapply(f$value, gql_literal), collapse = ", "), "]}"),
      contains    = paste0("{_ilike: ", gql_literal(paste0("%", f$value, "%")), "}"),
      starts_with = paste0("{_ilike: ", gql_literal(paste0(f$value, "%")), "}"),
      paste0("{_eq: ", gql_literal(f$value), "}")
    )
    paste0(field, ": ", op_str)
  })
  paste0("(where: {", paste(clauses, collapse = ", "), "})")
}

postgraphile_filter_args <- function(filters, schema_map) {
  clauses <- lapply(filters, function(f) {
    field <- schema_map[[f$property]] %||% f$property
    op_str <- switch(f$op,
      eq   = paste0("{equalTo: ", gql_literal(f$value), "}"),
      gt   = paste0("{greaterThan: ", gql_literal(f$value), "}"),
      gte  = paste0("{greaterThanOrEqualTo: ", gql_literal(f$value), "}"),
      lt   = paste0("{lessThan: ", gql_literal(f$value), "}"),
      lte  = paste0("{lessThanOrEqualTo: ", gql_literal(f$value), "}"),
      paste0("{equalTo: ", gql_literal(f$value), "}")
    )
    paste0(field, ": ", op_str)
  })
  paste0("(filter: {", paste(clauses, collapse = ", "), "})")
}

generic_filter_args <- function(filters, schema_map) {
  clauses <- lapply(filters, function(f) {
    field <- schema_map[[f$property]] %||% f$property
    paste0(field, ": ", gql_literal(f$value))
  })
  paste0("(", paste(clauses, collapse = ", "), ")")
}

gql_literal <- function(value) {
  if (is.numeric(value)) return(as.character(value))
  if (is.logical(value)) return(tolower(as.character(value)))
  paste0('"', gsub('"', '\\"', as.character(value)), '"')
}

`%||%` <- function(a, b) if (is.null(a)) b else a
