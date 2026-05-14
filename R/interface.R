#' Fetch rows from a live source connector
#'
#' @param connector An OntologyConnector object.
#' @param filters A list of filter specs, each a list with `property`, `op`, `value`.
#' @param select Character vector of property IDs to return, or NULL for all.
#' @param limit Integer or NULL.
#' @param offset Integer offset. Default 0L.
#' @return A data.frame with columns matching ontology property IDs.
#' @export
conn_fetch <- function(connector, filters = list(),
                       select = NULL, limit = NULL, offset = 0L) {
  UseMethod("conn_fetch")
}

#' Get a single row by primary key
#'
#' @param connector An OntologyConnector object.
#' @param pk_value The primary key value.
#' @return A single-row data.frame, or NULL if not found.
#' @export
conn_get_one <- function(connector, pk_value) {
  UseMethod("conn_get_one")
}

#' Count rows matching filters
#'
#' @param connector An OntologyConnector object.
#' @param filters A list of filter specs.
#' @return A single integer.
#' @export
conn_count <- function(connector, filters = list()) {
  UseMethod("conn_count")
}

#' Check whether a connector supports a given filter op natively
#'
#' @param connector An OntologyConnector object.
#' @param op A filter operator string (e.g. "eq", "gt", "contains").
#' @return Logical scalar.
#' @export
conn_supports_filter <- function(connector, op) {
  UseMethod("conn_supports_filter")
}

#' @export
conn_supports_filter.default <- function(connector, op) {
  FALSE
}

#' Check whether a connector supports link traversal natively
#'
#' @param connector An OntologyConnector object.
#' @param link_type_id Character scalar.
#' @return Logical scalar.
#' @export
conn_supports_traverse <- function(connector, link_type_id) {
  UseMethod("conn_supports_traverse")
}

#' @export
conn_supports_traverse.default <- function(connector, link_type_id) {
  FALSE
}

#' Default conn_get_one: calls conn_fetch with a pk equality filter
#' @export
conn_get_one.default <- function(connector, pk_value) {
  pk_prop <- connector$pk_property
  if (is.null(pk_prop)) {
    cli::cli_abort("Connector has no {.field pk_property} set.")
  }
  result <- conn_fetch(
    connector,
    filters = list(list(property = pk_prop, op = "eq", value = pk_value)),
    limit = 1L
  )
  if (nrow(result) == 0L) NULL else result
}

#' Default conn_count: calls conn_fetch and counts rows
#' @export
conn_count.default <- function(connector, filters = list()) {
  nrow(conn_fetch(connector, filters = filters))
}

#' Validate that a filter spec list is well-formed
#' @param filters list of filter specs
#' @return invisible(filters), or aborts on error
validate_filters <- function(filters) {
  if (!is.list(filters)) {
    cli::cli_abort("{.arg filters} must be a list.")
  }
  for (i in seq_along(filters)) {
    f <- filters[[i]]
    if (!all(c("property", "op", "value") %in% names(f))) {
      cli::cli_abort(
        "Filter {i} must have {.field property}, {.field op}, and {.field value}."
      )
    }
    valid_ops <- c("eq", "neq", "gt", "gte", "lt", "lte", "contains",
                   "starts_with", "ends_with", "in", "not_in", "is_null", "not_null")
    if (!f$op %in% valid_ops) {
      cli::cli_abort("Unknown filter op {.val {f$op}}. Valid: {.val {valid_ops}}.")
    }
  }
  invisible(filters)
}
