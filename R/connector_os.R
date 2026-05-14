#' Create a ConnectorObjectSet for a live-source object type
#'
#' Returns an S3 object that accumulates filter specs, select lists, and limits
#' in memory. Actual data fetching is deferred until `os_collect()` is called.
#'
#' @param ctx An `OntologyContext`.
#' @param object_type_id Character scalar.
#' @return A `ConnectorObjectSet` object.
#' @export
connector_object_set <- function(ctx, object_type_id) {
  connector <- ctx$connectors[[object_type_id]]
  if (is.null(connector)) {
    cli::cli_abort("No connector registered for type {.val {object_type_id}}.")
  }
  structure(
    list(
      ctx             = ctx,
      object_type_id  = object_type_id,
      connector       = connector,
      pending_filters = list(),
      pending_select  = NULL,
      pending_limit   = NULL,
      pending_offset  = 0L
    ),
    class = c("ConnectorObjectSet", "ObjectSet")
  )
}

#' @export
print.ConnectorObjectSet <- function(x, ...) {
  cli::cli_h2("ConnectorObjectSet [{x$object_type_id}]")
  cli::cli_bullets(c(
    "*" = "Connector: {.cls {class(x$connector)[1]}}",
    "*" = "Pending filters: {length(x$pending_filters)}",
    "*" = "Select: {if (is.null(x$pending_select)) 'all' else paste(x$pending_select, collapse=', ')}",
    "*" = "Limit: {x$pending_limit %||% 'none'}"
  ))
  invisible(x)
}

#' Add a filter to a ConnectorObjectSet
#'
#' Accumulates filter specs without executing any query.
#'
#' @param os A `ConnectorObjectSet`.
#' @param property Character. Property ID to filter on.
#' @param op Character. Filter operator (eq, neq, gt, gte, lt, lte, contains,
#'   starts_with, ends_with, in, not_in, is_null, not_null).
#' @param value The value to compare against (ignored for is_null / not_null).
#' @return A new `ConnectorObjectSet` with the filter appended.
#' @export
os_filter <- function(os, property, op, value = NULL) {
  UseMethod("os_filter")
}

#' @export
os_filter.ConnectorObjectSet <- function(os, property, op, value = NULL) {
  filter_spec <- list(property = property, op = op, value = value)
  os$pending_filters <- c(os$pending_filters, list(filter_spec))
  os
}

#' @export
os_filter.DbiObjectSet <- function(os, property, op, value = NULL) {
  filter_spec <- list(property = property, op = op, value = value)
  os$pending_filters <- c(os$pending_filters, list(filter_spec))
  os
}

#' Collect results from an object set
#'
#' For a `ConnectorObjectSet`, calls `conn_fetch()` with the accumulated
#' filters. For a DBI-backed set, executes the SQL query.
#'
#' @param os An object set.
#' @return A data.frame.
#' @export
os_collect <- function(os) {
  UseMethod("os_collect")
}

#' @export
os_collect.ConnectorObjectSet <- function(os) {
  cache_fetch(
    connector = os$connector,
    filters   = os$pending_filters,
    select    = os$pending_select,
    limit     = os$pending_limit,
    offset    = os$pending_offset
  )
}

#' @export
os_collect.DbiObjectSet <- function(os) {
  cli::cli_abort(
    "DBI-backed os_collect() requires objectSetsR to be loaded."
  )
}

#' Count matching rows in an object set
#'
#' @param os An object set.
#' @return Integer scalar.
#' @export
os_count <- function(os) {
  UseMethod("os_count")
}

#' @export
os_count.ConnectorObjectSet <- function(os) {
  conn_count(os$connector, filters = os$pending_filters)
}

#' @export
os_count.DbiObjectSet <- function(os) {
  cli::cli_abort(
    "DBI-backed os_count() requires objectSetsR to be loaded."
  )
}

#' Show the pending query for an object set
#'
#' For a `ConnectorObjectSet`, returns a human-readable description of the
#' pending filters (there is no SQL to show).
#'
#' @param os An object set.
#' @return Character scalar (invisibly).
#' @export
os_show_query <- function(os) {
  UseMethod("os_show_query")
}

#' @export
os_show_query.ConnectorObjectSet <- function(os) {
  lines <- c(
    paste0("ConnectorObjectSet [", os$object_type_id, "]"),
    paste0("  Connector: ", class(os$connector)[1]),
    paste0("  Pending filters (", length(os$pending_filters), "):")
  )
  for (f in os$pending_filters) {
    val_str <- if (is.null(f$value)) "NULL" else paste(f$value, collapse = ", ")
    lines <- c(lines, paste0("    ", f$property, " ", f$op, " ", val_str))
  }
  if (!is.null(os$pending_select)) {
    lines <- c(lines, paste0("  Select: ", paste(os$pending_select, collapse = ", ")))
  }
  if (!is.null(os$pending_limit)) {
    lines <- c(lines, paste0("  Limit: ", os$pending_limit))
  }
  query_str <- paste(lines, collapse = "\n")
  cat(query_str, "\n")
  invisible(query_str)
}

#' @export
os_show_query.DbiObjectSet <- function(os) {
  cli::cli_abort(
    "DBI-backed os_show_query() requires objectSetsR to be loaded."
  )
}

#' Restrict columns returned by an object set
#'
#' @param os A `ConnectorObjectSet`.
#' @param ... Property IDs to select (unquoted or character).
#' @return A new `ConnectorObjectSet`.
#' @export
os_select <- function(os, ...) {
  cols <- as.character(rlang::ensyms(...))
  os$pending_select <- cols
  os
}

#' Set a row limit on an object set
#'
#' @param os A `ConnectorObjectSet`.
#' @param n Integer.
#' @return A new `ConnectorObjectSet`.
#' @export
os_limit <- function(os, n) {
  os$pending_limit <- as.integer(n)
  os
}

#' Traverse a link from a ConnectorObjectSet
#'
#' If the connector supports native traversal, delegates to the connector.
#' Otherwise, collects results then resolves links in R.
#'
#' @param os A `ConnectorObjectSet`.
#' @param link_type_id Character. The link type ID defined in the bundle.
#' @return A `ConnectorObjectSet` for the target object type.
#' @export
os_traverse <- function(os, link_type_id) {
  UseMethod("os_traverse")
}

#' @export
os_traverse.ConnectorObjectSet <- function(os, link_type_id) {
  ctx  <- os$ctx
  conn <- os$connector

  link_type <- ctx$bundle$link_types[[link_type_id]]
  if (is.null(link_type)) {
    cli::cli_abort("Unknown link type: {.val {link_type_id}}.")
  }
  target_type_id <- link_type$target_type_id

  if (conn_supports_traverse(conn, link_type_id)) {
    # Connector handles the traversal natively
    result <- conn_traverse(conn, os, link_type_id)
    return(result)
  }

  # R-side traversal: collect source rows, resolve links via edges
  source_df <- os_collect(os)
  pk_prop   <- conn$pk_property %||% link_type$source_pk

  if (is.null(pk_prop) || !pk_prop %in% names(source_df)) {
    cli::cli_abort("Cannot traverse: primary key column not found in result.")
  }

  pk_values <- unique(source_df[[pk_prop]])

  # Fetch matching edges
  edges_conn <- ctx$connectors[["__edges__"]]
  if (is.null(edges_conn)) {
    cli::cli_abort(
      "No edge connector registered. Add an '__edges__' connector to the context."
    )
  }
  edges_df <- conn_fetch(
    edges_conn,
    filters = list(
      list(property = "link_type_id", op = "eq",  value = link_type_id),
      list(property = "source_id",    op = "in",  value = pk_values)
    )
  )

  target_ids <- unique(edges_df$target_id)

  target_os <- object_set(ctx, target_type_id)
  target_os <- os_filter(target_os, link_type$target_pk %||% "id", "in", target_ids)
  target_os
}
