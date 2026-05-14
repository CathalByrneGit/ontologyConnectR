#' Create an ontology context with optional live-source connections
#'
#' Associates a bundle with DBI and/or live source connections. Object types
#' backed by a live connection are served via that connection's dbplyr backend;
#' others fall back to the DBI \code{connection}.
#'
#' @param bundle An ontology bundle — a list with \code{$object_types} (named
#'   list) and \code{$link_types} (named list).
#' @param connection A DBI connection, or NULL when all types have live
#'   connections.
#' @param live_connections Named list of \code{LiveConnection} objects. Names
#'   must match object type IDs in the bundle.
#' @param strict Logical. If TRUE, abort on unknown object type IDs in
#'   \code{live_connections}.
#' @param check_interfaces Logical. Reserved; currently unused.
#' @return An \code{OntologyContext} list.
#' @export
ontology_context <- function(bundle,
                              connection       = NULL,
                              live_connections = list(),
                              strict           = FALSE,
                              check_interfaces = TRUE) {
  if (!is.list(live_connections) ||
      (length(live_connections) > 0 && is.null(names(live_connections)))) {
    cli::cli_abort("{.arg live_connections} must be a named list.")
  }

  ot_ids <- names(bundle$object_types)

  if (strict && length(live_connections) > 0) {
    unknown <- setdiff(names(live_connections), ot_ids)
    if (length(unknown)) {
      cli::cli_abort(
        "Unknown object type IDs in {.arg live_connections}: {.val {unknown}}"
      )
    }
  }

  dbi_types <- setdiff(ot_ids, names(live_connections))
  if (length(dbi_types) > 0 && is.null(connection)) {
    cli::cli_warn(c(
      "Some object types have no live connection and no DBI connection.",
      i = "Types without live connection: {.val {dbi_types}}"
    ))
  }

  structure(
    list(
      bundle           = bundle,
      connection       = connection,
      live_connections = live_connections
    ),
    class = "OntologyContext"
  )
}

#' @export
print.OntologyContext <- function(x, ...) {
  ot_ids   <- names(x$bundle$object_types)
  live_ids <- names(x$live_connections)
  dbi_ids  <- setdiff(ot_ids, live_ids)
  cli::cli_h1("OntologyContext")
  cli::cli_bullets(c(
    "*" = "Object types: {length(ot_ids)}",
    "*" = "Live connections: {length(live_ids)} ({.val {live_ids}})",
    "*" = "DBI-backed: {length(dbi_ids)} ({.val {dbi_ids}})"
  ))
  invisible(x)
}

#' Get a tbl for an object type
#'
#' Returns \code{dplyr::tbl(live_con, object_type_id)} for types backed by a
#' live connection, or \code{dplyr::tbl(dbi_con, table_name)} otherwise.
#' Because both paths return a dbplyr lazy tbl, all downstream \code{os_*}
#' operations work identically.
#'
#' @param ctx An \code{OntologyContext}.
#' @param object_type_id Character scalar.
#' @return A lazy \code{tbl} object (class \code{tbl_dbi} / \code{tbl_lazy}).
#' @export
object_set <- function(ctx, object_type_id) {
  if (!inherits(ctx, "OntologyContext")) {
    cli::cli_abort("{.arg ctx} must be an {.cls OntologyContext}.")
  }

  if (object_type_id %in% names(ctx$live_connections)) {
    live_con <- ctx$live_connections[[object_type_id]]
    if (requireNamespace("dplyr", quietly = TRUE)) {
      return(dplyr::tbl(live_con, object_type_id))
    }
    # Fallback without dplyr: return a thin wrapper
    return(live_object_set(live_con, object_type_id))
  }

  # DBI path
  if (is.null(ctx$connection)) {
    cli::cli_abort(
      "No live connection and no DBI connection for type {.val {object_type_id}}."
    )
  }
  ot <- ctx$bundle$object_types[[object_type_id]]
  if (is.null(ot)) {
    cli::cli_abort("Unknown object type: {.val {object_type_id}}.")
  }
  if (requireNamespace("dplyr", quietly = TRUE)) {
    tbl_name <- ot$table_name %||% object_type_id
    return(dplyr::tbl(ctx$connection, tbl_name))
  }
  cli::cli_abort("Package {.pkg dplyr} is required for {.fn object_set}.")
}

# Thin wrapper used when dplyr is not available --------------------------

live_object_set <- function(live_con, object_type_id) {
  structure(
    list(con = live_con, type_id = object_type_id),
    class = "LiveObjectSet"
  )
}

#' Collect results from a LiveObjectSet (no-dplyr fallback)
#' @export
os_collect <- function(x, ...) UseMethod("os_collect")

#' @export
os_collect.LiveObjectSet <- function(x, ...) {
  sql <- paste0('SELECT * FROM "', x$type_id, '"')
  DBI::dbGetQuery(x$con, sql)
}

#' @export
os_collect.default <- function(x, ...) {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    return(dplyr::collect(x))
  }
  cli::cli_abort("Package {.pkg dplyr} is required.")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
