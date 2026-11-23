#' Create a connector context with optional live-source connections
#'
#' Associates an ontologySpecR bundle with a DBI connection and/or live source
#' connections. Object types backed by a live connection are served through
#' that connection's dbplyr backend; the others use the DBI \code{connection}.
#'
#' This is ontologyConnectR's own lightweight context. To use live connections
#' with objectSetsR instead, pass them to objectSetsR's context, fully
#' qualified to avoid confusion:
#' \code{objectSetsR::object_set(objectSetsR::ontology_context(...), "Type")}.
#'
#' @param bundle An ontologySpecR bundle, or anything
#'   \code{ontologySpecR::as_bundle()} accepts (a list in the spec layout, or
#'   a path to a bundle JSON file).
#' @param connection A DBI connection, or NULL when all types have live
#'   connections.
#' @param live_connections Named list of \code{LiveConnection} objects. Names
#'   must match object type IDs in the bundle.
#' @param strict Logical. If TRUE, abort on object type IDs in
#'   \code{live_connections} that aren't in the bundle.
#' @param check_interfaces Logical. Reserved; currently unused.
#' @return A \code{ConnectContext} list.
#' @seealso \code{\link[=connect_object_set]{connect_object_set()}}
#' @export
connect_context <- function(bundle,
                            connection       = NULL,
                            live_connections = list(),
                            strict           = FALSE,
                            check_interfaces = TRUE) {
  if (!is.list(live_connections) ||
      (length(live_connections) > 0 && is.null(names(live_connections)))) {
    cli::cli_abort("{.arg live_connections} must be a named list.")
  }

  if (is.list(bundle) && !inherits(bundle, "ontology_bundle") &&
      !is.null(bundle[["object_types"]]) && is.null(bundle[["objects"]])) {
    cli::cli_abort(c(
      "{.arg bundle} uses the old {.field object_types} layout.",
      i = "Build it with {.fn ontologySpecR::bundle}; object types go in {.field objects}."
    ))
  }
  bundle <- ontologySpecR::as_bundle(bundle)
  ot_ids <- names(ontologySpecR::bundle_object_types(bundle))

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
    class = "ConnectContext"
  )
}

#' @export
print.ConnectContext <- function(x, ...) {
  ot_ids   <- names(ontologySpecR::bundle_object_types(x$bundle))
  live_ids <- names(x$live_connections)
  dbi_ids  <- setdiff(ot_ids, live_ids)
  cli::cli_h1("ConnectContext")
  cli::cli_bullets(c(
    "*" = "Object types: {length(ot_ids)}",
    "*" = "Live connections: {length(live_ids)} ({.val {live_ids}})",
    "*" = "DBI-backed: {length(dbi_ids)} ({.val {dbi_ids}})"
  ))
  invisible(x)
}

#' Get a lazy table for an object type
#'
#' For types backed by a live connection, returns
#' \code{dplyr::tbl(live_con, object_type_id)}. Otherwise the object type is
#' looked up in the bundle with \code{ontologySpecR::bundle_object_type()},
#' and the table comes from the DBI connection: the object type's source
#' table (\code{ontologySpecR::object_table()}, schema-qualified when
#' \code{source$schema} is set), or its defining SQL for query-backed types
#' (\code{ontologySpecR::object_source_sql()}).
#'
#' @param ctx A \code{ConnectContext}, from \code{connect_context()}.
#' @param object_type_id Character scalar.
#' @return A lazy \code{tbl} object (class \code{tbl_dbi} / \code{tbl_lazy}).
#' @export
connect_object_set <- function(ctx, object_type_id) {
  if (!inherits(ctx, "ConnectContext")) {
    cli::cli_abort("{.arg ctx} must be a {.cls ConnectContext}, from {.fn connect_context}.")
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
  ot <- ontologySpecR::bundle_object_type(ctx$bundle, object_type_id)
  if (is.null(ctx$connection)) {
    cli::cli_abort(
      "No live connection and no DBI connection for type {.val {object_type_id}}."
    )
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg dplyr} is required for {.fn connect_object_set}.")
  }

  src <- ontologySpecR::object_source(ot)
  if (identical(src$kind, "query")) {
    sql <- ontologySpecR::object_source_sql(ot)
    if (is.null(sql)) {
      cli::cli_abort(
        "Object type {.val {object_type_id}} is query-backed but has no source SQL."
      )
    }
    return(dplyr::tbl(ctx$connection, dplyr::sql(sql)))
  }

  table <- ontologySpecR::object_table(ot)
  if (!is.null(src$schema)) {
    table <- DBI::Id(schema = src$schema, table = table)
  }
  dplyr::tbl(ctx$connection, table)
}

# Deprecated names ---------------------------------------------------------

#' Deprecated context functions
#'
#' `ontology_context()` and `object_set()` were renamed to
#' [connect_context()] and [connect_object_set()] in 0.3.0, because objectSetsR
#' exports functions with the same names that do different things. They warn
#' once per session and call the new function. They will be removed in 0.4.0.
#'
#' @param ... Passed to the new function.
#' @return See [connect_context()] and [connect_object_set()].
#' @name ontologyConnectR-deprecated
#' @keywords internal
NULL

#' @rdname ontologyConnectR-deprecated
#' @export
ontology_context <- function(...) {
  warn_deprecated_name("ontology_context", "connect_context")
  connect_context(...)
}

#' @rdname ontologyConnectR-deprecated
#' @export
object_set <- function(...) {
  warn_deprecated_name("object_set", "connect_object_set")
  connect_object_set(...)
}

warn_deprecated_name <- function(old, new) {
  cli::cli_warn(
    c(
      "{.fn ontologyConnectR::{old}} is deprecated as of ontologyConnectR 0.3.0; use {.fn {new}} instead.",
      i = "It will be removed in 0.4.0."
    ),
    .frequency = "once",
    .frequency_id = paste0("ontologyConnectR_deprecated_", old),
    class = c("ontologyConnectR_deprecated", "deprecatedWarning")
  )
}

# Thin wrapper used when dplyr is not available --------------------------

live_object_set <- function(live_con, object_type_id) {
  structure(
    list(con = live_con, type_id = object_type_id),
    class = "LiveObjectSet"
  )
}

#' Collect results from a LiveObjectSet (no-dplyr fallback)
#'
#' @param x A \code{LiveObjectSet}, as returned by \code{connect_object_set()} when
#'   dplyr isn't installed.
#' @param ... Unused.
#' @return A data frame.
#' @export
connect_collect <- function(x, ...) UseMethod("connect_collect")

#' @export
connect_collect.LiveObjectSet <- function(x, ...) {
  sql <- paste0('SELECT * FROM "', x$type_id, '"')
  DBI::dbGetQuery(x$con, sql)
}

#' @export
connect_collect.default <- function(x, ...) {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    return(dplyr::collect(x))
  }
  cli::cli_abort("Package {.pkg dplyr} is required.")
}

`%||%` <- function(a, b) if (is.null(a)) b else a
