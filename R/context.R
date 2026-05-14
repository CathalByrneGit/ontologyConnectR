#' Create an ontology context with optional live-source connectors
#'
#' Wraps an ontology bundle (from ontologySpecR) and associates live-source
#' connectors with specific object types. When a connector is present for an
#' object type, queries use the connector instead of a DBI connection.
#'
#' @param bundle An ontology bundle (list with `$object_types` named list).
#' @param connection A DBI connection, or NULL if all types have connectors.
#' @param connectors Named list of OntologyConnector objects. Names must match
#'   object_type IDs in the bundle.
#' @param strict Logical. If TRUE, error on unknown object type IDs in connectors.
#' @param check_interfaces Logical. If TRUE, warn when connectors are missing
#'   required methods.
#' @return An `OntologyContext` object.
#' @export
ontology_context <- function(bundle,
                              connection = NULL,
                              connectors = list(),
                              strict = FALSE,
                              check_interfaces = TRUE) {
  if (!is.list(connectors) || (length(connectors) > 0 && is.null(names(connectors)))) {
    cli::cli_abort("{.arg connectors} must be a named list.")
  }

  # Determine available object type IDs
  ot_ids <- names(bundle$object_types)

  if (strict && length(connectors) > 0) {
    unknown <- setdiff(names(connectors), ot_ids)
    if (length(unknown) > 0) {
      cli::cli_abort(
        "Unknown object type IDs in {.arg connectors}: {.val {unknown}}"
      )
    }
  }

  if (check_interfaces) {
    for (nm in names(connectors)) {
      conn <- connectors[[nm]]
      if (!is.function(try(conn_fetch, silent = TRUE))) next
    }
  }

  # Validate that DBI connection is provided when needed
  dbi_types <- setdiff(ot_ids, names(connectors))
  if (length(dbi_types) > 0 && is.null(connection)) {
    cli::cli_warn(c(
      "Some object types have no connector and no DBI connection.",
      i = "Types without connector: {.val {dbi_types}}"
    ))
  }

  ctx <- structure(
    list(
      bundle     = bundle,
      connection = connection,
      connectors = connectors
    ),
    class = "OntologyContext"
  )
  ctx
}

#' @export
print.OntologyContext <- function(x, ...) {
  ot_ids    <- names(x$bundle$object_types)
  conn_ids  <- names(x$connectors)
  dbi_ids   <- setdiff(ot_ids, conn_ids)

  cli::cli_h1("OntologyContext")
  cli::cli_bullets(c(
    "*" = "Object types: {length(ot_ids)}",
    "*" = "Live connector types: {length(conn_ids)} ({.val {conn_ids}})",
    "*" = "DBI-backed types: {length(dbi_ids)} ({.val {dbi_ids}})"
  ))
  invisible(x)
}

#' Get or create an object set for an object type
#'
#' Dispatches to a `ConnectorObjectSet` if the type has a live connector,
#' or falls back to a DBI-backed object set.
#'
#' @param ctx An `OntologyContext`.
#' @param object_type_id Character scalar. The object type ID.
#' @return An object set (ConnectorObjectSet or DBI-backed ObjectSet).
#' @export
object_set <- function(ctx, object_type_id) {
  if (!inherits(ctx, "OntologyContext")) {
    cli::cli_abort("{.arg ctx} must be an {.cls OntologyContext}.")
  }
  if (object_type_id %in% names(ctx$connectors)) {
    connector_object_set(ctx, object_type_id)
  } else {
    dbi_object_set(ctx, object_type_id)
  }
}

#' Create a DBI-backed object set (internal fallback)
#' @param ctx OntologyContext
#' @param object_type_id character
#' @return a list with class ObjectSet
dbi_object_set <- function(ctx, object_type_id) {
  if (is.null(ctx$connection)) {
    cli::cli_abort(
      "No DBI connection and no connector for type {.val {object_type_id}}."
    )
  }
  ot <- ctx$bundle$object_types[[object_type_id]]
  if (is.null(ot)) {
    cli::cli_abort("Unknown object type: {.val {object_type_id}}.")
  }
  table_name <- ot$table_name %||% object_type_id
  structure(
    list(
      ctx            = ctx,
      object_type_id = object_type_id,
      table_name     = table_name,
      pending_filters = list(),
      pending_select  = NULL,
      pending_limit   = NULL
    ),
    class = c("DbiObjectSet", "ObjectSet")
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
