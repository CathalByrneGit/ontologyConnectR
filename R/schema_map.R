#' Validate a live-source schema map
#'
#' Each entry must have \code{resource} (character) and \code{columns} (named
#' character list mapping property IDs to source field names).
#'
#' @param schema_map Named list.
#' @return Invisible schema_map, or aborts on error.
validate_live_schema_map <- function(schema_map) {
  if (!is.list(schema_map) || is.null(names(schema_map))) {
    cli::cli_abort("{.arg schema_map} must be a named list.")
  }
  for (nm in names(schema_map)) {
    entry <- schema_map[[nm]]
    if (!is.list(entry) || !all(c("resource", "columns") %in% names(entry))) {
      cli::cli_abort(
        "schema_map[['{nm}']] must be a list with {.field resource} and {.field columns}."
      )
    }
    if (!is.list(entry$columns) || is.null(names(entry$columns))) {
      cli::cli_abort("schema_map[['{nm}']]$columns must be a named list.")
    }
  }
  invisible(schema_map)
}

#' Build a live connection from a bundle object type extension
#'
#' Reads the object type's \code{extensions$live_source} entry and constructs
#' the appropriate \code{LiveConnection}. Supported \code{connector_type}
#' values: \code{"rest"}, \code{"fhir_r4"}, \code{"jdbc"}.
#'
#' The object type can be given directly, or as a bundle plus an object type
#' id, in which case it is looked up with
#' \code{ontologySpecR::bundle_object_type()}.
#'
#' The source resource (REST path, FHIR resource type or table name) is taken
#' from \code{live_source$params$resource}, falling back to the legacy
#' \code{params$table_name}, and then to the object type's own source table
#' (\code{ontologySpecR::object_table()}, which defaults to the object type id).
#'
#' @param x An object type (an \code{ontology_object_type} from ontologySpecR,
#'   or a plain list with \code{id} and \code{extensions}), or a bundle.
#' @param object_type_id Character. Required when \code{x} is a bundle;
#'   must be \code{NULL} otherwise.
#' @param credentials Named list with source credentials.
#' @return A \code{LiveConnection} object.
#' @export
conn_from_bundle <- function(x, object_type_id = NULL, credentials = list()) {
  # Backwards compatibility: conn_from_bundle(object_type, credentials)
  if (is.list(object_type_id)) {
    credentials    <- object_type_id
    object_type_id <- NULL
  }
  object_type <- resolve_object_type(x, object_type_id)

  ext <- object_type$extensions$live_source
  if (is.null(ext)) {
    cli::cli_abort(
      "Object type {.val {object_type$id}} has no {.field live_source} extension."
    )
  }

  ct         <- ext$connector_type
  raw_cols   <- ext$schema_map %||% list()
  params     <- ext$params     %||% list()

  if (is.null(ct)) {
    cli::cli_abort(
      "{.field live_source} for {.val {object_type$id}} has no {.field connector_type}."
    )
  }

  # Wrap flat schema_map into the new nested format
  schema_map <- stats::setNames(
    list(list(
      resource = params$resource %||% params$table_name %||%
        default_object_table(object_type),
      pk       = params$pk_property %||% "id",
      columns  = raw_cols
    )),
    object_type$id
  )

  switch(ct,
    rest = live_rest(
      schema_map   = schema_map,
      base_url     = params$base_url %||% credentials$base_url,
      auth         = build_auth_from_creds(params$auth_type, credentials),
      cache_ttl    = params$cache_ttl %||% 0L
    ),
    fhir_r4 = live_fhir(
      schema_map = schema_map,
      base_url   = params$base_url %||% credentials$base_url,
      auth       = build_auth_from_creds(params$auth_type, credentials),
      cache_ttl  = params$cache_ttl %||% 0L
    ),
    jdbc = live_jdbc(
      schema_map   = schema_map,
      driver_class = params$driver_class,
      jdbc_url     = params$jdbc_url %||% credentials$jdbc_url,
      credentials  = list(
        username         = params$username %||% credentials$username,
        password_env_var = params$password_env_var %||% credentials$password_env_var
      ),
      driver_path = params$driver_path,
      dialect     = params$dialect %||% "duckdb",
      cache_ttl   = params$cache_ttl %||% 0L
    ),
    cli::cli_abort("Unknown connector_type: {.val {ct}}")
  )
}

# Resolve `x` (object type, or bundle + id) to a single object type.
resolve_object_type <- function(x, object_type_id = NULL) {
  if (is.null(object_type_id)) {
    if (inherits(x, "ontology_bundle")) {
      cli::cli_abort(
        "{.arg object_type_id} is required when {.arg x} is a bundle."
      )
    }
    if (!is.list(x) || is.null(x$id)) {
      cli::cli_abort(
        "{.arg x} must be an object type (a list with {.field id}) or a bundle."
      )
    }
    return(x)
  }

  if (!is.character(object_type_id) || length(object_type_id) != 1L) {
    cli::cli_abort("{.arg object_type_id} must be a single string.")
  }
  if (!requireNamespace("ontologySpecR", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ontologySpecR} is required to look up an object type in a bundle.",
      i = "Install it with {.code remotes::install_github(\"CathalByrneGit/ontologySpecR\")}, or pass the object type directly."
    ))
  }
  ontologySpecR::bundle_object_type(x, object_type_id)
}

# Default source resource for an object type: its source table, else its id.
default_object_table <- function(object_type) {
  if (requireNamespace("ontologySpecR", quietly = TRUE)) {
    return(ontologySpecR::object_table(object_type))
  }
  # Same rule as ontologySpecR::object_table(), for plain-list object types
  object_type$source$table %||% object_type$id
}

build_auth_from_creds <- function(auth_type, credentials) {
  if (is.null(auth_type)) return(NULL)
  switch(auth_type,
    bearer  = conn_auth_bearer(env_var = credentials$token_env_var),
    basic   = conn_auth_basic(credentials$username, credentials$password_env_var),
    api_key = conn_auth_api_key(key_env_var = credentials$api_key_env_var),
    NULL
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
