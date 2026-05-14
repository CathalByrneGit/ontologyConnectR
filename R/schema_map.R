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
#' Reads \code{object_type$extensions$live_source} and constructs the
#' appropriate \code{LiveConnection}. Supported \code{connector_type} values:
#' \code{"rest"}, \code{"fhir_r4"}, \code{"jdbc"}.
#'
#' @param object_type A list describing an object type (from ontologySpecR).
#' @param credentials Named list with source credentials.
#' @return A \code{LiveConnection} object.
#' @export
conn_from_bundle <- function(object_type, credentials = list()) {
  ext <- object_type$extensions$live_source
  if (is.null(ext)) {
    cli::cli_abort(
      "Object type {.val {object_type$id}} has no {.field live_source} extension."
    )
  }

  ct         <- ext$connector_type
  raw_cols   <- ext$schema_map %||% list()
  params     <- ext$params     %||% list()

  # Wrap flat schema_map into the new nested format
  schema_map <- stats::setNames(
    list(list(
      resource = params$resource %||% params$table_name %||% tolower(object_type$id),
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
      dialect    = params$dialect %||% "duckdb",
      cache_ttl  = params$cache_ttl %||% 0L
    ),
    cli::cli_abort("Unknown connector_type: {.val {ct}}")
  )
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
