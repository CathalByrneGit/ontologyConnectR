#' Validate a schema_map list
#'
#' @param schema_map Named list mapping property IDs to source field paths.
#' @return Invisible schema_map, or aborts on error.
validate_schema_map <- function(schema_map) {
  if (!is.list(schema_map) || is.null(names(schema_map))) {
    cli::cli_abort("{.arg schema_map} must be a named list.")
  }
  blank_names <- names(schema_map)[nchar(names(schema_map)) == 0]
  if (length(blank_names) > 0) {
    cli::cli_abort("schema_map has entries with blank names.")
  }
  non_char <- sapply(schema_map, function(v) !is.character(v) || length(v) != 1)
  if (any(non_char)) {
    cli::cli_abort("All schema_map values must be single character strings.")
  }
  invisible(schema_map)
}

#' Apply a schema_map to a parsed JSON record
#'
#' Extracts fields from a nested list (parsed JSON object) using the paths
#' defined in schema_map. Supports simple dot notation and bracket indexing.
#'
#' @param record A named list (parsed JSON object).
#' @param schema_map Named list (property_id -> json_path).
#' @return A named list of extracted values.
apply_schema_map_to_record <- function(record, schema_map) {
  out <- vector("list", length(schema_map))
  names(out) <- names(schema_map)
  for (prop in names(schema_map)) {
    path <- schema_map[[prop]]
    out[[prop]] <- extract_json_path(record, path)
  }
  out
}

#' Apply a schema_map to a list of records
#' @param records A list of named lists (parsed JSON array).
#' @param schema_map Named list.
#' @return A data.frame.
apply_schema_map <- function(records, schema_map) {
  if (length(records) == 0L) {
    empty <- as.data.frame(
      lapply(names(schema_map), function(nm) character(0)),
      stringsAsFactors = FALSE
    )
    names(empty) <- names(schema_map)
    return(empty)
  }
  rows <- lapply(records, apply_schema_map_to_record, schema_map = schema_map)
  as.data.frame(
    lapply(names(schema_map), function(nm) {
      vals <- sapply(rows, function(r) {
        v <- r[[nm]]
        if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[1])
      })
      vals
    }),
    stringsAsFactors = FALSE,
    col.names = names(schema_map)
  )
}

#' Extract a value from a nested list using a simple path expression
#'
#' Supports:
#'   "field"            -> record$field
#'   "a.b"              -> record$a$b
#'   "a[0].b"           -> record$a[[1]]$b  (0-indexed → 1-indexed)
#'   "a.first()"        -> record$a[[1]]
#'
#' @param record Named list.
#' @param path Character scalar.
#' @return The extracted value, or NA if not found.
extract_json_path <- function(record, path) {
  tryCatch(
    eval_json_path(record, path),
    error = function(e) NA_character_
  )
}

eval_json_path <- function(node, path) {
  if (is.null(node)) return(NA_character_)

  # Handle .first() shorthand
  path <- sub("\\.first\\(\\)$", "[0]", path)

  # Split on dots, respecting bracket notation
  parts <- split_path(path)

  for (part in parts) {
    if (is.null(node)) return(NA_character_)
    # Bracket index: field[n]
    m <- regmatches(part, regexpr("^(\\w+)\\[(\\d+)\\]$", part, perl = TRUE))
    if (length(m) == 1) {
      sub_m <- regmatches(m, regexec("^(\\w+)\\[(\\d+)\\]$", m, perl = TRUE))[[1]]
      field <- sub_m[2]
      idx   <- as.integer(sub_m[3]) + 1L  # 0-indexed → 1-indexed
      node  <- node[[field]]
      if (is.null(node) || !is.list(node)) return(NA_character_)
      node  <- node[[idx]]
    } else {
      node <- node[[part]]
    }
  }
  if (is.null(node)) NA_character_ else node
}

split_path <- function(path) {
  # Split on dots not inside brackets
  parts <- character(0)
  current <- ""
  depth <- 0L
  for (ch in strsplit(path, "")[[1]]) {
    if (ch == "[") depth <- depth + 1L
    if (ch == "]") depth <- depth - 1L
    if (ch == "." && depth == 0L) {
      parts <- c(parts, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }
  if (nchar(current) > 0) parts <- c(parts, current)
  parts
}

#' Build a connector from a bundle object type extension
#'
#' Reads `object_type$extensions$live_source` and constructs the appropriate
#' connector. Supported connector_type values: "rest", "fhir_r4", "jdbc",
#' "graphql".
#'
#' @param object_type A list describing an object type (from ontologySpecR).
#' @param credentials Named list with connection credentials.
#' @return An OntologyConnector object.
#' @export
conn_from_bundle <- function(object_type, credentials = list()) {
  ext <- object_type$extensions$live_source
  if (is.null(ext)) {
    cli::cli_abort(
      "Object type {.val {object_type$id}} has no {.field live_source} extension."
    )
  }

  connector_type <- ext$connector_type
  schema_map     <- ext$schema_map %||% list()
  params         <- ext$params     %||% list()

  switch(connector_type,
    rest = {
      conn_rest(
        base_url    = params$base_url   %||% credentials$base_url,
        list_path   = params$list_path,
        get_path    = params$get_path,
        count_path  = params$count_path,
        schema_map  = schema_map,
        pk_property = params$pk_property %||% "id",
        auth        = build_auth_from_creds(params$auth_type, credentials),
        cache_ttl   = params$cache_ttl %||% 0L
      )
    },
    fhir_r4 = {
      conn_fhir(
        base_url      = params$base_url %||% credentials$base_url,
        resource_type = params$resource_type,
        schema_map    = schema_map,
        auth          = build_auth_from_creds(params$auth_type, credentials),
        cache_ttl     = params$cache_ttl %||% 0L
      )
    },
    jdbc = {
      conn_jdbc(
        driver_class = params$driver_class,
        jdbc_url     = params$jdbc_url %||% credentials$jdbc_url,
        table_name   = params$table_name,
        schema_map   = schema_map,
        credentials  = list(
          username         = params$username %||% credentials$username,
          password_env_var = params$password_env_var %||% credentials$password_env_var
        )
      )
    },
    graphql = {
      conn_graphql(
        endpoint       = params$endpoint %||% credentials$endpoint,
        query_template = params$query_template,
        data_path      = params$data_path,
        schema_map     = schema_map,
        auth           = build_auth_from_creds(params$auth_type, credentials),
        cache_ttl      = params$cache_ttl %||% 0L
      )
    },
    cli::cli_abort("Unknown connector_type: {.val {connector_type}}")
  )
}

build_auth_from_creds <- function(auth_type, credentials) {
  if (is.null(auth_type)) return(NULL)
  switch(auth_type,
    bearer  = conn_auth_bearer(env_var = credentials$token_env_var),
    basic   = conn_auth_basic(credentials$username,
                               credentials$password_env_var),
    api_key = conn_auth_api_key(key_env_var = credentials$api_key_env_var),
    NULL
  )
}
