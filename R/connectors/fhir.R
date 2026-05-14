#' Create a FHIR R4 connector
#'
#' Queries a FHIR R4 server using the FHIR search API. Supports push-down of
#' common filter operations (eq, contains, gt, gte, lt, lte, in) and automatic
#' pagination via Bundle links.
#'
#' @param base_url FHIR server base URL, e.g. "https://hapi.fhir.org/baseR4".
#' @param resource_type FHIR resource type, e.g. "Patient", "Encounter".
#' @param schema_map Named list mapping property IDs to FHIRPath expressions.
#' @param auth Authentication object, or NULL.
#' @param cache_ttl Integer. Cache TTL in seconds.
#' @return A list of class `conn_fhir`.
#' @export
conn_fhir <- function(base_url,
                       resource_type,
                       schema_map,
                       auth      = NULL,
                       cache_ttl = 0L) {
  validate_schema_map(schema_map)
  conn <- list(
    base_url      = sub("/$", "", base_url),
    resource_type = resource_type,
    schema_map    = schema_map,
    auth          = auth,
    pk_property   = "id",
    connector_id  = paste0("fhir:", base_url, "/", resource_type)
  )
  conn <- init_cache(conn, cache_ttl)
  class(conn) <- c("conn_fhir", "OntologyConnector")
  conn
}

#' @export
conn_supports_filter.conn_fhir <- function(connector, op) {
  op %in% c("eq", "contains", "gt", "gte", "lt", "lte", "in")
}

#' @export
conn_fetch.conn_fhir <- function(connector, filters = list(),
                                   select = NULL, limit = NULL, offset = 0L) {
  pushable <- Filter(function(f) conn_supports_filter(connector, f$op), filters)
  r_side   <- Filter(function(f) !conn_supports_filter(connector, f$op), filters)

  search_params <- fhir_filters_to_params(pushable, connector$schema_map)

  url <- paste0(connector$base_url, "/", connector$resource_type)
  all_resources <- list()
  next_url      <- NULL

  repeat {
    req <- httr2::request(url)

    if (length(search_params) > 0) {
      req <- do.call(httr2::req_url_query, c(list(req), search_params))
    }
    if (!is.null(limit)) {
      req <- httr2::req_url_query(req, `_count` = min(limit, 1000L))
    }

    req  <- apply_auth(req, connector$auth)
    resp <- httr2::req_perform(req)
    bundle <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    resources <- extract_fhir_resources(bundle)
    all_resources <- c(all_resources, resources)

    if (!is.null(limit) && length(all_resources) >= limit + offset) break

    next_url <- get_fhir_next_url(bundle)
    if (is.null(next_url)) break
    url <- next_url
    search_params <- list()  # params are embedded in next_url
  }

  df <- fhirpath_schema_map(all_resources, connector$schema_map)
  df <- translate_filters_to_r(r_side)(df)
  df <- apply_limit(df, limit, offset)
  df
}

#' @export
conn_count.conn_fhir <- function(connector, filters = list()) {
  pushable     <- Filter(function(f) conn_supports_filter(connector, f$op), filters)
  search_params <- fhir_filters_to_params(pushable, connector$schema_map)
  search_params[["_summary"]] <- "count"

  url <- paste0(connector$base_url, "/", connector$resource_type)
  req <- httr2::request(url)
  if (length(search_params) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), search_params))
  }
  req    <- apply_auth(req, connector$auth)
  resp   <- httr2::req_perform(req)
  bundle <- httr2::resp_body_json(resp)
  as.integer(bundle$total %||% 0L)
}

#' @export
conn_get_one.conn_fhir <- function(connector, pk_value) {
  url  <- paste0(connector$base_url, "/", connector$resource_type, "/", pk_value)
  req  <- apply_auth(httr2::request(url), connector$auth)
  resp <- tryCatch(
    httr2::req_perform(req),
    httr2_http_404 = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  resource <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  fhirpath_schema_map(list(resource), connector$schema_map)
}

# FHIR helpers ------------------------------------------------------------

fhir_filters_to_params <- function(filters, schema_map) {
  params <- list()
  for (f in filters) {
    # Map property_id to FHIR search param name (use schema_map key as hint,
    # or fall back to property itself)
    fhir_param <- property_to_fhir_param(f$property)

    switch(f$op,
      eq       = { params[[fhir_param]] <- as.character(f$value) },
      contains = { params[[paste0(fhir_param, ":contains")]] <- as.character(f$value) },
      gt       = { params[[fhir_param]] <- paste0("gt", f$value) },
      gte      = { params[[fhir_param]] <- paste0("ge", f$value) },
      lt       = { params[[fhir_param]] <- paste0("lt", f$value) },
      lte      = { params[[fhir_param]] <- paste0("le", f$value) },
      `in`     = { params[[fhir_param]] <- paste(f$value, collapse = ",") }
    )
  }
  params
}

property_to_fhir_param <- function(property) {
  # Common FHIR search parameter name mappings
  map <- c(
    family_name = "family",
    given_name  = "given",
    birth_date  = "birthdate",
    gender      = "gender",
    patient_id  = "_id",
    id          = "_id",
    status      = "status",
    code        = "code",
    date        = "date"
  )
  map[property] %||% gsub("_", "-", property)
}

extract_fhir_resources <- function(bundle) {
  if (is.null(bundle$entry)) return(list())
  lapply(bundle$entry, function(e) e$resource)
}

get_fhir_next_url <- function(bundle) {
  if (is.null(bundle$link)) return(NULL)
  for (link in bundle$link) {
    if (!is.null(link$relation) && link$relation == "next") {
      return(link$url)
    }
  }
  NULL
}

# FHIRPath evaluation (simplified subset) ---------------------------------

fhirpath_schema_map <- function(resources, schema_map) {
  if (length(resources) == 0L) {
    empty <- as.data.frame(
      lapply(names(schema_map), function(nm) character(0)),
      stringsAsFactors = FALSE
    )
    names(empty) <- names(schema_map)
    return(empty)
  }
  rows <- lapply(resources, function(res) {
    out <- vector("list", length(schema_map))
    names(out) <- names(schema_map)
    for (prop in names(schema_map)) {
      fp  <- schema_map[[prop]]
      val <- eval_fhirpath(res, fp)
      out[[prop]] <- if (is.null(val) || length(val) == 0) NA_character_ else as.character(val[1])
    }
    out
  })
  as.data.frame(
    lapply(names(schema_map), function(nm) {
      sapply(rows, function(r) r[[nm]] %||% NA_character_)
    }),
    stringsAsFactors = FALSE,
    col.names = names(schema_map)
  )
}

eval_fhirpath <- function(node, fp) {
  tryCatch(eval_fhirpath_inner(node, fp), error = function(e) NA_character_)
}

eval_fhirpath_inner <- function(node, fp) {
  if (is.null(node)) return(NA_character_)

  # Handle .ofType(boolean) etc.
  fp <- sub("\\.ofType\\([^)]+\\)$", "", fp)

  # Handle .first()
  fp <- sub("\\.first\\(\\)$", "[0]", fp)

  # Handle .where(use='official') — find item where $use == "official"
  where_m <- regexpr("(\\w+)\\.where\\(([^=]+)='([^']+)'\\)", fp, perl = TRUE)
  if (where_m > 0) {
    starts <- attr(where_m, "capture.start")
    lens   <- attr(where_m, "capture.length")
    field  <- substr(fp, starts[1], starts[1] + lens[1] - 1)
    attr_k <- substr(fp, starts[2], starts[2] + lens[2] - 1)
    attr_v <- substr(fp, starts[3], starts[3] + lens[3] - 1)
    suffix <- substr(fp, where_m + attr(where_m, "match.length"), nchar(fp))
    items  <- node[[field]]
    if (is.null(items)) return(NA_character_)
    matched <- Filter(function(item) {
      !is.null(item[[attr_k]]) && item[[attr_k]] == attr_v
    }, items)
    if (length(matched) == 0) return(NA_character_)
    node <- matched[[1]]
    if (nchar(suffix) > 0) {
      return(eval_fhirpath_inner(node, sub("^\\.", "", suffix)))
    }
    return(node)
  }

  # Fall back to simple dot-path / bracket extraction
  extract_json_path(node, fp)
}
