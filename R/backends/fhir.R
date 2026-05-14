#' Create a FHIR R4 live connection
#'
#' Returns a \code{LiveFhirConnection} S4 object. dbplyr-generated SQL is
#' intercepted and translated to FHIR search parameters. Filters using
#' \code{=}, \code{!=}, \code{>}, \code{>=}, \code{<}, \code{<=}, \code{IN},
#' and \code{LIKE} are pushed to the FHIR server; others are applied in R.
#'
#' @param schema_map Named list. Each name is an object type ID; each value is
#'   \code{list(resource, pk, columns, search_params)} where \code{search_params}
#'   maps property IDs to FHIR search parameter names.
#' @param base_url FHIR server base URL.
#' @param auth An auth helper, or NULL.
#' @param cache_ttl Integer. Cache TTL in seconds.
#' @return A \code{LiveFhirConnection} object.
#' @export
live_fhir <- function(schema_map,
                       base_url,
                       auth      = NULL,
                       cache_ttl = 0L) {
  validate_live_schema_map(schema_map)
  new_live_connection(
    "LiveFhirConnection",
    source_type = "fhir",
    schema_map  = schema_map,
    cache_ttl   = cache_ttl,
    config = list(base_url = sub("/$", "", base_url), auth = auth)
  )
}

#' @export
setMethod("dispatch_query", "LiveFhirConnection", function(conn, source_query, ...) {
  schema <- conn@schema_map[[source_query$table]]
  sp_map <- schema$search_params %||% list()

  # Translate filters to FHIR search params
  fhir_params <- list()
  r_side      <- list()

  for (f in source_query$filters) {
    if (is.null(f$source_column)) { r_side <- c(r_side, list(f)); next }
    sp_name <- sp_map[[f$source_column]] %||% fhir_default_param(f$source_column)
    fhir_val <- fhir_filter_value(f$op, f$value)
    if (!is.null(fhir_val)) {
      key <- if (f$op == "LIKE") paste0(sp_name, ":contains") else sp_name
      fhir_params[[key]] <- fhir_val
    } else {
      r_side <- c(r_side, list(f))
    }
  }

  if (!is.null(source_query$limit)) {
    fhir_params[["_count"]] <- min(source_query$limit + (source_query$offset %||% 0L), 1000L)
  }
  if (!is.null(source_query$select)) {
    fhir_params[["_elements"]] <- paste(source_query$select, collapse = ",")
  }

  url <- paste0(conn@config$base_url, "/", source_query$resource)
  all_resources <- list()

  repeat {
    req <- httr2::request(url)
    if (length(fhir_params)) {
      req <- do.call(httr2::req_url_query, c(list(req), fhir_params))
    }
    req    <- apply_auth(req, conn@config$auth)
    resp   <- httr2::req_perform(req)
    bundle <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    resources <- extract_fhir_bundle(bundle)
    all_resources <- c(all_resources, resources)

    lim <- source_query$limit %||% Inf
    if (length(resources) == 0L || length(all_resources) >= lim + (source_query$offset %||% 0L)) break
    next_url <- fhir_next_link(bundle)
    if (is.null(next_url)) break
    url <- next_url
    fhir_params <- list()  # params embedded in next URL
  }

  df <- fhirpath_to_df(all_resources, schema$columns)

  for (f in r_side) {
    fn <- make_r_filter(f$source_column %||% f$column, f$op, f$value)
    df <- fn(df)
  }

  off   <- source_query$offset %||% 0L
  n     <- nrow(df)
  start <- min(off + 1L, n + 1L)
  end   <- if (!is.null(source_query$limit)) min(off + source_query$limit, n) else n
  if (start <= end) df[seq(start, end), , drop = FALSE] else df[integer(0), , drop = FALSE]
})

# FHIR helpers ------------------------------------------------------------

fhir_filter_value <- function(op, value) {
  switch(op,
    "="  = as.character(value),
    "!=" = paste0("ne", value),
    ">"  = paste0("gt", value),
    ">=" = paste0("ge", value),
    "<"  = paste0("lt", value),
    "<=" = paste0("le", value),
    "IN" = paste(value, collapse = ","),
    "LIKE" = {
      # Strip leading/trailing % for :contains
      as.character(gsub("^%|%$", "", value))
    },
    NULL  # not translatable → R-side filter
  )
}

fhir_default_param <- function(source_field) {
  defaults <- c(
    id          = "_id",
    family      = "family",
    given       = "given",
    birthDate   = "birthdate",
    gender      = "gender",
    status      = "status",
    code        = "code",
    date        = "date"
  )
  defaults[[source_field]] %||% gsub("_", "-", source_field)
}

extract_fhir_bundle <- function(bundle) {
  if (is.null(bundle$entry)) return(list())
  lapply(bundle$entry, function(e) e$resource)
}

fhir_next_link <- function(bundle) {
  for (lnk in bundle$link %||% list()) {
    if (!is.null(lnk$relation) && lnk$relation == "next") return(lnk$url)
  }
  NULL
}

fhirpath_to_df <- function(resources, columns) {
  if (!length(resources)) {
    out <- as.data.frame(matrix(nrow = 0L, ncol = length(columns)),
                          stringsAsFactors = FALSE)
    names(out) <- names(columns)
    return(out)
  }
  rows <- lapply(resources, function(res) {
    lapply(names(columns), function(prop) {
      path <- columns[[prop]]
      val  <- eval_fhirpath(res, path)
      if (is.null(val) || length(val) == 0L) NA_character_ else as.character(val[1L])
    })
  })
  df <- as.data.frame(
    lapply(seq_along(columns), function(i) {
      vapply(rows, function(r) r[[i]] %||% NA_character_, character(1))
    }),
    stringsAsFactors = FALSE
  )
  names(df) <- names(columns)
  df
}

eval_fhirpath <- function(node, fp) {
  tryCatch(eval_fhirpath_inner(node, fp), error = function(e) NA_character_)
}

eval_fhirpath_inner <- function(node, fp) {
  if (is.null(node)) return(NA_character_)
  fp <- sub("\\.ofType\\([^)]+\\)$", "", fp)
  fp <- sub("\\.first\\(\\)$",       "[0]", fp)

  # .where(field='value')
  wm <- regexpr("(\\w+)\\.where\\(([^=]+)='([^']+)'\\)", fp, perl = TRUE)
  if (wm > 0L) {
    cs <- attr(wm, "capture.start"); cl <- attr(wm, "capture.length")
    field  <- substr(fp, cs[1], cs[1] + cl[1] - 1L)
    attr_k <- substr(fp, cs[2], cs[2] + cl[2] - 1L)
    attr_v <- substr(fp, cs[3], cs[3] + cl[3] - 1L)
    suffix <- substr(fp, wm + attr(wm, "match.length"), nchar(fp))
    items  <- node[[field]]
    matched <- Filter(function(x) !is.null(x[[attr_k]]) && x[[attr_k]] == attr_v, items)
    if (!length(matched)) return(NA_character_)
    node <- matched[[1L]]
    if (nchar(suffix) > 0L) return(eval_fhirpath_inner(node, sub("^\\.", "", suffix)))
    return(node)
  }

  # Simple path
  parts <- strsplit(fp, "\\.")[[1]]
  for (part in parts) {
    if (is.null(node)) return(NA_character_)
    bm <- regexpr("^(\\w+)\\[(\\d+)\\]$", part, perl = TRUE)
    if (bm > 0L) {
      cs <- attr(bm, "capture.start"); cl <- attr(bm, "capture.length")
      fname <- substr(part, cs[1], cs[1] + cl[1] - 1L)
      idx   <- as.integer(substr(part, cs[2], cs[2] + cl[2] - 1L)) + 1L
      node  <- node[[fname]]
      if (is.null(node) || !is.list(node)) return(NA_character_)
      node  <- node[[idx]]
    } else {
      node <- node[[part]]
    }
  }
  if (is.null(node)) NA_character_ else node
}

`%||%` <- function(a, b) if (is.null(a)) b else a
