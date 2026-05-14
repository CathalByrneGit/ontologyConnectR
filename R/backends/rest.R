#' Create a REST API live connection
#'
#' Returns a \code{LiveRestConnection} S4 object. Call
#' \code{ontology_context(bundle, live_connections = list(MyType = conn))}
#' and then use \code{dplyr::tbl()} / \code{objectSetsR::object_set()} as
#' normal. dbplyr generates SQL which is intercepted and translated to HTTP
#' query parameters.
#'
#' @param schema_map Named list. Each name is an object type ID; each value
#'   is \code{list(resource, pk, columns = list(property_id = source_field))}.
#' @param base_url Base URL (no trailing slash).
#' @param auth An auth helper (\code{conn_auth_*}), or NULL.
#' @param pagination A pagination helper (\code{conn_pagination_*}), or NULL.
#' @param data_path Character. JSON path to the records array in the response.
#' @param filter_style One of \code{"query_params"} (default), \code{"odata"}.
#' @param cache_ttl Integer. Cache TTL in seconds; 0 disables caching.
#' @return A \code{LiveRestConnection} object.
#' @export
live_rest <- function(schema_map,
                       base_url,
                       auth         = NULL,
                       pagination   = NULL,
                       data_path    = NULL,
                       filter_style = c("query_params", "odata"),
                       cache_ttl    = 0L) {
  filter_style <- match.arg(filter_style)
  validate_live_schema_map(schema_map)
  new_live_connection(
    "LiveRestConnection",
    source_type = "rest",
    schema_map  = schema_map,
    cache_ttl   = cache_ttl,
    config = list(
      base_url     = sub("/$", "", base_url),
      auth         = auth,
      pagination   = pagination,
      data_path    = data_path,
      filter_style = filter_style
    )
  )
}

#' @export
setMethod("dispatch_query", "LiveRestConnection", function(conn, source_query, ...) {
  cfg <- conn@config

  # Build pushable query params from equality filters (safest default)
  push_ops  <- c("=")
  pushable  <- Filter(function(f) f$op %in% push_ops && !is.null(f$source_column),
                      source_query$filters)
  r_side    <- Filter(function(f) !(f$op %in% push_ops) || is.null(f$source_column),
                      source_query$filters)

  query_params <- lapply(pushable, function(f) {
    stats::setNames(list(as.character(f$value)), f$source_column)
  })
  query_params <- do.call(c, c(list(), query_params))

  # Select fields
  if (!is.null(source_query$select)) {
    query_params[["fields"]] <- paste(source_query$select, collapse = ",")
  }

  all_rows      <- list()
  pagination    <- cfg$pagination
  page_num      <- 0L
  cursor        <- NULL
  next_url      <- NULL
  total_fetched <- 0L
  base_path     <- paste0(cfg$base_url, "/", source_query$resource)

  repeat {
    req_url <- if (!is.null(next_url) && inherits(pagination, "conn_pagination_link")) {
      next_url
    } else {
      base_path
    }
    req <- httr2::request(req_url)
    if (length(query_params)) {
      req <- do.call(httr2::req_url_query, c(list(req), query_params))
    }
    if (!is.null(pagination)) {
      req <- add_rest_pagination(req, pagination, page_num, cursor)
    }
    req  <- apply_auth(req, cfg$auth)
    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    records <- extract_records(body, cfg$data_path)
    all_rows <- c(all_rows, records)
    total_fetched <- total_fetched + length(records)

    lim <- source_query$limit %||% Inf
    if (length(records) == 0L || total_fetched >= lim + (source_query$offset %||% 0L)) break
    if (is.null(pagination)) break

    nxt <- get_rest_next_page(body, resp, pagination, page_num, cursor)
    if (is.null(nxt)) break
    page_num <- nxt$page_num
    cursor   <- nxt$cursor
    next_url <- nxt$next_url
  }

  df <- apply_rest_schema_map(all_rows, source_query$select)

  # Apply R-side filters
  for (f in r_side) {
    if (is.null(f$source_column)) next
    fn <- make_r_filter(f$source_column, f$op, f$value)
    df <- fn(df)
  }

  # Apply limit/offset
  n     <- nrow(df)
  off   <- source_query$offset %||% 0L
  start <- min(off + 1L, n + 1L)
  end   <- if (!is.null(source_query$limit)) min(off + source_query$limit, n) else n
  if (start <= end) df[seq(start, end), , drop = FALSE] else df[integer(0), , drop = FALSE]
})

# REST helpers ------------------------------------------------------------

extract_records <- function(body, data_path) {
  if (!is.null(data_path) && nchar(data_path)) {
    node <- body
    for (part in strsplit(data_path, "\\.")[[1]]) node <- node[[part]]
    if (is.list(node)) return(node)
    return(list())
  }
  # Auto-detect wrapper key
  if (is.list(body) && !is.data.frame(body)) {
    for (key in c("data", "results", "items", "records", "content", "value")) {
      if (!is.null(body[[key]]) && is.list(body[[key]])) return(body[[key]])
    }
    if (length(body) && is.list(body[[1]])) return(body)
  }
  list()
}

apply_rest_schema_map <- function(records, source_select) {
  if (!length(records)) {
    cols <- source_select %||% character(0)
    out  <- as.data.frame(matrix(nrow = 0L, ncol = length(cols)),
                           stringsAsFactors = FALSE)
    names(out) <- cols
    return(out)
  }
  cols <- if (!is.null(source_select)) source_select else unique(unlist(lapply(records, names)))
  df   <- as.data.frame(
    lapply(cols, function(col) {
      vapply(records, function(r) {
        v <- r[[col]]
        if (is.null(v) || length(v) == 0L) NA_character_ else as.character(v[1L])
      }, character(1))
    }),
    stringsAsFactors = FALSE
  )
  names(df) <- cols
  df
}

add_rest_pagination <- function(req, pag, page_num, cursor) {
  if (inherits(pag, "conn_pagination_offset")) {
    req <- httr2::req_url_query(req,
      !!pag$page_param := page_num, !!pag$size_param := pag$page_size)
  } else if (inherits(pag, "conn_pagination_cursor") && !is.null(cursor)) {
    req <- httr2::req_url_query(req, !!pag$cursor_param := cursor)
  }
  req
}

get_rest_next_page <- function(body, resp, pag, page_num, cursor) {
  if (inherits(pag, "conn_pagination_offset")) {
    list(page_num = page_num + 1L, cursor = NULL, next_url = NULL)
  } else if (inherits(pag, "conn_pagination_cursor")) {
    nc <- body[[pag$cursor_path]]
    if (is.null(nc)) NULL else list(page_num = 0L, cursor = nc, next_url = NULL)
  } else if (inherits(pag, "conn_pagination_link")) {
    # Try body path
    nu <- body[[pag$next_path]]
    if (is.null(nu)) {
      lh <- tryCatch(httr2::resp_header(resp, "Link"), error = function(e) NULL)
      nu <- parse_link_header(lh)
    }
    if (is.null(nu)) NULL else list(page_num = 0L, cursor = NULL, next_url = nu)
  } else NULL
}

parse_link_header <- function(hdr) {
  if (is.null(hdr)) return(NULL)
  for (part in strsplit(hdr, ",\\s*")[[1]]) {
    if (grepl('rel="next"', part, fixed = TRUE)) {
      m <- regmatches(part, regexpr("<([^>]+)>", part))
      if (length(m)) return(sub("^<(.+)>$", "\\1", m))
    }
  }
  NULL
}

make_r_filter <- function(col, op, value) {
  switch(op,
    "="  = function(df) df[!is.na(df[[col]]) & df[[col]] == value, , drop = FALSE],
    "!=" = function(df) df[!is.na(df[[col]]) & df[[col]] != value, , drop = FALSE],
    ">"  = function(df) df[!is.na(df[[col]]) & df[[col]] >  value, , drop = FALSE],
    ">=" = function(df) df[!is.na(df[[col]]) & df[[col]] >= value, , drop = FALSE],
    "<"  = function(df) df[!is.na(df[[col]]) & df[[col]] <  value, , drop = FALSE],
    "<=" = function(df) df[!is.na(df[[col]]) & df[[col]] <= value, , drop = FALSE],
    "IN" = function(df) df[!is.na(df[[col]]) & df[[col]] %in% value, , drop = FALSE],
    "LIKE" = {
      pat <- gsub("%", ".*", gsub("_", ".", value, fixed = TRUE), fixed = TRUE)
      function(df) df[grepl(pat, df[[col]]), , drop = FALSE]
    },
    identity
  )
}

validate_live_schema_map <- function(schema_map) {
  if (!is.list(schema_map) || is.null(names(schema_map))) {
    cli::cli_abort("{.arg schema_map} must be a named list.")
  }
  for (nm in names(schema_map)) {
    entry <- schema_map[[nm]]
    if (!all(c("resource", "columns") %in% names(entry))) {
      cli::cli_abort(
        "schema_map['{nm}'] must have {.field resource} and {.field columns}."
      )
    }
  }
  invisible(schema_map)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
