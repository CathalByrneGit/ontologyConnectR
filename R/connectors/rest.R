# Auth helper constructors ------------------------------------------------

#' Bearer token authentication
#' @param token Static token string, or NULL.
#' @param env_var Name of environment variable holding the token, or NULL.
#' @return A list of class `conn_auth_bearer`.
#' @export
conn_auth_bearer <- function(token = NULL, env_var = NULL) {
  structure(list(token = token, env_var = env_var), class = "conn_auth_bearer")
}

#' Basic (username + password) authentication
#' @param username Character.
#' @param password_env_var Name of env var holding the password.
#' @return A list of class `conn_auth_basic`.
#' @export
conn_auth_basic <- function(username, password_env_var) {
  structure(list(username = username, password_env_var = password_env_var),
            class = "conn_auth_basic")
}

#' API key authentication
#' @param header HTTP header name. Default "X-API-Key".
#' @param key_env_var Name of env var holding the API key.
#' @return A list of class `conn_auth_api_key`.
#' @export
conn_auth_api_key <- function(header = "X-API-Key", key_env_var) {
  structure(list(header = header, key_env_var = key_env_var),
            class = "conn_auth_api_key")
}

#' OAuth2 client credentials authentication
#' @param token_url Token endpoint URL.
#' @param client_id Client ID.
#' @param client_secret_env_var Name of env var holding the client secret.
#' @param scope OAuth2 scope string, or NULL.
#' @return A list of class `conn_auth_oauth2`.
#' @export
conn_auth_oauth2 <- function(token_url, client_id, client_secret_env_var,
                              scope = NULL) {
  structure(
    list(
      token_url             = token_url,
      client_id             = client_id,
      client_secret_env_var = client_secret_env_var,
      scope                 = scope,
      .token_cache          = NULL,
      .token_expiry         = NULL
    ),
    class = "conn_auth_oauth2"
  )
}

# Apply auth to an httr2 request
apply_auth <- function(req, auth) {
  if (is.null(auth)) return(req)
  UseMethod("apply_auth", auth)
}

apply_auth.conn_auth_bearer <- function(req, auth) {
  token <- auth$token %||% Sys.getenv(auth$env_var %||% "", unset = NA)
  if (is.na(token) || nchar(token) == 0) {
    cli::cli_abort("Bearer token not found. Set {.envvar {auth$env_var}}.")
  }
  httr2::req_auth_bearer_token(req, token)
}

apply_auth.conn_auth_basic <- function(req, auth) {
  password <- Sys.getenv(auth$password_env_var, unset = NA)
  if (is.na(password)) {
    cli::cli_abort("Password env var {.envvar {auth$password_env_var}} not set.")
  }
  httr2::req_auth_basic(req, auth$username, password)
}

apply_auth.conn_auth_api_key <- function(req, auth) {
  key <- Sys.getenv(auth$key_env_var, unset = NA)
  if (is.na(key)) {
    cli::cli_abort("API key env var {.envvar {auth$key_env_var}} not set.")
  }
  httr2::req_headers(req, .headers = stats::setNames(list(key), auth$header))
}

apply_auth.conn_auth_oauth2 <- function(req, auth) {
  token <- get_oauth2_token(auth)
  httr2::req_auth_bearer_token(req, token)
}

get_oauth2_token <- function(auth) {
  now <- as.numeric(Sys.time())
  if (!is.null(auth$.token_cache) &&
      !is.null(auth$.token_expiry) &&
      now < auth$.token_expiry - 30) {
    return(auth$.token_cache)
  }
  secret <- Sys.getenv(auth$client_secret_env_var, unset = NA)
  if (is.na(secret)) {
    cli::cli_abort("OAuth2 secret env var {.envvar {auth$client_secret_env_var}} not set.")
  }
  body <- list(
    grant_type    = "client_credentials",
    client_id     = auth$client_id,
    client_secret = secret
  )
  if (!is.null(auth$scope)) body$scope <- auth$scope

  resp <- httr2::request(auth$token_url) |>
    httr2::req_body_form(!!!body) |>
    httr2::req_perform()
  parsed <- httr2::resp_body_json(resp)
  token  <- parsed$access_token
  expires_in <- parsed$expires_in %||% 3600
  auth$.token_cache  <- token
  auth$.token_expiry <- now + expires_in
  token
}

# Pagination helpers -------------------------------------------------------

#' Offset-based pagination
#' @export
conn_pagination_offset <- function(page_param = "page", size_param = "size",
                                    page_size = 100L) {
  structure(
    list(page_param = page_param, size_param = size_param,
         page_size = as.integer(page_size)),
    class = "conn_pagination_offset"
  )
}

#' Cursor-based pagination
#' @export
conn_pagination_cursor <- function(cursor_param = "cursor",
                                    cursor_path = "nextCursor") {
  structure(list(cursor_param = cursor_param, cursor_path = cursor_path),
            class = "conn_pagination_cursor")
}

#' Link-header-based pagination (RFC 5988)
#' @export
conn_pagination_link <- function(next_path = "links.next") {
  structure(list(next_path = next_path), class = "conn_pagination_link")
}

# REST connector -----------------------------------------------------------

#' Create a REST API connector
#'
#' @param base_url Base URL of the REST API (no trailing slash).
#' @param list_path Path for listing resources, e.g. "/patients".
#' @param get_path Path template for a single resource, e.g. "/patients/{pk}".
#' @param count_path Path for counting resources, or NULL.
#' @param schema_map Named list mapping property IDs to JSON paths.
#' @param pk_property Property ID that serves as the primary key.
#' @param auth Authentication object (conn_auth_*), or NULL.
#' @param pagination Pagination object (conn_pagination_*), or NULL.
#' @param cache_ttl Integer. Cache TTL in seconds; 0 = no cache.
#' @return A list of class `conn_rest`.
#' @export
conn_rest <- function(base_url,
                       list_path,
                       get_path,
                       count_path  = NULL,
                       schema_map,
                       pk_property,
                       auth        = NULL,
                       pagination  = NULL,
                       cache_ttl   = 0L) {
  validate_schema_map(schema_map)
  conn <- list(
    base_url      = sub("/$", "", base_url),
    list_path     = list_path,
    get_path      = get_path,
    count_path    = count_path,
    schema_map    = schema_map,
    pk_property   = pk_property,
    auth          = auth,
    pagination    = pagination,
    connector_id  = paste0("rest:", base_url, list_path)
  )
  conn <- init_cache(conn, cache_ttl)
  class(conn) <- c("conn_rest", "OntologyConnector")
  conn
}

#' @export
conn_supports_filter.conn_rest <- function(connector, op) {
  # REST APIs typically support equality filters as query params
  op %in% c("eq")
}

#' @export
conn_fetch.conn_rest <- function(connector, filters = list(),
                                  select = NULL, limit = NULL, offset = 0L) {
  # Split filters: pushable vs R-side
  pushable <- Filter(function(f) conn_supports_filter(connector, f$op), filters)
  r_side   <- Filter(function(f) !conn_supports_filter(connector, f$op), filters)

  # Build base query params from pushable filters
  query_params <- lapply(pushable, function(f) {
    field <- connector$schema_map[[f$property]] %||% f$property
    stats::setNames(list(as.character(f$value)), field)
  })
  query_params <- do.call(c, query_params)

  all_rows <- list()
  pagination <- connector$pagination
  page_num   <- 0L
  cursor     <- NULL
  next_url   <- NULL
  total_fetched <- 0L

  repeat {
    req <- httr2::request(paste0(connector$base_url, connector$list_path))

    # Add query params
    if (length(query_params) > 0) {
      req <- do.call(httr2::req_url_query, c(list(req), query_params))
    }

    # Add pagination params
    if (!is.null(pagination)) {
      req <- add_pagination_params(req, pagination, page_num, cursor, next_url)
    }

    # Add auth
    req <- apply_auth(req, connector$auth)

    # Execute
    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    # Extract records array (try common wrapper keys first)
    records <- extract_records_from_body(body, connector)

    if (length(records) == 0L) break

    all_rows <- c(all_rows, records)
    total_fetched <- total_fetched + length(records)

    # Check if we've hit the limit
    if (!is.null(limit) && total_fetched >= limit + offset) break

    # Determine if there are more pages
    if (is.null(pagination)) break
    next_info <- get_next_page_info(body, resp, pagination, page_num, cursor)
    if (is.null(next_info$continue)) break
    page_num <- next_info$page_num
    cursor   <- next_info$cursor
    next_url <- next_info$next_url
  }

  df <- apply_schema_map(all_rows, connector$schema_map)
  df <- translate_filters_to_r(r_side)(df)
  df <- apply_limit(df, limit, offset)
  df
}

extract_records_from_body <- function(body, connector) {
  if (is.list(body) && !is.data.frame(body)) {
    # Try common wrapper keys
    for (key in c("data", "results", "items", "records", "content", "value")) {
      if (!is.null(body[[key]]) && is.list(body[[key]])) {
        return(body[[key]])
      }
    }
    # If body is itself an array-like list of lists, use directly
    if (length(body) > 0 && is.list(body[[1]])) {
      return(body)
    }
    return(list())
  }
  list()
}

add_pagination_params <- function(req, pagination, page_num, cursor, next_url) {
  if (inherits(pagination, "conn_pagination_offset")) {
    req <- httr2::req_url_query(
      req,
      !!pagination$page_param := page_num,
      !!pagination$size_param  := pagination$page_size
    )
  } else if (inherits(pagination, "conn_pagination_cursor")) {
    if (!is.null(cursor)) {
      req <- httr2::req_url_query(req, !!pagination$cursor_param := cursor)
    }
  } else if (inherits(pagination, "conn_pagination_link")) {
    if (!is.null(next_url)) {
      req <- httr2::req_url(req, next_url)
    }
  }
  req
}

get_next_page_info <- function(body, resp, pagination, page_num, cursor) {
  if (inherits(pagination, "conn_pagination_offset")) {
    list(continue = TRUE, page_num = page_num + 1L, cursor = NULL, next_url = NULL)
  } else if (inherits(pagination, "conn_pagination_cursor")) {
    next_cursor <- extract_json_path(body, pagination$cursor_path)
    if (is.na(next_cursor) || is.null(next_cursor)) {
      list(continue = NULL)
    } else {
      list(continue = TRUE, page_num = 0L, cursor = next_cursor, next_url = NULL)
    }
  } else if (inherits(pagination, "conn_pagination_link")) {
    # Try body first
    next_url <- extract_json_path(body, pagination$next_path)
    if (is.na(next_url) || is.null(next_url)) {
      # Try Link header
      link_header <- tryCatch(httr2::resp_header(resp, "Link"), error = function(e) NULL)
      next_url <- parse_link_header(link_header)
    }
    if (is.null(next_url) || is.na(next_url)) {
      list(continue = NULL)
    } else {
      list(continue = TRUE, page_num = 0L, cursor = NULL, next_url = next_url)
    }
  } else {
    list(continue = NULL)
  }
}

parse_link_header <- function(header) {
  if (is.null(header)) return(NULL)
  parts <- strsplit(header, ",\\s*")[[1]]
  for (part in parts) {
    if (grepl('rel="next"', part)) {
      m <- regmatches(part, regexpr("<([^>]+)>", part))
      if (length(m) > 0) return(sub("^<(.+)>$", "\\1", m))
    }
  }
  NULL
}

#' @export
conn_count.conn_rest <- function(connector, filters = list()) {
  if (!is.null(connector$count_path)) {
    req <- apply_auth(
      httr2::request(paste0(connector$base_url, connector$count_path)),
      connector$auth
    )
    resp   <- httr2::req_perform(req)
    body   <- httr2::resp_body_json(resp)
    count  <- body$count %||% body$total %||% body$size %||% body
    return(as.integer(count))
  }
  nrow(conn_fetch(connector, filters = filters))
}

#' @export
conn_get_one.conn_rest <- function(connector, pk_value) {
  path <- sub("\\{pk\\}", pk_value, connector$get_path)
  req  <- apply_auth(
    httr2::request(paste0(connector$base_url, path)),
    connector$auth
  )
  resp <- tryCatch(
    httr2::req_perform(req),
    httr2_http_404 = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  body    <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  records <- if (is.list(body) && is.list(body[[1]])) body else list(body)
  apply_schema_map(records, connector$schema_map)
}
