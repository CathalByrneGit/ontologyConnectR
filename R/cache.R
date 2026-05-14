#' Fetch from a connector with TTL caching
#'
#' Internal function. Checks the connector's in-memory cache before calling
#' conn_fetch(). Falls through to conn_fetch() on cache miss or if TTL is 0.
#'
#' @param connector An OntologyConnector object.
#' @param filters A list of filter specs.
#' @param select Character vector or NULL.
#' @param limit Integer or NULL.
#' @param offset Integer.
#' @return A data.frame.
cache_fetch <- function(connector, filters = list(),
                        select = NULL, limit = NULL, offset = 0L) {
  ttl <- connector$cache_ttl %||% 0L

  if (ttl <= 0L) {
    return(conn_fetch(connector, filters = filters, select = select,
                      limit = limit, offset = offset))
  }

  cache <- connector$.cache
  if (is.null(cache)) {
    return(conn_fetch(connector, filters = filters, select = select,
                      limit = limit, offset = offset))
  }

  key <- make_cache_key(connector, filters, select, limit, offset)
  cached <- cache$get(key)

  if (!cachem::is.key_missing(cached)) {
    return(cached)
  }

  result <- conn_fetch(connector, filters = filters, select = select,
                       limit = limit, offset = offset)
  cache$set(key, result)
  result
}

make_cache_key <- function(connector, filters, select, limit, offset) {
  id_part <- connector$connector_id %||% class(connector)[1]
  digest::digest(list(id_part, filters, select, limit, offset),
                 algo = "md5")
}

# Fallback if digest is not available: use paste/toString
make_cache_key <- function(connector, filters, select, limit, offset) {
  id_part  <- connector$connector_id %||% class(connector)[1]
  f_str    <- paste(sapply(filters, function(f)
    paste(f$property, f$op, paste(f$value, collapse = "|"), sep = ":")),
    collapse = ";")
  s_str    <- paste(select,  collapse = ",")
  key_raw  <- paste(id_part, f_str, s_str, limit %||% "NULL", offset, sep = "|")
  # Simple hash to keep key short
  as.character(sum(utf8ToInt(substr(key_raw, 1, 255))) %% 1e9)
}

#' Clear all cached results for a connector
#'
#' @param connector An OntologyConnector object.
#' @return The connector with an empty cache (invisibly).
#' @export
conn_invalidate <- function(connector) {
  cache <- connector$.cache
  if (!is.null(cache)) cache$reset()
  invisible(connector)
}

#' Set the cache TTL on an existing connector
#'
#' @param connector An OntologyConnector object.
#' @param ttl_seconds Numeric. Seconds to cache results. 0 disables caching.
#' @return The modified connector (invisibly).
#' @export
conn_set_ttl <- function(connector, ttl_seconds) {
  ttl <- as.integer(ttl_seconds)
  connector$cache_ttl <- ttl
  if (ttl > 0L && is.null(connector$.cache)) {
    connector$.cache <- cachem::cache_mem(max_age = ttl)
  } else if (ttl <= 0L) {
    connector$.cache <- NULL
  } else {
    # Re-create cache with new TTL
    connector$.cache <- cachem::cache_mem(max_age = ttl)
  }
  invisible(connector)
}

#' Initialise a cache inside a connector list
#' @param connector_list mutable list being constructed for a connector
#' @param ttl integer TTL in seconds
#' @return connector_list with $.cache set if ttl > 0
init_cache <- function(connector_list, ttl) {
  connector_list$cache_ttl <- as.integer(ttl)
  if (as.integer(ttl) > 0L) {
    connector_list$.cache <- cachem::cache_mem(max_age = as.integer(ttl))
  } else {
    connector_list$.cache <- NULL
  }
  connector_list
}

`%||%` <- function(a, b) if (is.null(a)) b else a
