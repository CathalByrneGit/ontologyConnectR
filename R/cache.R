#' Clear all cached results for a LiveConnection
#'
#' @param conn A \code{LiveConnection} object.
#' @return The connection invisibly.
#' @export
conn_invalidate <- function(conn) {
  if (!is.null(conn@cache)) conn@cache$reset()
  invisible(conn)
}

#' Set the cache TTL on an existing LiveConnection
#'
#' Returns a new connection with an updated cache. The original is unchanged.
#'
#' @param conn A \code{LiveConnection} object.
#' @param ttl_seconds Numeric. Cache TTL in seconds. 0 disables caching.
#' @return A modified \code{LiveConnection}.
#' @export
conn_set_ttl <- function(conn, ttl_seconds) {
  ttl <- as.integer(ttl_seconds)
  new_cache <- if (ttl > 0L) cachem::cache_mem(max_age = ttl) else NULL
  conn@cache <- new_cache
  invisible(conn)
}
