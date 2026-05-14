#' @include ontologyConnectR-package.R
NULL

# Base S4 class -----------------------------------------------------------

#' LiveConnection — base class for all live source connections
#'
#' Extends \code{DBIConnection} so that dbplyr treats it as a valid backend.
#' Do not instantiate this class directly; use \code{live_rest()},
#' \code{live_fhir()}, \code{live_jdbc()}, or \code{live_icij()}.
#'
#' @slot source_type Character. One of "rest", "fhir", "jdbc", "icij".
#' @slot schema_map Named list. Maps object type IDs to source schema info.
#'   Each entry: list(resource, pk, columns = list(property_id = source_field)).
#' @slot cache A \code{cachem} cache object, or NULL if caching is disabled.
#' @slot config List of source-specific configuration (base_url, auth, etc.).
#'
#' @export
setClass(
  "LiveConnection",
  contains = "DBIConnection",
  slots = c(
    source_type = "character",
    schema_map  = "list",
    cache       = "ANY",
    config      = "list"
  )
)

# Subclasses --------------------------------------------------------------

#' @rdname LiveConnection
#' @export
setClass("LiveRestConnection",  contains = "LiveConnection")

#' @rdname LiveConnection
#' @export
setClass("LiveFhirConnection",  contains = "LiveConnection")

#' @rdname LiveConnection
#' @export
setClass("LiveJdbcConnection",  contains = "LiveConnection")

#' @rdname LiveConnection
#' @export
setClass("LiveIcijConnection",
  contains = "LiveConnection",
  slots    = c(duckdb_con = "ANY")
)

# Result class ------------------------------------------------------------

#' LiveResult — holds query results in the DBI result-set protocol
#'
#' @slot data data.frame of query results.
#' @slot fetched Logical; TRUE after dbFetch() has been called.
#' @export
setClass(
  "LiveResult",
  contains = "DBIResult",
  slots    = c(data = "data.frame", fetched = "logical")
)

# Driver (required by DBI) -----------------------------------------------

setClass("LiveDriver", contains = "DBIDriver")

#' @export
setMethod("dbGetInfo", "LiveDriver", function(dbObj, ...) {
  list(driver.version = utils::packageVersion("ontologyConnectR"),
       client.version = NA_character_)
})

# Internal constructor helper --------------------------------------------

#' @keywords internal
new_live_connection <- function(Class, source_type, schema_map,
                                 cache_ttl = 0L, config = list(), ...) {
  cache <- if (as.integer(cache_ttl) > 0L) {
    cachem::cache_mem(max_age = as.integer(cache_ttl))
  } else {
    NULL
  }
  new(Class,
      source_type = source_type,
      schema_map  = schema_map,
      cache       = cache,
      config      = config,
      ...)
}
