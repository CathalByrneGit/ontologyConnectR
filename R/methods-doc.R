#' @include live_connection.R execute.R dbi_methods.R
NULL

#' DBI and dispatch methods for live connections
#'
#' \code{LiveConnection} implements the DBI generics dbplyr needs, so a live
#' connection can be used with \code{dplyr::tbl()} like any DBI connection.
#' \code{dbGetQuery()} / \code{dbSendQuery()} intercept the SQL dbplyr
#' generates and route it to \code{dispatch_query()}, which each backend
#' (\code{LiveRestConnection}, \code{LiveFhirConnection},
#' \code{LiveJdbcConnection}, \code{LiveIcijConnection}) implements.
#' \code{LiveResult} holds the fetched data frame.
#'
#' @name LiveConnection-methods
#' @keywords internal
#' @aliases dbClearResult,LiveResult-method
#' @aliases dbColumnInfo,LiveResult-method
#' @aliases dbDataType,LiveConnection-method
#' @aliases dbDisconnect,LiveConnection-method
#' @aliases dbDisconnect,LiveIcijConnection-method
#' @aliases dbExistsTable,LiveConnection,ANY-method
#' @aliases dbFetch,LiveResult-method
#' @aliases dbGetInfo,LiveConnection-method
#' @aliases dbGetInfo,LiveDriver-method
#' @aliases dbGetQuery,LiveConnection,ANY-method
#' @aliases dbHasCompleted,LiveResult-method
#' @aliases dbIsValid,LiveConnection-method
#' @aliases dbListTables,LiveConnection-method
#' @aliases dbQuoteIdentifier,LiveConnection,character-method
#' @aliases dbQuoteString,LiveConnection,character-method
#' @aliases dbSendQuery,LiveConnection-method
#' @aliases dispatch_query,LiveFhirConnection-method
#' @aliases dispatch_query,LiveIcijConnection-method
#' @aliases dispatch_query,LiveJdbcConnection-method
#' @aliases dispatch_query,LiveRestConnection-method
#' @aliases show,LiveConnection-method
#' @aliases show,LiveIcijConnection-method
NULL
