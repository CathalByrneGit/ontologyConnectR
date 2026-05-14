#' Create a JDBC connector
#'
#' Connects to JDBC-accessible databases (SAP, Teradata, Oracle, etc.) using
#' RJDBC. Builds parameterised SELECT queries from filter specs and returns
#' data.frames. Requires the `RJDBC` package.
#'
#' @param driver_class Java class name of the JDBC driver,
#'   e.g. "com.teradata.jdbc.TeraDriver".
#' @param jdbc_url JDBC connection URL.
#' @param table_name Database table or view name.
#' @param schema_map Named list mapping property IDs to column names.
#' @param credentials List with `username` and `password_env_var`.
#' @param driver_path Path to the JDBC driver JAR, or NULL if already on
#'   the Java classpath.
#' @return A list of class `conn_jdbc`.
#' @export
conn_jdbc <- function(driver_class,
                       jdbc_url,
                       table_name,
                       schema_map,
                       credentials = list(username = NULL, password_env_var = NULL),
                       driver_path = NULL) {
  validate_schema_map(schema_map)
  conn <- list(
    driver_class  = driver_class,
    jdbc_url      = jdbc_url,
    table_name    = table_name,
    schema_map    = schema_map,
    credentials   = credentials,
    driver_path   = driver_path,
    pk_property   = names(schema_map)[1],
    connector_id  = paste0("jdbc:", jdbc_url, ":", table_name)
  )
  class(conn) <- c("conn_jdbc", "OntologyConnector")
  conn
}

#' @export
conn_supports_filter.conn_jdbc <- function(connector, op) {
  op %in% c("eq", "neq", "gt", "gte", "lt", "lte", "in", "not_in",
             "is_null", "not_null", "contains", "starts_with", "ends_with")
}

#' @export
conn_fetch.conn_jdbc <- function(connector, filters = list(),
                                  select = NULL, limit = NULL, offset = 0L) {
  if (!requireNamespace("RJDBC", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg RJDBC} is required for JDBC connections. Install it with {.code install.packages('RJDBC')}."
    )
  }

  con <- open_jdbc_connection(connector)
  on.exit(try(RJDBC::dbDisconnect(con), silent = TRUE), add = TRUE)

  sql <- build_jdbc_query(connector, filters, select, limit, offset)
  result <- RJDBC::dbGetQuery(con, sql)

  # Rename columns from DB names to property IDs
  inv_map <- stats::setNames(names(connector$schema_map),
                              unname(unlist(connector$schema_map)))
  names(result) <- sapply(names(result), function(col) {
    inv_map[col] %||% col
  })
  result
}

#' @export
conn_count.conn_jdbc <- function(connector, filters = list()) {
  if (!requireNamespace("RJDBC", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg RJDBC} is required.")
  }
  con <- open_jdbc_connection(connector)
  on.exit(try(RJDBC::dbDisconnect(con), silent = TRUE), add = TRUE)

  where_clause <- build_where_clause(connector, filters)
  sql <- paste0("SELECT COUNT(*) AS n FROM ", quote_identifier(connector$table_name),
                if (nchar(where_clause) > 0) paste0(" WHERE ", where_clause) else "")
  result <- RJDBC::dbGetQuery(con, sql)
  as.integer(result[[1]][1])
}

# JDBC internals ----------------------------------------------------------

open_jdbc_connection <- function(connector) {
  drv <- RJDBC::JDBC(connector$driver_class, connector$driver_path)
  password <- if (!is.null(connector$credentials$password_env_var)) {
    Sys.getenv(connector$credentials$password_env_var, unset = NA)
  } else {
    NA
  }
  if (!is.na(password)) {
    RJDBC::dbConnect(drv, connector$jdbc_url,
                     connector$credentials$username, password)
  } else {
    RJDBC::dbConnect(drv, connector$jdbc_url)
  }
}

build_jdbc_query <- function(connector, filters, select, limit, offset) {
  col_clause <- if (!is.null(select) && length(select) > 0) {
    db_cols <- sapply(select, function(prop) {
      connector$schema_map[[prop]] %||% prop
    })
    paste(sapply(db_cols, quote_identifier), collapse = ", ")
  } else {
    "*"
  }

  where_clause <- build_where_clause(connector, filters)

  sql <- paste0(
    "SELECT ", col_clause,
    " FROM ", quote_identifier(connector$table_name)
  )
  if (nchar(where_clause) > 0) sql <- paste0(sql, " WHERE ", where_clause)
  if (!is.null(limit))         sql <- paste0(sql, " FETCH FIRST ", limit + offset, " ROWS ONLY")
  sql
}

build_where_clause <- function(connector, filters) {
  if (length(filters) == 0L) return("")
  clauses <- sapply(filters, function(f) {
    col <- quote_identifier(connector$schema_map[[f$property]] %||% f$property)
    filter_to_sql(col, f$op, f$value)
  })
  paste(clauses, collapse = " AND ")
}

filter_to_sql <- function(col, op, value) {
  switch(op,
    eq          = paste0(col, " = ",  sql_literal(value)),
    neq         = paste0(col, " <> ", sql_literal(value)),
    gt          = paste0(col, " > ",  sql_literal(value)),
    gte         = paste0(col, " >= ", sql_literal(value)),
    lt          = paste0(col, " < ",  sql_literal(value)),
    lte         = paste0(col, " <= ", sql_literal(value)),
    `in`        = paste0(col, " IN (", paste(sapply(value, sql_literal), collapse = ", "), ")"),
    not_in      = paste0(col, " NOT IN (", paste(sapply(value, sql_literal), collapse = ", "), ")"),
    is_null     = paste0(col, " IS NULL"),
    not_null    = paste0(col, " IS NOT NULL"),
    contains    = paste0(col, " LIKE ", sql_literal(paste0("%", value, "%"))),
    starts_with = paste0(col, " LIKE ", sql_literal(paste0(value, "%"))),
    ends_with   = paste0(col, " LIKE ", sql_literal(paste0("%", value))),
    cli::cli_abort("Unknown filter op: {.val {op}}")
  )
}

sql_literal <- function(value) {
  if (is.numeric(value)) return(as.character(value))
  # Escape single quotes
  paste0("'", gsub("'", "''", as.character(value)), "'")
}

quote_identifier <- function(id) {
  paste0('"', gsub('"', '""', id), '"')
}

`%||%` <- function(a, b) if (is.null(a)) b else a
