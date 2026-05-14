#' Offset-based pagination
#'
#' @param page_param Query parameter for the page number.
#' @param size_param Query parameter for the page size.
#' @param page_size Integer. Records per page.
#' @return A list of class \code{conn_pagination_offset}.
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
#'
#' @param cursor_param Query parameter for the cursor value.
#' @param cursor_path JSON path in the response body where the next cursor lives.
#' @return A list of class \code{conn_pagination_cursor}.
#' @export
conn_pagination_cursor <- function(cursor_param = "cursor",
                                    cursor_path  = "nextCursor") {
  structure(list(cursor_param = cursor_param, cursor_path = cursor_path),
            class = "conn_pagination_cursor")
}

#' Link-header-based pagination (RFC 5988)
#'
#' Follows the \code{rel="next"} link in the HTTP \code{Link} response header
#' or a JSON body field.
#'
#' @param next_path JSON path in the response body for the next-page URL.
#' @return A list of class \code{conn_pagination_link}.
#' @export
conn_pagination_link <- function(next_path = "links.next") {
  structure(list(next_path = next_path), class = "conn_pagination_link")
}
