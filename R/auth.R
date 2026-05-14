# Authentication helpers --------------------------------------------------

#' Bearer token authentication
#'
#' @param token Static token string, or NULL.
#' @param env_var Name of environment variable holding the token.
#' @return A list of class \code{conn_auth_bearer}.
#' @export
conn_auth_bearer <- function(token = NULL, env_var = NULL) {
  structure(list(token = token, env_var = env_var), class = "conn_auth_bearer")
}

#' Basic (username + password) authentication
#' @param username Character.
#' @param password_env_var Name of env var holding the password.
#' @return A list of class \code{conn_auth_basic}.
#' @export
conn_auth_basic <- function(username, password_env_var) {
  structure(list(username = username, password_env_var = password_env_var),
            class = "conn_auth_basic")
}

#' API key header authentication
#' @param header HTTP header name. Default \code{"X-API-Key"}.
#' @param key_env_var Name of env var holding the API key.
#' @return A list of class \code{conn_auth_api_key}.
#' @export
conn_auth_api_key <- function(header = "X-API-Key", key_env_var) {
  structure(list(header = header, key_env_var = key_env_var),
            class = "conn_auth_api_key")
}

#' OAuth2 client credentials authentication
#'
#' Tokens are cached in the auth object and refreshed automatically when
#' within 30 seconds of expiry.
#'
#' @param token_url Token endpoint URL.
#' @param client_id Client ID.
#' @param client_secret_env_var Name of env var holding the client secret.
#' @param scope OAuth2 scope string, or NULL.
#' @return A list of class \code{conn_auth_oauth2}.
#' @export
conn_auth_oauth2 <- function(token_url, client_id, client_secret_env_var,
                              scope = NULL) {
  env <- new.env(parent = emptyenv())
  env$token  <- NULL
  env$expiry <- NULL
  structure(
    list(
      token_url             = token_url,
      client_id             = client_id,
      client_secret_env_var = client_secret_env_var,
      scope                 = scope,
      .state                = env
    ),
    class = "conn_auth_oauth2"
  )
}

# apply_auth S3 dispatch -------------------------------------------------

apply_auth <- function(req, auth) {
  if (is.null(auth)) return(req)
  UseMethod("apply_auth", auth)
}

apply_auth.conn_auth_bearer <- function(req, auth) {
  token <- auth$token %||% {
    t <- Sys.getenv(auth$env_var %||% "", unset = NA_character_)
    if (is.na(t) || !nchar(t)) {
      cli::cli_abort("Bearer token not found. Set {.envvar {auth$env_var}}.")
    }
    t
  }
  httr2::req_auth_bearer_token(req, token)
}

apply_auth.conn_auth_basic <- function(req, auth) {
  pw <- Sys.getenv(auth$password_env_var, unset = NA_character_)
  if (is.na(pw)) cli::cli_abort("Password env var {.envvar {auth$password_env_var}} not set.")
  httr2::req_auth_basic(req, auth$username, pw)
}

apply_auth.conn_auth_api_key <- function(req, auth) {
  key <- Sys.getenv(auth$key_env_var, unset = NA_character_)
  if (is.na(key)) cli::cli_abort("API key env var {.envvar {auth$key_env_var}} not set.")
  httr2::req_headers(req, .headers = stats::setNames(list(key), auth$header))
}

apply_auth.conn_auth_oauth2 <- function(req, auth) {
  httr2::req_auth_bearer_token(req, get_oauth2_token(auth))
}

get_oauth2_token <- function(auth) {
  now <- as.numeric(Sys.time())
  st  <- auth$.state
  if (!is.null(st$token) && !is.null(st$expiry) && now < st$expiry - 30) {
    return(st$token)
  }
  secret <- Sys.getenv(auth$client_secret_env_var, unset = NA_character_)
  if (is.na(secret)) {
    cli::cli_abort("OAuth2 secret {.envvar {auth$client_secret_env_var}} not set.")
  }
  body <- list(grant_type = "client_credentials",
               client_id  = auth$client_id, client_secret = secret)
  if (!is.null(auth$scope)) body$scope <- auth$scope

  resp   <- httr2::request(auth$token_url) |>
    httr2::req_body_form(!!!body) |>
    httr2::req_perform()
  parsed <- httr2::resp_body_json(resp)
  st$token  <- parsed$access_token
  st$expiry <- now + (parsed$expires_in %||% 3600L)
  st$token
}

`%||%` <- function(a, b) if (is.null(a)) b else a
