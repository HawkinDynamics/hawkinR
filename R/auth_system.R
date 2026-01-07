#' @include utils.R
NULL

#' @title Hawkin Dynamics Authentication System
#' @description
#' Defines the S7 classes and methods for managing authentication state,
#' secure credential storage, and session configuration.
#' @name auth_system
NULL

# 1. Internal Package State -----------------------------------------------

#' Internal State Container
#' @description
#' This object is initialized in zzz.R during .onLoad().
#' It holds the active HawkinAuth connection.
#' @keywords internal
.hawkin_env <- NULL

#' Set the Active Hawkin Connection
#' @description Internal function to set the global connection pointer.
#' @param auth_object A `HawkinAuth` S7 object.
#' @noRd
set_active_conn <- function(auth_object) {
  # Use S7::is_instance for more robust class checking
  if (!inherits(auth_object, HawkinAuth)) {
    stop("Object must be a HawkinAuth connection", call. = FALSE)
  }
  .hawkin_env$active_conn <- auth_object
  logger::log_trace("hawkinR/auth -> Active connection updated")
}

#' Get the Active Hawkin Connection
#' @description Internal function to retrieve the global connection pointer.
#' @return The active `HawkinAuth` object.
#' @noRd
get_active_conn <- function() {
  if (is.null(.hawkin_env$active_conn)) {
    stop("No active connection found. Please run `hd_connect()` first.", call. = FALSE)
  }
  return(.hawkin_env$active_conn)
}

# 2. S7 Class Definitions -------------------------------------------------

#' Hawkin Configuration Class
#'
#' @description
#' Stores environment and profile configuration settings.
#'
#' @param profile character. The profile name used for credential lookup (default: "default").
#' @param org_id character. The organization ID for API paths (default: "v1").
#' @param environment character. "development" (keyring) or "production" (env vars).
#' @param log_level character. Logging verbosity ("INFO", "DEBUG", "WARN").
#' @export
HawkinConfig <- S7::new_class("HawkinConfig",
                              properties = list(
                                profile     = S7::new_property(S7::class_character, default = "default"),
                                org_id      = S7::new_property(S7::class_character, default = "v1"),
                                environment = S7::new_property(S7::class_character, default = "development"),
                                log_level   = S7::new_property(S7::class_character, default = "INFO")
                              ),
                              validator = function(self) {
                                if (!self@environment %in% c("development", "production")) {
                                  "@environment must be either 'development' or 'production'"
                                }
                              }
)

#' Hawkin Authentication Class
#'
#' @description
#' Manages the API session state, including the access token lifecycle and regional endpoints.
#'
#' @param config HawkinConfig. The configuration settings.
#' @param access_token character. The ephemeral Bearer token.
#' @param expires_at POSIXct. The timestamp when the access token expires.
#' @param region character. The data region ("Americas", "Europe", "APAC").
#'
#' @section Properties:
#' \itemize{
#'   \item \code{base_url}: The computed API base URL for the region (read-only).
#' }
#' @export
HawkinAuth <- S7::new_class("HawkinAuth",
                            properties = list(
                              config       = HawkinConfig,
                              access_token = S7::new_property(S7::class_any, default = NULL),
                              expires_at   = S7::new_property(S7::class_any, default = NULL),
                              region       = S7::new_property(S7::class_character, default = "Americas"),
                              base_url     = S7::new_property(
                                getter = function(self) {
                                  switch(self@region,
                                         "Americas"     = "https://cloud.hawkindynamics.com/api",
                                         "Europe"       = "https://eu.cloud.hawkindynamics.com/api",
                                         "Asia/Pacific" = "https://apac.cloud.hawkindynamics.com/api",
                                         "APAC"         = "https://apac.cloud.hawkindynamics.com/api",
                                         stop("Invalid region provided. Options: Americas, Europe, APAC", call. = FALSE)
                                  )
                                }
                              )
                            ),
                            validator = function(self) {
                              if (!is.null(self@expires_at) && !inherits(self@expires_at, "POSIXct")) {
                                return("@expires_at must be a POSIXct object or NULL")
                              }
                            }
)

# 3. Authentication Logic -------------------------------------------------

#' Authenticate / Refresh Token
#'
#' @description
#' Internal S7 generic to exchange a Refresh Token for a new Access Token.
#' @param x A `HawkinAuth` object.
#' @return An updated `HawkinAuth` object with a valid access token.
#' @keywords internal
authenticate <- S7::new_generic("authenticate", "x")

S7::method(authenticate, HawkinAuth) <- function(x) {
  logger::log_trace("hawkinR -> Authenticating profile: {x@config@profile}")
  logger::log_debug("hawkinR/auth -> Environment: {x@config@environment}, Region: {x@region}")

  refresh_secret <- NULL
  profile_name   <- x@config@profile

  # Strategy: Determine where to find the secret
  if (x@config@environment == "development") {
    # Method A: Keyring (Local)
    refresh_secret <- tryCatch(
      keyring::key_get(service = "hawkinR", username = profile_name),
      error = function(e) NULL
    )

    if (is.null(refresh_secret)) {
      msg <- sprintf("No credentials found for profile '%s'. Run hd_auth_store('%s') first.",
                     profile_name, profile_name)
      logger::log_error(msg)
      stop(msg, call. = FALSE)
    }

  } else {
    # Method B: Environment Variables (Production)
    env_var_name <- paste0("HAWKIN_KEY_", toupper(profile_name))
    refresh_secret <- Sys.getenv(env_var_name)

    if (refresh_secret == "") {
      msg <- sprintf("Environment variable '%s' is missing for production auth.", env_var_name)
      logger::log_error(msg)
      stop(msg, call. = FALSE)
    }
  }

  # Perform Exchange
  req <- httr2::request(paste0(x@base_url, "/token")) |>
    httr2::req_headers(Authorization = paste("Bearer", refresh_secret)) |>
    httr2::req_method("GET") |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status != 200) {
    logger::log_error("hawkinR -> Auth failed with status {status}")
    stop("Authentication failed. Check your Refresh Token or Region.", call. = FALSE)
  }

  body <- httr2::resp_body_json(resp)

  # Update Object State
  x@access_token <- body$access_token
  x@expires_at   <- as.POSIXct(body$expires_at, origin = "1970-01-01")

  logger::log_debug("hawkinR/auth -> Token valid until: {x@expires_at}")
  logger::log_success("hawkinR -> Successfully authenticated as '{profile_name}'")
  return(x)
}

# 4. User-Facing Functions ------------------------------------------------

#' Store Hawkin Credentials Securely
#'
#' @description
#' Securely saves your Hawkin Dynamics Refresh Token to your operating system's
#' credential store (Keychain on macOS, Credential Manager on Windows).
#'
#' @param profile character. A name for this set of credentials (default: "default").
#' @param token character. Optional. If NULL (default), a secure prompt will appear.
#' @return NULL
#' @export
hd_auth_store <- function(profile = "default", token = NULL) {
  check_interactive()

  tryCatch({
    if (is.null(token)) {
      # This triggers the OS popup
      keyring::key_set(service = "hawkinR", username = profile, prompt = "API Integration Key: ")
    } else {
      keyring::key_set_with_value(service = "hawkinR", username = profile, password = token)
    }
    logger::log_success("Success: Credentials for profile '{profile}' stored safely.")
  }, error = function(e) {
    # Check if the error was a user cancellation
    message("\nError: Registration cancelled or could not find credentials for ", profile)
    return(NULL)
  })
}

#' Remove Hawkin Credentials
#'
#' @description
#' Deletes a stored Refresh Token from the system keychain.
#' @param profile character. The name of the profile to remove.
#' @return NULL
#' @export
hd_auth_reset <- function(profile = "default") {
  tryCatch({
    keyring::key_delete(service = "hawkinR", username = profile)
    logger::log_success("Credentials for profile '{profile}' removed.")
  }, error = function(e) {
    logger::log_warn("No credentials found for profile '{profile}' to remove.")
  })
}

#' Connect to Hawkin Dynamics API
#'
#' @description
#' Initializes a connection to the Hawkin Dynamics Cloud. This function creates
#' the session object, performs the initial authentication, and sets it as the
#' active connection for subsequent data calls.
#'
#' @param profile character. The name of the stored profile credential to use (default: "default").
#' @param org_id character. Your Organization ID. Defaults to "v1" for standard users.
#' @param environment character. "development" to use local keychain, or "production" to use Environment Variables.
#' @param region character. The API region: "Americas" (default), "Europe", or "APAC".
#' @param log_level character. Logging verbosity: "INFO", "DEBUG", or "WARN".
#'
#' @return Invisibly returns the authenticated `HawkinAuth` object.
#' @export
hd_connect <- function(profile = "default",
                       org_id = "v1",
                       environment = "development",
                       region = "Americas",
                       log_level = "INFO") {

  # Initialize Logger
  logger::log_threshold(log_level)

  logger::log_info("hawkinR -> Connecting to {region} region with profile '{profile}'")

  # Create Classes
  cfg  <- HawkinConfig(profile = profile,
                       org_id = org_id,
                       environment = environment,
                       log_level = log_level)

  auth <- HawkinAuth(config = cfg, region = region)

  # Authenticate
  auth <- authenticate(auth)

  # Set Global State
  set_active_conn(auth)

  logger::log_info("hawkinR -> Connection established. Token expires: {auth@expires_at}")

  invisible(auth)
}
