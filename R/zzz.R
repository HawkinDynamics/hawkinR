#' @importFrom logger log_appender log_formatter log_layout appender_console formatter_glue_or_sprintf layout_glue_colors
#' @keywords internal

.onLoad <- function(libname, pkgname) {

  # 1. Initialize logger settings -----------------------------------------
  # Using :: ensures we don't need to load the whole package into the search path
  logger::log_appender(logger::appender_console)
  logger::log_formatter(logger::formatter_glue_or_sprintf)
  logger::log_layout(logger::layout_glue_colors)

  # 2. Initialize the internal state container ----------------------------
  # .hawkin_env is defined at the top level of auth_system.R as an environment.
  # We mutate its contents here (reference semantics) instead of rebinding the
  # symbol, so we never write to the global environment (CRAN policy forbids
  # superassignment into .GlobalEnv from package code).
  .hawkin_env$active_conn <- NULL

  logger::log_trace("hawkinR -> Package initialized")
}

.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)

  # ASCII Art Logo
  msg <- paste0(
    "\n",
    "############################################################\n",
    "#                                                          #\n",
    "#   _                          _      _           _____    #\n",
    "#  | |                        | |    (_)         |  __ \\   #\n",
    "#  | |__     __ _  __      __ | | __  _   _ __   | |__) |  #\n",
    "#  | '_ \\   / _` | \\ \\ /\\ / / | |/ / | | | '_ \\  |  _  /   #\n",
    "#  | | | | | (_| |  \\ V  V /  |   <  | | | | | | | | \\ \\   #\n",
    "#  |_| |_|  \\__,_|   \\_/\\_/   |_|\\_\\ |_| |_| |_| |_|  \\_\\  #\n",
    "#                                                          #\n",
    "############################################################\n",
    "\n",
    " v", version, " | Modern Hawkin Dynamics API Client\n",
    " ----------------------------------------------\n",
    " \U0001f512 Credentials:  hd_auth_store()\n",
    " \U0001f310 Connection:   hd_connect()\n",
    " \U0001f4d6 Guides:       browseVignettes('hawkinR')\n",
    " ----------------------------------------------\n"
  )

  packageStartupMessage(msg)
}
