#' @importFrom logger log_appender log_formatter log_layout appender_console formatter_glue_or_sprintf layout_glue_colors
#' @keywords internal

.onLoad <- function(libname, pkgname) {

  # 1. Initialize logger settings -----------------------------------------
  # Using :: ensures we don't need to load the whole package into the search path
  logger::log_appender(logger::appender_console)
  logger::log_formatter(logger::formatter_glue_or_sprintf)
  logger::log_layout(logger::layout_glue_colors)

  # 2. Initialize the internal state container ----------------------------
  # We use the object already defined in the package namespace
  # This makes it accessible to get_active_conn() and set_active_conn()

  env <- new.env(parent = emptyenv())
  env$active_conn <- NULL

  # Update the package-level variable defined in auth_system.R
  # Note: .hawkin_env must be defined at the top level of one of your R files
  .hawkin_env <<- env
}

.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)

  # ASCII Art Logo and Message
  msg <- paste0(
    "\n",
    "  _                 _   _       ____  \n",
    " | |__   __ _ _ __ | |_(_)_ __ |  _ \\ \n",
    " | '_ \\ / _` | '_ \\| __| | '_ \\| |_) |\n",
    " | | | | (_| | | | | |_| | | | |  _ < \n",
    " |_| |_|\\__,_|_| |_|\\__|_|_| |_|_| \\_\\\n",
    "                                      \n",
    " v", version, " | Modern Hawkin Dynamics API Client\n",
    " ----------------------------------------------\n",
    " \U0001f512 Credentials:  hd_auth_store()\n",
    " \U0001f310 Connection:   hd_connect()\n",
    " \U0001f4d6 Guides:       browseVignettes('hawkinR')\n",
    " ----------------------------------------------\n"
  )

  packageStartupMessage(msg)
}
