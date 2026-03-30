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
    "\n",
    " > Documentation: https://hawkindynamics.github.io/hawkinR/\n",
    " > Issues: https://github.com/HawkinDynamics/hawkinR/issues\n"
  )

  packageStartupMessage(msg)
}
