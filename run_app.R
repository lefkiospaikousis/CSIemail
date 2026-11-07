# ============================================================================
# Production launcher for the CSIemail Shiny app (golem package)
# ============================================================================
# Started by NSSM as a Windows service - point NSSM straight at Rscript.exe
# with this file as the argument (no .bat file). See
# docs/handover/06-deployment.md.
#
#   Rscript run_app.R          # port from APP_PORT, else 3846
#   Rscript run_app.R 3850     # explicit port (overrides APP_PORT)
#
# Not to be confused with R/run_app.R, which defines the package's run_app()
# function; this script loads the package and calls it.
#
# Location-independent: finds its own folder, so it works wherever the
# project is cloned. Listens on all network interfaces (0.0.0.0), so other
# machines on the LAN can open http://<server-name>:<port>/.
# Config profile: GOLEM_CONFIG_ACTIVE if set (e.g. by NSSM), else "production".
# Logs (appended, so crash history survives restarts): logs/app.log
# ============================================================================

# ---- 1. Find this project's folder ------------------------------------------
# Rscript passes the script path as --file=...; fall back to the working
# directory when the file is source()d from an interactive session.
app_dir <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 1) {
    dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/"))
  } else {
    normalizePath(getwd(), winslash = "/")
  }
})
setwd(app_dir)   # DB/ and data-raw/ paths in golem-config.yml are relative to it

# ---- 2. Logging ---------------------------------------------------------------
dir.create("logs", showWarnings = FALSE)
log_con <- file(file.path("logs", "app.log"), open = "at")
sink(log_con, type = "output", split = TRUE)   # split: still visible in a console
sink(log_con, type = "message")
log_line <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n", sep = "")
  flush(log_con)
}

# R buffers file output, and NSSM stops the service by killing R - so without
# regular flushes the last lines (often the interesting ones) never reach the
# file. Flush the app's own output every few seconds while it runs.
flush_log_regularly <- function() {
  if (isOpen(log_con)) flush(log_con)
  later::later(flush_log_regularly, 5)
}

log_line("---- starting CSIemail from ", app_dir, " ----")

exit_code <- tryCatch({

  # ---- 3. Activate the project's renv library -------------------------------
  # .Rprofile (which activates renv) only runs when R *starts* in this folder.
  # If R was started elsewhere, activate it here - otherwise the app would
  # silently run on the global library with different package versions.
  if (!"renv" %in% loadedNamespaces()) source("renv/activate.R")

  this_r <- paste(R.version$major, R.version$minor, sep = ".")
  lock_r <- jsonlite::read_json("renv.lock")$R$Version
  log_line("R ", this_r, " (renv.lock: R ", lock_r, "); library: ", .libPaths()[1])
  if (!identical(sub("\\.[0-9]+$", "", this_r), sub("\\.[0-9]+$", "", lock_r))) {
    log_line("WARNING: R ", this_r, " does not match renv.lock (R ", lock_r, "). ",
             "Point NSSM at R ", lock_r, "'s Rscript.exe.")
  }

  # ---- 4. Config profile + files that git does not carry --------------------
  if (!file.exists("inst/golem-config.yml")) {
    stop("inst/golem-config.yml is missing. It is git-ignored: copy ",
         "inst/golem-config.example.yml to inst/golem-config.yml and fill it in.")
  }
  if (!nzchar(Sys.getenv("GOLEM_CONFIG_ACTIVE"))) {
    Sys.setenv(GOLEM_CONFIG_ACTIVE = "production")
  }
  options(golem.app.prod = TRUE)

  pkgload::load_all(app_dir, export_all = FALSE, helpers = FALSE,
                    attach_testthat = FALSE, quiet = TRUE)

  # A wrong path would make RSQLite silently create an empty database
  for (key in c("db_path", "db_users")) {
    db <- CSIemail:::get_golem_config(key)
    if (!file.exists(db)) {
      stop("Database file for config key '", key, "' not found: ", db,
           " (relative to ", app_dir, ")")
    }
  }

  mode <- CSIemail:::app_mode_info()
  log_line("CSIemail ", as.character(utils::packageVersion("CSIemail")),
           " | config profile: ", mode$profile,
           " | SMTP credentials: ", mode$smtp_source,
           " | cashier recipients: ", mode$cashier_table,
           if (mode$test_recipients) " (TEST LIST)" else "")

  # ---- 5. Port + the URLs users can open ------------------------------------
  cli_args <- commandArgs(trailingOnly = TRUE)
  port <- as.integer(if (length(cli_args) >= 1) cli_args[1] else Sys.getenv("APP_PORT", "3846"))
  if (is.na(port)) stop("Port must be a number (got '", cli_args[1], "').")

  # Informational only - a failure here must never stop the app from starting.
  ips <- tryCatch({
    ipv4_lines <- grep("IPv4", system("ipconfig", intern = TRUE), value = TRUE)
    regmatches(ipv4_lines, regexpr("([0-9]{1,3}\\.){3}[0-9]{1,3}", ipv4_lines))
  }, error = function(e) character())
  log_line("App URL(s): ",
           paste0("http://", c(Sys.info()[["nodename"]], ips), ":", port, "/", collapse = "  "))

  # ---- 6. Run - blocks until the service is stopped --------------------------
  flush_log_regularly()
  shiny::runApp(run_app(), host = "0.0.0.0", port = port, launch.browser = FALSE)
  0L

}, error = function(e) {
  log_line("ERROR: ", conditionMessage(e))
  1L
})

log_line("app stopped (exit code ", exit_code, ")")
sink(type = "message")
sink(type = "output")
close(log_con)

# Non-zero exit on failure, so NSSM sees the crash and restarts the service.
quit(save = "no", status = exit_code)
