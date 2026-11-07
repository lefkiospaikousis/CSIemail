# ============================================================================
# DEPRECATED - NOT USED ANYMORE (since 2026-11-08, version 0.3.2)
#
# Replaced by run_app.R in the project root, which the NSSM service starts
# directly with R 4.2.3's Rscript.exe (no ShinyApp.bat). run_app.R finds its
# own folder, logs to logs/app.log and serves on all interfaces, port 3846.
# See docs/handover/06-deployment.md.
#
# Kept only for reference and for rolling back to an older version. Its folder
# and log paths below are hard-coded to the production server.
# ============================================================================

#' This script is used for running the app on the browser via a windows service
#' 
#' The Windows task scheduler calls the 'RScript.exe' and the later sources this file
#' @noRd
# Set up error log ------------------------------------------------------------
#error_log <- file("C:/Users/pcuser/OneDrive/IMPROVAST/ACS/CSIemail/error_log.Rout", open="wt")
error_log <- file("C:/Users/administrator.ACSCY/Documents/CSIemail/error_log.Rout", open="wt")
sink(error_log, type="message")

try({
  print(Sys.time())
  
  library(shiny)
  #folder_address = 'C:/Users/pcuser/OneDrive/IMPROVAST/ACS/CSIemail'
  folder_address = 'C:/Users/administrator.ACSCY/Documents/CSIemail'
  
  x <- system("ipconfig", intern=TRUE)
  z <- x[grep("IPv4", x)]
  ip <- gsub(".*? ([[:digit:]])", "\\1", z)
  print(paste0("the Shiny Web application runs on: http://", ip, ":3846/"))
  
  # Run it but don;t launch the browser. Whenever I type the address on  the browser
  # I will see the app
  runApp(folder_address, launch.browser=FALSE, port = 3846         #, host = getOption("shiny.host", "127.0.0.1")
         , host = ip
         
  )
  
})
