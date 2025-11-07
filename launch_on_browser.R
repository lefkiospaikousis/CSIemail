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
