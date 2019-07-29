#' Open a New Remote Browser
#' @details Shortcut for the following code. \code{library(RSelenium)
#' selCommand <- wdman::selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"),
#'                               retcommand = TRUE)
#' shell(selCommand, wait = FALSE, minimized = TRUE)
#' path <- "C:/Program Files (x86)/Google/Chrome Beta/Application/chrome.exe"
#' ecap <- list(chromeOptions = list("binary" = path))
#' return(RSelenium::remoteDriver(port = 4567L, browserName = "chrome",
#'                                extraCapabilities = ecap))}
newRemDr <- function() {
    library(RSelenium)
    selCommand <- wdman::selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"),
                                  retcommand = TRUE)
    shell(selCommand, wait = FALSE, minimized = TRUE)
    path <- "C:/Program Files (x86)/Google/Chrome Beta/Application/chrome.exe"
    ecap <- list(chromeOptions = list("binary" = path))
    return(RSelenium::remoteDriver(port = 4567L, browserName = "chrome",
                          extraCapabilities = ecap))
}

