% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remDr.R
\name{newRemDr}
\alias{newRemDr}
\title{Open a New Remote Browser}
\usage{
newRemDr()
}
\description{
Open a New Remote Browser
}
\details{
Shortcut for the following code. \code{library(RSelenium)
selCommand <- wdman::selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"),
                              retcommand = TRUE)
shell(selCommand, wait = FALSE, minimized = TRUE)
path <- "C:/Program Files (x86)/Google/Chrome Beta/Application/chrome.exe"
ecap <- list(chromeOptions = list("binary" = path))
return(RSelenium::remoteDriver(port = 4567L, browserName = "chrome",
                               extraCapabilities = ecap))}
}
