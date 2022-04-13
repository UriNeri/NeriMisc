# Information  --------------------------------------------------------------------
## Script name: Addins.R
## Purpose of script: A few addins to R/Rstudio that I use.
## Copyright (c) Uri Neri, 2020-2022
## Email: uri.neri@gmail.com

open_last_file <- function() {
  rstudioapi::navigateToFile(file =readLines("~/.local/share/rstudio/monitored/lists/file_mru")[1])
}

open_all_recent_files <- function() {
  rstudioapi::navigateToFile(file =readLines("~/.local/share/rstudio/monitored/lists/file_mru"))
}

open_selected_recent_file <- function() {
  rstudioapi::navigateToFile(file = select.list(choices = readLines("~/.local/share/rstudio/monitored/lists/file_mru"),graphics = T))
}
