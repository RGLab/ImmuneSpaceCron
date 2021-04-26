#' Get labkey.url.base
#'
#' @export
#'
getLabkeyBaseURL <- function() {
  labkey.url.base <- ifelse(Sys.info()["nodename"] == "ImmuneTestRserve2",
    "https://test.immunespace.org",
    "https://www.immunespace.org"
  )
}

#' Save RDS object and clean up any previous versions
#'
#' @param subdir location of artifacts on rsT or rsP
#' @param data the data to save
#' @param filename filename suffix to use for saving and cleaning up
#' @export
#'
saveAndCleanUp <- function(data, subdir, filename) {
  cleanUp(subdir, filename)
  saveOutput(data, subdir, filename)
}


saveOutput <- function(data, subdir, filename) {
  saveRDS(data, file = paste0(subdir, Sys.Date(), "_", filename, ".rds"))
}


cleanUp <- function(subdir, filename) {
  allFiles <- list.files(subdir)
  targetFiles <- allFiles[grep(filename, allFiles)]
  if (length(targetFiles) > 0) {
    targetFiles <- file.path(subdir, targetFiles)
    ret <- sapply(targetFiles, file.remove)
  }
}

#' Read in latest RDS object for a given filename
#'
#' @param subdir location of artifacts on rsT or rsP
#' @param filename filename suffix to use for saving and cleaning up
#' @export
#'
getCurrentRDS <- function(subdir, filename) {
  allFiles <- list.files(subdir)
  targetFiles <- allFiles[grepl(filename, allFiles)]
  mostRecentFile <- sort(targetFiles, decreasing = TRUE)[[1]]
  tmp <- readRDS(file.path(subdir, mostRecentFile))
}

#' Only load libraries if not already done
#'
#' @param libs libraries to load
#' @export
#'
requireLibs <- function(libs) {
  libs <- libs[!libs %in% .packages()]
  if (length(libs) > 0) {
    invisible(lapply(libs, library, character.only = TRUE))
  }
}
