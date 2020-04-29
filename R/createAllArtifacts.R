#' Create all artifacts for use in multiple modules
#'
#' @import data.table stringr Rlabkey rvest readr
#' @export
#'
createAllArtifacts <- function(subdir = "/share/files/Studies/R_API_resources/"){
  library(data.table)
  library(stringr)
  library(Rlabkey)
  library(rvest)
  library(readr)

  createParsedLogsArtifact(subdir)
  logs_dt <- getCurrentRDS(subdir, "_logs.rds")

  createImmuneSpaceRInitsArtifact(subdir)
  createPubMedArtifact(subdir)
  createResourcesPageArtifacts(subdir)
  createMonitorISArtifacts(subdir)
}
