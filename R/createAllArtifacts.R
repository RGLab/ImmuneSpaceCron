#' Create all artifacts for use in multiple modules
#'
#' @import data.table stringr Rlabkey rvest readr
#' @param subdir sub-directory
#' @export
#'
createAllArtifacts <- function(subdir = "/share/files/Studies/R_API_resources/"){

  createParsedLogsArtifact(subdir)
  logs_dt <- getCurrentRDS(subdir, "_logs.rds")

  createSharedArtifacts(subdir)
  createPubMedArtifact(subdir)
  createResourcesPageArtifacts(subdir)
  createMonitorISArtifacts(subdir)
  createGoogleAnalyticsArtifacts(subdir)
}
