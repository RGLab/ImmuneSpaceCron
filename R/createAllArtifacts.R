#' Create all artifacts for use in multiple modules
#'
#' @import data.table stringr Rlabkey readr
#' @param subdir sub-directory
#' @export
#'
createAllArtifacts <- function(subdir = "/share/resources/") {
  createParsedLogsArtifact(subdir)
  logs_dt <- getCurrentRDS(subdir, "_logs.rds")

  createSharedArtifacts(subdir)
  # Pubmed occasionally fails when site is unavailable
  dmp <- tryCatch(
    {
      createPubMedArtifact(subdir)
    },
    error = function(e) {
      return(e)
    }
  )
  createResourcesPageArtifacts(subdir)
  createMonitorISArtifacts(subdir)
  createGoogleAnalyticsArtifacts(subdir)
}
