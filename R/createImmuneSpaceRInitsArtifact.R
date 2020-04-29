#' Create ImmuneSpaceR Inits artifact for use in multiple modules
#'
#' @import data.table
#' @export
#'
createImmuneSpaceRInitsArtifact <- function(subdir){
  requireLibs(c("data.table"))
  logs_dt <- getCurrentRDS(subdir, "_logs.rds")

  #######################################
  ###           ISR_inits             ###
  #######################################
  # Find unique ImmuneSpaceR Connections using the `ISC_study_datasets` query that is only
  # hit during the CreateConnection() method.

  # X11 column defines the type of connection to the server
  # X12 is the user's email
  ISR <- logs_dt[ grepl("ImmuneSpaceR", X11) & !is.na(X12)]
  ISR[, study := ifelse(is.na(study), "All", study) ] # Fix study for project level connections
  ISR_inits <- ISR[ , list(cnt = .N), by = .(date2, X12, study)] # Filter down to connections
  saveAndCleanUp(ISR_inits, subdir, "ISR_inits")
}
