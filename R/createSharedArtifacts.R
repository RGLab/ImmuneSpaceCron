#' Create ImmuneSpaceR Inits artifact for use in multiple modules
#'
#' @import data.table
#' @param subdir sub-directory
#' @export
#'
createSharedArtifacts <- function(subdir){

  if(!exists("logs_dt")){
    logs_dt <- getCurrentRDS(subdir, "_logs.rds")
  }

  ISR_inits <- parseLogsToISRinits(logs_dt)
  saveAndCleanUp(ISR_inits, subdir, "ISR_inits")


  studyViewsInUI <- parseLogsToUIStudyViews(logs_dt)
  saveAndCleanUp(studyViewsInUI, subdir, "studyViewsInUI")
}

parseLogsToISRinits <- function(logs){
  # Find unique ImmuneSpaceR Connections using the `ISC_study_datasets` query that is only
  # hit during the CreateConnection() method.

  # X11 column defines the type of connection to the server
  # X12 is the user's email
  ISR <- logs[ grepl("ImmuneSpaceR", X11) & !is.na(X12)]
  ISR[, study := ifelse(is.na(study), "All", study) ] # Fix study for project level connections
  ISR_inits <- ISR[ , list(cnt = .N), by = .(date2, X12, study)] # Filter down to connections
}

parseLogsToUIStudyViews <- function(logs){
  studyLogs <- logs[, folder := stringr::str_extract(X5, searchStrings$studies)]
  studyLogs <- studyLogs[ !is.na(folder) ]
  studyLogs[ , study := ifelse(is.na(study), "Data Finder", study) ]
  return(studyLogs)
}
