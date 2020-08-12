#' Create artifacts for MonitorIS module report using parsed logs
#'
#' @import data.table stringr
#' @param subdir sub-directory
#' @export
#'
createMonitorISArtifacts <- function(subdir){

  if(!exists("logs_dt")){
    logs_dt <- getCurrentRDS(subdir, "_logs.rds")
  }

  ######################################
  ###                                ###
  ######################################

  logs.study <- getCurrentRDS(subdir, "studyViewsInUI")
  logs.study <- parseLogs.study(logs.study)
  saveAndCleanUp(logs.study, subdir, "log_study")

  #######################################
  ###           log_module            ###
  #######################################
  logs.modules <- parseLogs.modules(logs_dt, searchStrings$modules)
  saveAndCleanUp(logs.modules, subdir, "log_module")

  #######################################
  ###           log_reports           ###
  #######################################
  logs.reports <- parseLogs.reports(logs_dt, searchStrings$reports)
  saveAndCleanUp(logs.reports, subdir, "log_reports")

  #######################################
  ###           log_rstudio           ###
  #######################################
  logs.rstudio <- parseLogs.rstudio(logs_dt, searchStrings$rstudio)
  saveAndCleanUp(logs.rstudio, subdir, "log_rstudio")

}

# ----------------------------------------------------------------------
parseLogs.study <- function(logs.study){
  logs.study <- logs.study[ , list(count = .N), by = study]
  setorder(logs.study, -count)
  return(logs.study)
}


parseLogs.modules <- function(logs, searchString){
  logs.modules <- logs[, module := stringr::str_extract(X5, searchString)]
  logs.modules <- logs.modules[ !is.na(module) ]
  logs.modules <- logs.modules[ , list(count = .N), by = c("module", "date2")]
  setorder(logs.modules, date2)
  return(logs.modules)
}

parseLogs.reports <- function(logs, searchString){
  # First half of searchString is pre-2019, second half is post
  logs.reports <- logs[ , report := X5 ]
  logs.reports <- logs.reports[ grepl(searchString, report) ]
  logs.reports$report <- sapply(logs.reports$report, function(x){
    return(regmatches(x, regexpr("SDY\\d{3}|IS\\d{1}", x)))
  })
  logs.reports <- logs.reports[ , list(count = .N), by = c("report","date2")]
  setorder(logs.reports, date2)
  return(logs.reports)
}

parseLogs.rstudio <- function(logs, searchString){
  logs.rstudio <- logs[ (grepl(searchString, X5)) ]
  logs.rstudio <- logs.rstudio[ , list(count = .N), by = date2 ]
  logs.rstudio <- logs.rstudio[ , Date := as.Date(cut(date2, breaks = "days")) ]
  logs.rstudio <- logs.rstudio[ , list(Sessions = sum(count)), by = Date ]
}
