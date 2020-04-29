#' Create artifacts for MonitorIS module report using parsed logs
#'
#' @import data.table stringr
#' @export
#'
createMonitorISArtifacts <- function(subdir){
  requireLibs(c("data.table",
                "stringr"))

  if(!exists("logs_dt")){
    logs_dt <- getCurrentRDS(subdir, "_logs.rds")
  }

  #######################################
  ###           log_study             ###
  #######################################
  searchString <- "/project/(Studies/SDY\\d+|Studies|HIPC/IS\\d+|HIPC/Lyoplate)/begin.view\\?? HTTP/1.1"
  log_study <- logs_dt[, folder := stringr::str_extract(X5, searchString)]
  log_study <- log_study[ !is.na(folder) ]
  log_study[ , study := ifelse(is.na(study), "Data Finder", study) ]
  log_study <- log_study[ , list(count = .N), by = study]
  setorder(log_study, -count)
  saveAndCleanUp(log_study, subdir, "log_study")


  #######################################
  ###           log_module            ###
  #######################################
  string <- "(?<=GET /)(DataExplorer|GeneExpressionExplorer|GeneSetEnrichmentAnalysis|ImmuneResponsePredictor|DimensionReduction)(?=/\\S+/begin.view HTTP/1.1)"
  log_module <- logs_dt[, module := stringr::str_extract(X5, string)]
  log_module <- log_module[ !is.na(module) ]
  log_module <- log_module[ , list(count = .N), by = module]
  setorder(log_module, -count)
  saveAndCleanUp(log_module, subdir, "log_module")


  #######################################
  ###           log_reports           ###
  #######################################
  # Get study-specific reports, incl. IS1 using search string based on minimal URL
  # then update the report column match study id.

  log_reports <- logs_dt[ , report := X5 ]
  searchString <- "/reports/Studies/SDY(144|180|207|269)/runReport\\.view|IS1/begin\\.view.+pageId=Report"
  log_reports <- log_reports[ grepl(searchString, report) ]
  log_reports$report <- sapply(log_reports$report, function(x){
    return(regmatches(x, regexpr("SDY\\d{3}|IS\\d{1}", x)))
  })
  log_reports <- log_reports[ , list(count = .N), by = report]
  setorder(log_reports, -count)
  saveAndCleanUp(log_reports, subdir, "log_reports")


  #######################################
  ###           log_rstudio           ###
  #######################################

  # First half of searchString is pre-2019, second half is post
  searchString <- "GET /_rstudio/ HTTP/1.1|GET /login/login\\.view\\?returnUrl=%2Frstudio"
  log_rstudio <- logs_dt[ (grepl(searchString, X5)) ]
  log_rstudio <- log_rstudio[ , list(count = .N), by = date2 ]
  log_rstudio <- log_rstudio[ , Date := as.Date(cut(date2, breaks = "days")) ]
  log_rstudio <- log_rstudio[ , list(Sessions = sum(count)), by = Date ]
  saveAndCleanUp(log_rstudio, subdir, "log_rstudio")

}
