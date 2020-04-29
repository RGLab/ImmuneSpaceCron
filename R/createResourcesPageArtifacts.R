#' Create artifact from tomcat server logs to use for downstream analysis
#'
#' @import data.table stringr Rlabkey rvest
#' @export
#'
createResourcesPageArtifacts <- function(subdir){

  requireLibs(c("Rlabkey",
                "data.table",
                "stringr",
                "rvest"))

  ######################################
  ###        General Setup           ###
  ######################################
  labkey.url.base <- getLabkeyBaseURL()
  logs_dt <- getCurrentRDS(subdir, "_logs.rds")

  ######################################
  ###           Tomcat Logs          ###
  ######################################

  # ImmuneSpaceR connections to a study (without date filtering)
  ISR_inits <- getCurrentRDS(subdir, "ISR_inits")
  ISR_study <- ISR_inits[ , list(ISR_connections = .N), by = .(study, date2) ]
  ISR_study <- ISR_study[ grepl("SDY", study) ]

  # UI pageviews of a study
  searchString <- "/project/(Studies/SDY\\d+|Studies|HIPC/IS\\d+|HIPC/Lyoplate)/begin.view\\?? HTTP/1.1"
  logs_dt <- logs_dt[, folder := stringr::str_extract(X5, searchString)]
  UI <- logs_dt[ !is.na(folder) ]
  UI[ , study := ifelse(is.na(study), "Data Finder", study) ]
  UI_views <- UI[, list(cnt = .N), by = .(date2, X12, study) ]
  UI_study <- UI_views[ , list(UI_pageviews = .N), by = .(study,date2) ]
  UI_study <- UI_study[ grepl("SDY", study) ]

  # Merge together and prep
  studyStats <- merge(ISR_study, UI_study, by = c("study","date2"), all=TRUE)
  studyStats[is.na(studyStats)] <- 0
  colnames(studyStats)[ grep("date2", colnames(studyStats))] <- "Date"
  studyStats[, studyId := as.numeric(gsub("SDY", "", study))]
  studyStats[, total_views := ISR_connections + UI_pageviews ]
  setcolorder(studyStats, c("Date", "study", "studyId",
                            "ISR_connections", "UI_pageviews", "total_views"))
  setorder(studyStats, "Date")

  saveAndCleanUp(studyStats, subdir, filename = "parsedLogs")

  ######################################
  ###          Study Metadata        ###
  ######################################

  # Ensure only valid studies in ImmuneSpace
  valid <- labkey.selectRows(baseUrl = labkey.url.base,
                             folderPath = "/home/",
                             schemaName = "study",
                             queryName = "WPV_studies_with_status",
                             colNameOpt = "fieldname")

  # Study Investigator
  pi <- labkey.selectRows(baseUrl = labkey.url.base,
                          folderPath = "/Studies/",
                          schemaName = "immport",
                          queryName = "study_personnel",
                          colNameOpt = "fieldname",
                          colSelect = c("role_in_study", "study_accession", "person_accession"))
  pi <- pi[ pi$role_in_study == "Principal Investigator", ]
  pi <- pi[ pi$study_accession %in% valid$Name, ]
  coauths <- pi$study_accession[ duplicated(pi$study_accession)]
  for(i in coauths){
    rows <- grep(i, pi$study_accession)
    pi <- pi[ -rows[[1]], ]
  }
  colnames(pi)[ grepl("study_accession", colnames(pi))] <- "study"

  # Study Overview
  study <- labkey.selectRows(baseUrl = labkey.url.base,
                             folderPath = "/Studies/",
                             schemaName = "immport",
                             queryName = "study",
                             colNameOpt = "fieldname",
                             colSelect = c("study_accession",
                                           "actual_enrollment",
                                           "clinical_trial",
                                           "condition_studied",
                                           "maximum_age",
                                           "minimum_age",
                                           "sponsoring_organization",
                                           "initial_data_release_date"))
  study <- study[ study$study_accession %in% valid$Name, ]
  study$minimum_age[ is.na(study$minimum_age)] <- 0
  study$maximum_age[ is.na(study$maximum_age)] <- 100
  colnames(study)[ grepl("study_accession", colnames(study))] <- "study"

  # Assay * timepoint
  at <- labkey.selectRows(baseUrl = labkey.url.base,
                          folderPath = "/Studies/",
                          schemaName = "study",
                          queryName = "DimRedux_assay_data_computed",
                          colNameOpt = "fieldname")
  at$study <- gsub("SUB\\d{5,6}\\.", "SDY", at$ParticipantId)
  at$Timepoint <- gsub(" ", "_", at$Timepoint)
  at$name_timepoint <- paste(at$Name, at$Timepoint, sep = "_")
  at <- data.table(at)
  at <- at[!duplicated(at[ , c("study", "name_timepoint")])]
  at$count <- 1
  at <- dcast(at, study ~ name_timepoint, value.var = "count")
  at[is.na(at)] <- 0
  missingSdys <- valid$Name[ !valid$Name %in% at$study]
  ms <- data.frame(matrix(ncol = length(colnames(at)),
                          nrow = length(missingSdys),
                          data = 0))
  colnames(ms) <- colnames(at)
  ms$study <- missingSdys
  atm <- rbind(at, ms)

  # Merge
  all <- Reduce( function(x,y){ merge(x, y, by = "study") }, list(pi, study, atm))
  rownames(all) <- all$study
  all <- all[ , -grep("study", colnames(all))]

  saveAndCleanUp(all, subdir, filename = "sdyMetaData")
}
