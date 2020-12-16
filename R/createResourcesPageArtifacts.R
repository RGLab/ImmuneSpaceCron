#' Create artifact from tomcat server logs to use for downstream analysis
#'
#' @import data.table stringr Rlabkey
#' @param subdir sub-directory
#' @export
#'
createResourcesPageArtifacts <- function(subdir){

  ######################################
  ###        General Setup           ###
  ######################################
  labkey.url.base <- getLabkeyBaseURL()

  if(!exists("logs_dt")){
    logs_dt <- getCurrentRDS(subdir, "_logs.rds")
  }

  ######################################
  ###       Study View Stats         ###
  ######################################
  ISR_inits <- getCurrentRDS(subdir, "ISR_inits")
  UI <- getCurrentRDS(subdir, "studyViewsInUI")
  studyStats <- makeStudyStats(ISR_inits, UI)

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

  pi <- mungePIData(pi, valid$Name)

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

  study <- mungeStudyOverviewData(study, valid$Name)

  # Assay * timepoint
  at <- labkey.selectRows(baseUrl = labkey.url.base,
                          folderPath = "/Studies/",
                          schemaName = "study",
                          queryName = "DimRedux_assay_data_computed",
                          colNameOpt = "fieldname")

  at <- mungeAssayTimepointData(at)

  all <- mergeStudyMetaData(valid, pi, study, at)

  saveAndCleanUp(all, subdir, filename = "sdyMetaData")
}

# ----------------------------------------------------------------

makeStudyStats <- function(ISR_inits, UI){
  ISR_study <- ISR_inits[ , list(ISR_connections = .N), by = .(study, date2) ]
  ISR_study <- ISR_study[ grepl("SDY", study) ]

  UI_views <- UI[, list(cnt = .N), by = .(date2, X12, study) ]
  UI_study <- UI_views[ , list(UI_pageviews = .N), by = .(study,date2) ]
  UI_study <- UI_study[ grepl("SDY", study) ]

  studyStats <- merge(ISR_study, UI_study, by = c("study","date2"), all=TRUE)
  studyStats[is.na(studyStats)] <- 0
  colnames(studyStats)[ grep("date2", colnames(studyStats))] <- "Date"
  studyStats[, studyId := as.numeric(gsub("SDY", "", study))]
  studyStats[, total_views := ISR_connections + UI_pageviews ]
  setcolorder(studyStats, c("Date", "study", "studyId",
                            "ISR_connections", "UI_pageviews", "total_views"))
  setorder(studyStats, "Date")
  return(studyStats)
}

mungePIData <- function(pi, validStudies){
  pi <- pi[ pi$role_in_study == "Principal Investigator", ]
  pi <- pi[ pi$study_accession %in% validStudies, ]
  coauths <- pi$study_accession[ duplicated(pi$study_accession)]
  for(i in coauths){
    rows <- grep(i, pi$study_accession)
    pi <- pi[ -rows[[1]], ]
  }
  colnames(pi)[ grepl("study_accession", colnames(pi))] <- "study"
  return(pi)
}

mungeStudyOverviewData <- function(study, validStudies){
  study <- study[ study$study_accession %in% validStudies, ]
  study$minimum_age[ is.na(study$minimum_age)] <- 0
  study$maximum_age[ is.na(study$maximum_age)] <- 100
  colnames(study)[ grepl("study_accession", colnames(study))] <- "study"
  return(study)
}

mungeAssayTimepointData <- function(at){
  at$study <- gsub("SUB\\d{5,6}\\.", "SDY", at$ParticipantId)
  at$Timepoint <- gsub(" ", "_", at$Timepoint)
  at$name_timepoint <- paste(at$Name, at$Timepoint, sep = "_")
  at <- data.table(at)
  at <- at[!duplicated(at[ , c("study", "name_timepoint")])]
  at$count <- 1
  return(at)
}

mergeStudyMetaData <- function(valid, pi, study, at){
  at <- dcast(at, study ~ name_timepoint, value.var = "count")
  at[is.na(at)] <- 0
  missingSdys <- valid$Name[ !valid$Name %in% at$study ]
  ms <- data.frame(matrix(ncol = length(colnames(at)),
                          nrow = length(missingSdys),
                          data = 0))
  colnames(ms) <- colnames(at)
  ms$study <- missingSdys
  atm <- rbind(at, ms)

  all <- Reduce( function(x,y){ merge(x, y, by = "study") }, list(pi, study, atm))
  rownames(all) <- all$study
  all <- all[ , -grep("study", colnames(all))]
}
