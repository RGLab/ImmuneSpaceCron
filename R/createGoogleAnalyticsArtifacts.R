#' Create a google analytics artifact by reading in all existing raw data generated
#' by Python/createGoogleAnalyticsArtifacts.py for previous dates, then assessing
#' dates that must be run and adding those.
#'
#' @import data.table
#' @param subdir sub-directory
#' @export
#'
createGoogleAnalyticsArtifacts <- function(subdir) {
  googleAnalyticsKeyFile <- Sys.getenv("GOOGLE_ANALYTICS_KEY_FILE")

  if (nchar(googleAnalyticsKeyFile) == 0) {
    stop("No google analytics key file found. Put in .Renviron.")
  }

  pathToScript <- file.path(
    system.file(package = "ImmuneSpaceCron"),
    "createGoogleAnalyticsArtifacts.py"
  )

  googleAnalyticsOutputDir <- file.path(subdir, "googleAnalyticsOutput")
  if (!dir.exists(googleAnalyticsOutputDir)) {
    dir.create(googleAnalyticsOutputDir)
  }

  currentFiles <- list.files(googleAnalyticsOutputDir)
  if (length(currentFiles) > 0) {
    rawResults <- lapply(file.path(googleAnalyticsOutputDir, currentFiles), fread)
    rawResultsDF <- rbindlist(rawResults, fill = TRUE) # handle empty values
    rawResultsDF <- rawResultsDF[!is.na(rawResultsDF$date)]

    nextDateNeeded <- as.Date(max(rawResultsDF$date)) + 1
  } else {
    # Public launch of datatools.immunespace.org
    nextDateNeeded <- as.Date("2016-01-01")
  }

  # nextDateNeeded == NA when being re-run on same day
  if (nextDateNeeded != Sys.Date()) {
    dates <- seq.Date(from = nextDateNeeded, to = Sys.Date(), by = "day")

    startEndDates <- data.frame(
      start = dates[1:length(dates) - 1],
      end = dates[2:length(dates)],
      stringsAsFactors = FALSE
    )

    res <- apply(startEndDates, 1, function(x) {
      ret <- getDailyGoogleAnalyticsResults(
        startDay = x[[1]],
        endDay = x[[2]],
        pathToScript = pathToScript,
        googleAnalyticsOutputDir = googleAnalyticsOutputDir,
        keyFile = googleAnalyticsKeyFile
      )
    })

    allResults <- rbindlist(res)

    if (exists("rawResultsDF")) {
      allResults <- rbind(rawResultsDF, allResults)
    }
  } else {
    allResults <- rawResultsDF
  }

  allResults <- mungeGoogleAnalyticsData(allResults)
  saveAndCleanUp(allResults, subdir, "googleAnalyticsArtifact")
}

getDailyGoogleAnalyticsResults <- function(startDay, endDay, pathToScript,
                                           googleAnalyticsOutputDir, keyFile) {
  cmd <- paste(
    "python", pathToScript,
    googleAnalyticsOutputDir,
    startDay,
    endDay,
    keyFile
  )
  system(cmd, wait = TRUE)

  dailyResultsPath <- paste0(
    googleAnalyticsOutputDir,
    "/",
    startDay,
    "_googleAnalyticsRawDailyData.csv"
  )
  dailyResults <- fread(dailyResultsPath)
}

mungeGoogleAnalyticsData <- function(allResults) {

  # rm likely admin results
  adminPageTerms <- c(
    "admin",
    "pipeline",
    "integration",
    "queryName"
  )
  adminPages <- paste(adminPageTerms, collapse = "|")
  allResults <- allResults[!grepl(adminPages, landingPage) & !grepl(adminPages, secondPage)]

  # rm likely admins by looking at source
  adminSourceTerms <- c(
    "localhost",
    "github.com/RGLab/immport/",
    "labkey.org/HIPC/Support%20Tickets",
    "labkey.org/home/Developer",
    "3\\.218\\.206\\.229",
    "analytics\\.google\\.com/analytics"
  )
  adminSources <- paste(adminSourceTerms, collapse = "|")
  allResults <- allResults[!grepl(adminSources, fullReferrer)]

  # rm direct results that are NOT to the home page since they are not informative
  allResults <- allResults[!grepl("direct", fullReferrer) & !grepl("^/$", landingPage)]

  # summarize users by source as "sessions" as these could actually be same user
  summarizedResults <- allResults[, list(
    usageSessions = sum(users),
    bounces = sum(bounces)
  ),
  by = c("source", "fullReferrer", "date")
  ]
}
