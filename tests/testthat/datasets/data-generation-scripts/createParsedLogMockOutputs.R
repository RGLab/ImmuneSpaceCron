library(readr)
library(Rlabkey)
library(data.table)

exclusionEmails <- ImmuneSpaceCron:::getExcludedEmailAddresses("https://datatools-dev.immunespace.org")
saveRDS(exclusionEmails, file = "tests/testthat/datasets/exclusionEmailsSample.rds")

makeParsedLogArtifact <- function(path.input, exclusionEmails) {
  res <- suppressWarnings(ImmuneSpaceCron:::readLogFile(path.input))
  date <- gsub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", path.input)
  date <- as.POSIXct(date, format = "%Y-%m-%d")
  res <- ImmuneSpaceCron:::parseLogData(res, exclusionEmails, date)
  res <- ImmuneSpaceCron:::createAccurateDateField(res)
}

# File Notes:
# 2019-04-25 has most data, but lacks rstudio and reports
# 2020-04-29 has rstudio
# 2019-03-30 has reports
datasetDir <- "tests/testthat/datasets/"
fls <- list.files(datasetDir)
fls <- file.path(datasetDir, grep("localhost_access_log", fls, value = TRUE))

all <- lapply(fls, makeParsedLogArtifact, exclusionEmails = exclusionEmails)
saveRDS(all[[1]], file = "tests/testthat/datasets/singleParsedLogFileOutput.rds")

all <- rbindlist(all)
saveRDS(all, file = "tests/testthat/datasets/parsedLogFileOutput.rds")
