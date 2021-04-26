context("createParsedLogsArtifacts")

test_that("daily log file can be read", {
  fl <- "datasets/localhost_access_log.2019-04-25.txt"
  res <- suppressWarnings(ImmuneSpaceCron:::readLogFile(fl))
  template <- readRDS("datasets/readLogFileOutput.rds")
  expect_equivalent(res, template)
})

test_that("daily log file parser returns expected output", {
  unparsed <- readRDS("datasets/readLogFileOutput.rds")
  exclusionEmails <- readRDS("datasets/exclusionEmailsSample.rds")
  date <- as.POSIXct("2019-04-25", format = "%Y-%m-%d")
  res <- ImmuneSpaceCron:::parseLogData(unparsed, exclusionEmails, date)
  res <- ImmuneSpaceCron:::createAccurateDateField(res)
  template <- readRDS("datasets/singleParsedLogFileOutput.rds")
  expect_true(all.equal(res, template, tolerance = .0001))
})
