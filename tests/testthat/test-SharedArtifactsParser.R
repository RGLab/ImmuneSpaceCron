context("createSharedArtifacts")

test_that("parseLogsToISRinits", {
  logs <- readRDS("datasets/parsedLogFileOutput.rds")
  res <- ImmuneSpaceCronjobs:::parseLogsToISRinits(logs)
  expect_true(all.equal(colnames(res), c("date2", "X12", "study", "cnt")))
  types <- unname(sapply(res, typeof))
  expect_true(all.equal(types, c("double", "character", "character", "integer")))
  expect_true(length(unique(res$X12)) == 5)
})

test_that("parseLogsToStudyViewsinUI", {
  logs <- readRDS("datasets/parsedLogFileOutput.rds")
  res <- ImmuneSpaceCronjobs:::parseLogsToUIStudyViews(logs)
  expect_true(all(c("date2", "X12", "study") %in% colnames(res)))
  expectedStudies <- c("Data Finder", "SDY28", "SDY18", "SDY515", "SDY1293",
                       "SDY296", "SDY387", "SDY241", "SDY520", "SDY702",
                       "SDY820", "SDY888", "SDY269")
  expect_equivalent(sort(expectedStudies), sort(unique(res$study)))
  expect_true(length(unique(res$X12)) == 8)
})
