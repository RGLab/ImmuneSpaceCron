context("createResourcesPageArtifacts")

studyPersonnel <- readRDS("datasets/ResourcesPageMock.studyPersonnel.rds")
assayTimepoint <- readRDS("datasets/ResourcesPageMock.assayTimepoint.rds")
studyOverview <- readRDS("datasets/ResourcesPageMock.studyOverview.rds")
validStudies <- readRDS("datasets/ResourcesPageMock.validStudies.rds")

test_that("makeStudyStats", {
  ISR_inits <- readRDS("datasets/parseLogsToISRInitsOutput.rds")
  UI <- readRDS("datasets/parseLogsToUIStudyViews.rds")
  res <- ImmuneSpaceCronjobs:::makeStudyStats(ISR_inits, UI)
  expectedCols <- c("Date", "study", "studyId", "ISR_connections", "UI_pageviews", "total_views")
  expect_equivalent(colnames(res), expectedCols)
  types <- unname(sapply(res, typeof))
  expectedTypes <- c("double", "character", "double", "double", "integer", "double")
  expect_equivalent(types, expectedTypes)
  expect_true(all(grepl("SDY\\d{2,4}", res$study)))
  expect_true(nrow(res) == 8)
})

test_that("mungePIData", {
  res <- ImmuneSpaceCronjobs:::mungePIData(studyPersonnel, validStudies$Name)
  expect_true(all.equal(colnames(res), c("role_in_study", "study", "person_accession")))
  types <- unname(sapply(res, typeof))
  expect_equivalent(types, c("character", "character", "character"))
  expect_true(nrow(res) > 100)
  expect_true(all(!is.na(res$study)))
  expect_true(all(grepl("SDY\\d{2,4}", res$study)))
})

test_that("mungeStudyOverviewData", {
  res <- ImmuneSpaceCronjobs:::mungeStudyOverviewData(studyOverview, validStudies$Name)
  expectedCols <- c("study", "actual_enrollment", "clinical_trial", "condition_studied",
                    "maximum_age", "minimum_age", "sponsoring_organization", "initial_data_release_date")
  expect_equivalent(colnames(res), expectedCols)
  types <- unname(sapply(res, typeof))
  expectedTypes <- c("character", "integer", "character", "character",
                     "character", "character", "character", "double")
  expect_equivalent(types, expectedTypes)
  expect_true(nrow(res) > 100)
  expect_true(all(!is.na(res$study)))
})

test_that("mungeAssayTimepointData", {
  res <- ImmuneSpaceCronjobs:::mungeAssayTimepointData(assayTimepoint)
  expectedCols <- c("ParticipantId", "Name", "Label", "Timepoint",
                    "Features", "study", "name_timepoint", "count")
  expect_equivalent(colnames(res), expectedCols)
  types <- unname(sapply(res, typeof))
  expectedTypes <- c("character", "character", "character", "character",
                     "integer", "character", "character", "double")
  expect_equivalent(types, expectedTypes)
  expect_true(nrow(res) > 1000)
  expect_true(all(!is.na(res$study)))
  expect_true(all(grepl("SDY\\d{2,4}", res$study)))
})

test_that("mergeStudyMetaData", {
  assayTimepoint <- ImmuneSpaceCronjobs:::mungeAssayTimepointData(assayTimepoint)
  studyOverview <- ImmuneSpaceCronjobs:::mungeStudyOverviewData(studyOverview, validStudies$Name)
  studyPersonnel <- ImmuneSpaceCronjobs:::mungePIData(studyPersonnel, validStudies$Name)

  res <- ImmuneSpaceCronjobs:::mergeStudyMetaData(valid = validStudies,
                                                  pi = studyPersonnel,
                                                  study = studyOverview,
                                                  at = assayTimepoint)
  sdyOverviewCols <- c("person_accession", "actual_enrollment", "clinical_trial", "condition_studied",
                       "maximum_age", "minimum_age", "sponsoring_organization", "initial_data_release_date")
  expect_true(all(sdyOverviewCols %in% colnames(res)))
  expect_true(any(grepl("neut_ab_titer", colnames(res))))
  expect_true(nrow(res) == nrow(studyOverview))
  elisa <- res[ , grep("elisa", colnames(res))]
  expect_equivalent(range(elisa), c(0,1))
  expect_true(all(grepl("SDY\\d{2,4}", rownames(res))))
})
