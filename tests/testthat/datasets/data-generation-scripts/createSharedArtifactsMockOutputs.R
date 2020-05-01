logs <- readRDS("tests/testthat/datasets/parsedLogFileOutput.rds")

res <- ImmuneSpaceCronjobs:::parseLogsToISRinits(logs)
saveRDS(res, file = "tests/testthat/datasets/parseLogsToISRInitsOutput.rds")

res <- ImmuneSpaceCronjobs:::parseLogsToUIStudyViews(logs)
saveRDS(res, file = "tests/testthat/datasets/parseLogsToUIStudyViews.rds")
