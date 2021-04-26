library(ImmuneSpaceCron) # load searchStrings
logs <- readRDS("tests/testthat/datasets/parsedLogFileOutput.rds")

res <- ImmuneSpaceCron:::parseLogsToISRinits(logs)
saveRDS(res, file = "tests/testthat/datasets/parseLogsToISRInitsOutput.rds")

res <- ImmuneSpaceCron:::parseLogsToUIStudyViews(logs)
saveRDS(res, file = "tests/testthat/datasets/parseLogsToUIStudyViews.rds")
