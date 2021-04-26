library(rvest)
pubmedIds <- c("28130404", "31491384", "29081779", "26682988", "31636302", "30842675")
res <- ImmuneSpaceCron:::getPubMedInfo(pubmedIds)
saveRDS(res, file = "tests/testthat/datasets/pubMedInfoResults.rds")
