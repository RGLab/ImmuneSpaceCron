context("createPubMedInfoArtifact")

test_that("getPubMedInfo provides expected output", {
  pubmedIds <- c("28130404", "31491384", "29081779", "26682988", "31636302", "30842675")
  res <- ImmuneSpaceCronjobs:::getPubMedInfo(pubmedIds)
  template <- readRDS("datasets/pubMedInfoResults.rds")
  expect_equivalent(res, template)
})
