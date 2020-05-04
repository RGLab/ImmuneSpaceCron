context("createPubMedInfoArtifact")

test_that("getPubMedInfo provides expected output", {
  pubmedIds <- c("28130404", "31491384", "29081779", "26682988", "31636302", "30842675")
  res <- ImmuneSpaceCronjobs:::getPubMedInfo(pubmedIds)
  template <- readRDS("datasets/pubMedInfoResults.rds")
  setdiff(template$citedby_title, res$citedby_title)
  expect_true(all.equal(res, template))
})
