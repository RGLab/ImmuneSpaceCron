library(testthat)
library(ImmuneSpaceCronjobs)

if (Sys.info()["user"] != "biocbuild") test_check("ImmuneSpaceCronjobs")
