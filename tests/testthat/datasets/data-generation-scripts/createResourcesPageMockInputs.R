library(Rlabkey)
labkey.url.base <- "https://datatools-dev.immunespace.org"

validStudies <- labkey.selectRows(
  baseUrl = labkey.url.base,
  folderPath = "/home/",
  schemaName = "study",
  queryName = "WPV_studies_with_status",
  colNameOpt = "fieldname"
)
saveRDS(validStudies, file = "tests/testthat/datasets/ResourcesPageMock.validStudies.rds")


studyPersonnel <- labkey.selectRows(
  baseUrl = labkey.url.base,
  folderPath = "/Studies/",
  schemaName = "immport",
  queryName = "study_personnel",
  colNameOpt = "fieldname",
  colSelect = c("role_in_study", "study_accession", "person_accession")
)
saveRDS(studyPersonnel, file = "tests/testthat/datasets/ResourcesPageMock.studyPersonnel.rds")


studyOverview <- labkey.selectRows(
  baseUrl = labkey.url.base,
  folderPath = "/Studies/",
  schemaName = "immport",
  queryName = "study",
  colNameOpt = "fieldname",
  colSelect = c(
    "study_accession",
    "actual_enrollment",
    "clinical_trial",
    "condition_studied",
    "maximum_age",
    "minimum_age",
    "sponsoring_organization",
    "initial_data_release_date"
  )
)
saveRDS(studyOverview, file = "tests/testthat/datasets/ResourcesPageMock.studyOverview.rds")


assayTimepoint <- labkey.selectRows(
  baseUrl = labkey.url.base,
  folderPath = "/Studies/",
  schemaName = "study",
  queryName = "DimRedux_assay_data_computed",
  colNameOpt = "fieldname"
)
saveRDS(assayTimepoint, file = "tests/testthat/datasets/ResourcesPageMock.assayTimepoint.rds")
