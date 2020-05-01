searchStrings <- list(
  modules = "(?<=GET /)(DataExplorer|GeneExpressionExplorer|GeneSetEnrichmentAnalysis|ImmuneResponsePredictor|DimensionReduction)(?=/\\S+/begin.view HTTP/1.1)",
  reports = "/reports/Studies/SDY(144|180|207|269)/runReport\\.view|IS1/begin\\.view.+pageId=Report",
  rstudio = "GET /_rstudio/ HTTP/1.1|GET /login/login\\.view\\?returnUrl=%2Frstudio",
  studies = "/project/(Studies/SDY\\d+|Studies|HIPC/IS\\d+|HIPC/Lyoplate)/begin.view\\?? HTTP/1.1"
)
usethis::use_data(searchStrings)
