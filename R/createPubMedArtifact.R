#' Create artifact from PubMed data for use in ResourcesPage and MonitorIS to describe
#' publications related to ImmuneSpace
#'
#' @import data.table Rlabkey
#' @export
#'
createPubMedArtifact <- function(subdir){
  requireLibs(c("Rlabkey",
                "data.table"))


  labkey.url.base <- getLabkeyBaseURL()

  ######################################
  ###        PubMed Citations        ###
  ######################################

  df <- labkey.selectRows(baseUrl = labkey.url.base,
                          folderPath = "/Studies/",
                          schemaName = "immport",
                          queryName = "study_pubmed",
                          colNameOpt = "fieldname")

  # Subset down to those in immunespace only
  sdysInIS <- labkey.selectRows(baseUrl = labkey.url.base,
                                folderPath = "/home/",
                                schemaName = "lists",
                                queryName = "Studies",
                                colNameOpt = "fieldname")
  df <- df[ df$study_accession %in% sdysInIS$name, ]

  # For each pubmedid
  base <- "http://www.ncbi.nlm.nih.gov/pubmed?linkname=pubmed_pubmed_citedin&from_uid="
  results <- lapply(df$pubmed_id, function(id){
    page <- read_html(paste0(base, id))
    nodes <- html_nodes(page, css = '.rslt')
    res <- lapply(nodes, html_text)
    parsed <- lapply(res, function(x){
      spl <- strsplit(x, "\\.")[[1]]
      if(length(spl) > 0){
        title <- spl[[1]]
        authors <- spl[[2]]
        pubmedid <- spl[[length(spl)]]
        pubmedid <- regmatches(pubmedid, regexpr("\\d{8}", pubmedid))
      }else{
        title <- authors <- pubmedid <- NA
      }
      return(c(title, authors, pubmedid, id))
    })
    parsed <- data.frame(do.call(rbind, parsed))
  })
  allIds <- data.table(do.call(rbind, results))
  cnames <- c("citedby_title",
              "citedby_authors",
              "citedby_id",
              "original_id")
  setnames(allIds, colnames(allIds), cnames)

  # Add study
  allIds$study <- df$study_accession[ match(allIds$original_id, df$pubmed_id) ]
  allIds$studyNum <- as.numeric(gsub("SDY","", allIds$study))

  # Add date published (as YYYY-MM for sorting)
  df$datePublished <- paste(df$year, match(df$month, month.abb), sep = "-")
  allIds$datePublished <- df$datePublished[ match(allIds$study, df$study_accession) ]

  # title - TODO v2 (journal impact score using scopus API)
  allIds$original_title <- df$title[ match(allIds$original_id, df$pubmed_id) ]

  saveAndCleanUp(allIds, subdir, filename = "pubmedInfo")
}
