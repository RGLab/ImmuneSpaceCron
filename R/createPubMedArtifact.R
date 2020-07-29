#' Create artifact from PubMed data for use in ResourcesPage and MonitorIS to describe
#' publications related to ImmuneSpace
#'
#' @import data.table Rlabkey rvest xml2 stringr
#' @param subdir sub-directory
#' @export
#'
createPubMedArtifact <- function(subdir){

  labkey.url.base <- getLabkeyBaseURL()

  ######################################
  ###        PubMed Citations        ###
  ######################################

  sdyPubMedData <- labkey.selectRows(baseUrl = labkey.url.base,
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
  sdyPubMedData <- sdyPubMedData[ sdyPubMedData$study_accession %in% sdysInIS$name, ]
  sdyPubMedData <- sdyPubMedData[ grepl("^\\d{8}$", sdyPubMedData$pubmed_id), ]

  allIds <- getPubMedInfo(sdyPubMedData$pubmed_id)
  allIds <- mungePubMedData(allIds, sdyPubMedData)

  saveAndCleanUp(allIds, subdir, filename = "pubmedInfo")
}

getPubMedInfo <- function(pubMedIds){
  base <- "http://www.ncbi.nlm.nih.gov/pubmed?linkname=pubmed_pubmed_citedin&from_uid="
  results <- lapply(pubMedIds, function(id){
    page <- xml2::read_html(paste0(base, id))
    nodes <- rvest::html_nodes(page, css = '.docsum-content')
    res <- lapply(nodes, rvest::html_text)
    parsed <- lapply(res, function(x){
      x <- stringr::str_trim(x)
      spl <- strsplit(x, "\\.|\\?")[[1]]
      if(length(spl) > 0){
        title <- stringr::str_trim(spl[[1]])
        authors <- stringr::str_trim(spl[[2]])
        pubmedid <- gsub(".*PMID: (\\d{8}).*", "\\1", x)
      }else{
        title <- authors <- pubmedid <- NA
      }
      return(c(title, authors, pubmedid, id))
    })
    parsed <- data.table(do.call(rbind, parsed))
  })
  allIds <- data.table(do.call(rbind, results))
  cnames <- c("citedby_title",
              "citedby_authors",
              "citedby_id",
              "original_id")
  setnames(allIds, colnames(allIds), cnames)
  return(allIds)
}

mungePubMedData <- function(allIds, sdyPubMedData){
  allIds$study <- sdyPubMedData$study_accession[ match(allIds$original_id, sdyPubMedData$pubmed_id) ]
  allIds$studyNum <- as.numeric(gsub("SDY","", allIds$study))

  # Add date published (as YYYY-MM for sorting)
  sdyPubMedData$datePublished <- paste(sdyPubMedData$year, match(sdyPubMedData$month, month.abb), sep = "-")
  allIds$datePublished <- sdyPubMedData$datePublished[ match(allIds$study, sdyPubMedData$study_accession) ]

  allIds$original_title <- sdyPubMedData$title[ match(allIds$original_id, sdyPubMedData$pubmed_id) ]
  return(allIds)
}
