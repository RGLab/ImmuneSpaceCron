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

  pubMedIds <- unique(sdyPubMedData$pubmed_id)
  allIds <- getPubMedInfo(pubMedIds)
  allIds <- mungePubMedData(allIds, sdyPubMedData)

  saveAndCleanUp(allIds, subdir, filename = "pubmedInfo")
}

getPubMedInfo <- function(pubMedIds){
  base <- "https://pubmed.ncbi.nlm.nih.gov/?size=200&linkname=pubmed_pubmed_citedin&from_uid="

  # Must ensure connection closes otherwise http 500 error seems to occur, potentially
  # due to a block on number of connections per IP? (best guess)
  getParsedResults <- function(id, pageNumber){
    url <- paste0(base, id, "&page=", pageNumber)
    page <- xml2::read_html(url)
    nodes <- rvest::html_nodes(page, css = '.docsum-content')
    res <- lapply(nodes, rvest::html_text)
    closeAllConnections()
    gc()
    return(res)
  }

  # PubMed has limit of 200 citations per 'page' per id
  # but some ids have more citations
  results <- lapply(pubMedIds, function(id){
    print(id)
    resHolder <- list()
    valid <- TRUE
    pageNumber <- 1

    while(valid & pageNumber < 10){
      newRes <- getParsedResults(id, pageNumber)
      if(length(newRes) > 0){
        resHolder <- c(resHolder, newRes)
        pageNumber <- pageNumber + 1
      }else{
        valid <- FALSE
      }
    }

    parsed <- lapply(resHolder, function(x){
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
  tmp <- merge(sdyPubMedData, allIds, by.x = "pubmed_id", by.y = "original_id", all.y = TRUE)
  setnames(tmp, c("study_accession", "title", "pubmed_id"), c("study", "original_title", "original_id"))
  tmp$studyNum <- as.numeric(gsub("SDY","", tmp$study))
  tmp$datePublished <- paste(tmp$year, tmp$month, sep = "-")
  keepCols <- c("citedby_title",
                "citedby_authors",
                "citedby_id",
                "original_id",
                "original_title",
                "study",
                "studyNum",
                "datePublished")
  tmp <- tmp[, colnames(tmp) %in% keepCols ]
}
