#' Create artifact from tomcat server logs to use for downstream analysis
#'
#' @import data.table stringr Rlabkey readr
#' @export
#'
createParsedLogsArtifact <- function(subdir){

  requireLibs(c("Rlabkey",
                "data.table",
                "stringr",
                "readr"))


  ######################################
  ###        General Setup           ###
  ######################################

  # Set dates for subsetting.
  format <- "%Y-%m-%d"
  from <- as.POSIXct("2016-01-01", format = format)
  to <- as.POSIXct(Sys.Date(), format = format)

  # Set labkey.url.base given the hostname
  labkey.url.base <- ifelse(Sys.info()["nodename"] == "ImmuneTestRserve2",
                            "https://test.immunespace.org",
                            "https://www.immunespace.org")

  # Ensure admins or "not-real" users are excluded
  usersToExclude <- labkey.selectRows(baseUrl = labkey.url.base,
                                      folderPath = "/home",
                                      schemaName = "core",
                                      queryName = "SiteUsers",
                                      viewName = "",
                                      colFilter = makeFilter(c("Groups/Group$SName",
                                                               "CONTAINS_ONE_OF",
                                                               "Developers;Administrators;LabKey;excludeFromLogs")),
                                      containerFilter = NULL)

  # Admin emails no longer in the DB
  oldAdminEmails <- c("rsautera@fhcrc.org",
                      "ldashevs@scharp.org")

  # Vectors of people to exclude from counts
  exclusionEmails <- c(usersToExclude$Email, oldAdminEmails)

  ######################################
  ###           Tomcat Logs          ###
  ######################################

  # ---- get logs ----
  # To determine how ImmuneSpaceR has been used, we parse the server logs. Since the server logs
  # all types of GET / POSTS and other requests, there is a fair amount of filtering that must
  # be done to find the requests that help us understand usage.
  #
  # These logs are created by Tomcat (server software) and written out to `/labkey/apps/tomcat/logs/`
  # on the webserve machine. Since they cannot be accessed by the Rserve, we need to copy them to
  # `/share` by setting up a cron job on `wsP/T` as `immunespace`.
  #
  # crontab -e
  # Add this line
  # 00 0-23/6 * * * rsync -a -v /labkey/apps/tomcat/logs/localhost_access_log.* /share/tomcat-logs/
  # This will sync logs to `/share/tomcat-logs/` every six hour.
  read_log2 <- function(date, exclusionEmails) {

    if (Sys.info()["nodename"] == "ImmuneTestRserve2" && date < "2017-09-22") {
      file_name <- paste0("/share/tomcat-logs/localhost_access_log..", date, ".txt")
      file_name_m <- paste0("/share/tomcat-logs/modified/localhost_access_log..", date, ".txt")
    } else {
      file_name <- paste0("/share/tomcat-logs/localhost_access_log.", date, ".txt")
      file_name_m <- paste0("/share/tomcat-logs/modified/localhost_access_log.", date, ".txt")
    }

    if (file.exists(file_name)) {
      # 1. Try reading unmodified logs
      tried <- try(
        readr::read_log(file = file_name,
                        col_types = readr::cols(.default = readr::col_character()))
      )

      # 2. If original errors out due to NULL values, create modified without NULL and
      # try reading that. Note that if tried does not have a "try-error" a list is returned
      # and to avoid warnings the `any` fn is used.
      if (any(class(tried) == "try-error")) {
        if (!file.exists(file_name_m)) {
          original <- file(file_name, "r")
          modified <- file(file_name_m, "w")
          lines <- readLines(original, skipNul = TRUE)
          writeLines(lines, modified)
          close(original)
          close(modified)
        }
        tried <- try(
          readr::read_log(file = file_name_m,
                          col_types = cols(.default = readr::col_character()))
        )
      }

      # 3. Parse
      if (!any(class(tried) == "try-error")) {
        if (nrow(tried) > 0) {

          # delete if the log is still being modfied
          if (date == as.POSIXct(Sys.Date())) {
            if (file.exists(file_name_m)) {
              file.remove(file_name_m)
            }
          }

          tried <- data.table(tried)
          tried <- tried[ !X12 %in% exclusionEmails & X6 == 200,
                          c("date", "study", "schema", "query") :=
                            list(date,
                                 stringr::str_extract(X5, "SDY\\d+|IS\\d+|Lyoplate"),
                                 grepl("schemaName=study?", X5),
                                 stringr::str_extract(X5, "(?<=queryName=)\\w+"))]
        } else {
          NULL
        }
      } else {
        NULL
      }
    } else {
      NULL
    }
  }

  logs_list <- lapply(seq(from, to, by = "1 day"), read_log2, exclusionEmails = exclusionEmails)
  logs_dt <- data.table::rbindlist(logs_list)

  # Create date from X4 rather than log file name b/c log file name leaves many NAs
  # and visualizations require a date
  tmp <- logs_dt$X4
  tmp <- regmatches(tmp, regexpr("\\d{2}/\\w{3}/\\d{4}", tmp))
  logs_dt$date2 <- as.POSIXct(tmp, format="%d/%b/%Y", tz="UTC")
  logs_dt <- logs_dt[ !X12 %in% exclusionEmails ]

  saveAndCleanUp(logs_dt, subdir, filename = "logs")
}
