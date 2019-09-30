#' Title
#'
#' @param snippet_location
#' @param backup_infix
#' @param backup_timestamp_format
#'
#' @return
#'
#' @examples
get_backup_prefix <- function(snippet_location,
                              backup_infix = getOption(
                                "typewritr.backup.infix"
                              ),
                              backup_timestamp_format = getOption(
                                "typewritr.backup.timestamp.format"
                              )) {
  backup_timestamp_and_infix <-
    if (is.null(backup_timestamp_format) || backup_timestamp_format == "") {
      ""
    } else {
      Sys.time() %>%
        format(backup_timestamp_format) %>%
        paste0(backup_infix)
    }

  snippet_location %>%
    basename() %>%
    paste0(backup_infix) %>%
    paste0(backup_timestamp_and_infix)
}

#' Title
#'
#' @param backup
#' @param snippet_location
#'
#' @return
#'
#' @examples
get_snippet_location <- function(snippet_location = getOption(
                                     "typewritr.snippet.location"
                                   ),
                                   backup = FALSE) {
  snippet_location <-
    tryCatch({
      normalizePath(snippet_location)
    }, error = function(e) {
      stop(
        glue::glue(
          "Could not find snippet file at {snippet_location}. ",
          "Please check that the file exists, ",
          "or check getOption('typewritr.snippet.location')."
        )
      )
    })

  if (backup) {
    return(
      tempfile(
        pattern = snippet_location %>% get_backup_prefix(),
        tmpdir  = snippet_location %>% dirname()
      )
    )
  }

  snippet_location
}
