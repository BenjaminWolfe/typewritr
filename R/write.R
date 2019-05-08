#' Write out a list of words
#'
#' @param word_list  Character vector. Words to be listed out.
#' @param quoted     Logical. If TRUE, will wrap each word in quotation marks.
#' @param human      Logical. If FALSE, will leave out the final conjunction.
#' @param separator  Character. What punctuation do you want between elements?
#'                   Defaults to semicolon if any element contains a comma,
#'                   comma otherwise.
#' @param placement  Character. If "US", will place commas and periods inside
#'                   any quotation marks (and other punctuation outside them).
#'                   If "international", will place all punctuation outside
#'                   quotation marks. Defaults to "US" if \code{Sys.getlocale}
#'                   places you in the US and human is set to TRUE.
#' @param oxford     Logical. Use an Oxford comma? Defaults to TRUE.
#'                   Parameter is ignored if separator is not a comma.
#' @param quote_type Character. "double" for double quotes, "single" for single.
#' @param curly      Logical. TRUE for Unicode curly quotes,
#'                   FALSE for straight ASCII quotes.
#'                   Defaults to FALSE if human == FALSE, TRUE otherwise.
#' @param collapse   Character. What final conjunction or sign do you want?
#'                   "and", an ampersand, a plus sign, an "or", an "and/or"...?
#' @param ending     Character (e.g. period, comma, or zero-length string).
#'                   What character if any do you need inside the final
#'                   quotation mark, or at the end of the function"s output?
#'
#' @return Character. A pretty, human-readable list.
#'
#' @importFrom dplyr %>%
#' @importFrom glue glue
#' @export
#'
#' @examples
#' write_out("apples")
#' write_out(c("apples", "oranges"))
#' write_out(c("apples", "oranges"), quoted = TRUE)
#' write_out(
#'   c("hello, my friend", "welcome", "enter"),
#'   quoted = TRUE,
#'   ending = "."
#' )
#' write_out(c("apples", "oranges"), quoted = TRUE, quote_type = "single")
#' write_out(
#'   c("apples", "oranges"),
#'   quoted = TRUE,
#'   quote_type = "single",
#'   curly = FALSE
#' )
#' write_out(c("apples", "oranges", "bananas"))
#' write_out(c("apples", "oranges", "bananas"), quoted = TRUE, ending = ".")
#' write_out(c("apples", "oranges", "bananas"), oxford = FALSE)
#' write_out(c("apples", "oranges", "bananas"), collapse = "or")
#' write_out(c("apples", "oranges", "bananas"), human = FALSE)
#' write_out(character(0))
write_out <- function(word_list,
                      quoted = FALSE,
                      human = TRUE,
                      separator = ifelse(sum(grepl(",", word_list)), ";", ","),
                      placement = ifelse(
                        human & grepl("United States", Sys.getlocale()),
                        "US",
                        "international"
                      ),
                      oxford = TRUE,
                      quote_type = c("double", "single"),
                      curly = human,
                      collapse = c("and", "&", "+", "or", "and/or"),
                      ending = c("", ".", ",")) {

  # error handling and prep ----------------------------------------------------
  # handle accidental factor or numerical "word" lists
  word_list <-
    if (class(word_list) == "factor") {
      as.character(word_list)
    } else if (class(word_list) != "character") {
      tryCatch({
        as.character(word_list)
      }, error = function(e) {
        stop("`word_list` must be a character vector.")
      })
    } else {
      word_list
    }

  # validate logical inputs
  if (class(quoted) != "logical") {
    stop("`quoted` must be TRUE, FALSE, or omitted.")
  }
  if (class(human) != "logical") {
    stop("`human` must be TRUE, FALSE, or omitted.")
  }
  if (class(oxford) != "logical") {
    stop("`oxford` must be TRUE, FALSE, or omitted.")
  }

  # coerce character vectors to single strings as needed
  separator <-
    if (class(separator) != "character") {
      stop("`separator` must be a character value.")
    } else {
      separator[1]
    }
  placement <-
    if (class(placement) != "character") {
      stop("`placement` must be a character value.")
    } else if (!(placement[1] %in% c("US", "international"))) {
      stop("`placement` must be either 'US' or 'international'.")
    } else {
      placement[1]
    }
  collapse <-
    if (class(collapse) != "character") {
      stop("`collapse` must be a character value.")
    } else {
      collapse[1]
    }
  ending <-
    if (class(ending) != "character") {
      stop("`ending` must be a character value.")
    } else {
      ending[1]
    }

  # combine "quote_type" and "curly" into a quote-wrapping function
  quote_wrap <-
    if (class(quote_type) != "character") {
      stop("`quotes` must be a character value.")
    } else if (!(quote_type[1] %in% c("double", "single"))) {
      stop("`quotes` must be either 'double' or 'single'.")
    } else if (class(curly) != "logical") {
      stop("`curly` must be TRUE, FALSE, or omitted.")
    } else {
      function(x) quotes(x, type = quote_type[1], curly = curly)
    }

  # punctuation and quotation marks --------------------------------------------
  # make a parallel vector of punctuation: commas, blanks, semicolons, periods
  # remove Oxford comma if necessary
  suffix <- character(length(word_list))
  if (length(suffix) == 1) {
    suffix <- ending
  } else if (length(suffix) == 2) {
    if (!human) {
      suffix <- c(separator, ending)
    } else {
      suffix <- c("", ending)
    }
  } else if (length(suffix) >= 3) {
    suffix[ length(suffix)] <- ending
    suffix[-length(suffix)] <- separator
    if (human & !oxford & separator == ",") {
      suffix[length(suffix) - 1] <- ""
    }
  }

  # wrap elements in quotes if requested, with punctuation appropriately placed
  # TODO: the looping here feels a little inelegant
  word_list <-
    if (!quoted) {
      paste0(word_list, suffix)
    } else {
      sapply(
        seq_along(word_list),
        function(i) {
          if (placement == "US" & suffix[i] %in% c(",", ".")) {
            quote_wrap(paste0(word_list[i], suffix[i]))
          } else {
            paste0(quote_wrap(word_list[i]), suffix[i])
          }
        }
      )
    }

  # finalizing and prettying up ------------------------------------------------
  # collapse string together, with final conjunction if requested
  response <-
    if (length(word_list) < 2) {
      word_list
    } else if (!human) {
      paste(word_list, collapse = " ")
    } else if (length(word_list) == 2) {
      paste(word_list, collapse = glue::glue(" {collapse} "))
    } else {
      paste(
        paste(word_list[-length(word_list)], collapse = " "),
        word_list[length(word_list)],
        sep = glue::glue(" {collapse} ")
      )
    }

  # if using curly quotes, switch to UTF-8 encoding; then return response.
  # TODO: FWIW, I"m really not sure I wrote examples for every test-worthy case.
  if (curly) {
    Encoding(response) <- "UTF-8"
  }
  response
}
