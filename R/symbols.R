#' Print an ellipsis
#'
#' @return Character. A UTF-8 ellipsis.
#' @export
#'
#' @examples
#' ellipsis()
ellipsis <- function() {
  response <- "\u2026"
  Encoding(response) <- "UTF-8"
  response
}

#' Print an em-dash
#'
#' @return Character. A UTF-8 em-dash.
#' @export
#'
#' @examples
#' em()
em <- function() {
  response <- "\u2014"
  Encoding(response) <- "UTF-8"
  response
}

#' Wrap text in em dashes
#'
#' @param ... Character. Text to go between em dashes,
#' and any other arguments to be passed to \code{\link[glue]{glue}}.
#'
#' @return Character. String surrounded in em dashes.
#'
#' \code{ems} is a wrapper for \code{\link[glue]{glue}},
#' so you can use curly bracket notation and multiple character strings
#' just like with \code{\link[glue]{glue}}.
#'
#' @export
#' @examples
#' ems("hello world")
#' ems("hello ", "world")
#' fname <- "Darth"
#' lname <- "Vader"
#' ems("hello there {fname} {lname}")
#' ems(
#'   "hello {fname} {lname}, ",
#'   "I says to him"
#' )
ems <- function(...) {
  dash <- "\u2014"
  response <- glue::glue(dash, glue::glue(...), dash)
  Encoding(response) <- "UTF-8"
  response
}

#' Print an en-dash
#'
#' @param from Character. If the en-dash separates two strings, the first one.
#' @param to Character. If the en-dash separates two strings, the second one.
#' @param format Function. If provided, formats \code{from} and \code{to}.
#' @param expand Logical. Add spaces?
#'
#' @return Character. The from and to strings, separated by an en-dash (UTF-8).
#'
#' \code{en} is a wrapper for \code{\link[glue]{glue}},
#' so you can use curly bracket notation in \code{from} and \code{to},
#' just like with \code{\link[glue]{glue}}.
#'
#' @export
#' @examples
#' library(lubridate)
#' library(scales)
#' en()
#' en(today(), today() %m+% months(1))
#' en(today(), today() %m+% months(1), format = date_format("%B %e, %Y"))
#' en(1234, 5678)
#' en(1234, 5678, format = comma)
#' en(1234, 5678, format = comma, expand = TRUE)
en <- function(from = "", to = "", format = NA, expand = F) {
  dash <- "\u2013"
  if (class(format) == "function") {
    # if from and to are zero-length strings, leave them as is.
    # but check classes first so we don"t inadvertently convert them to strings
    from <-
      if ("character" %in% class(from)) {
        if (from == "") {
          ""
        } else {
          format(glue::glue(from))
        }
      } else {
        format(from)
      }
    to <-
      if ("character" %in% class(to)) {
        if (to == "") {
          ""
        } else {
          format(glue::glue(to))
        }
      } else {
        format(to)
      }
  }
  response <-
    if (expand) {
      glue::glue("{from} {dash} {to}")
    } else {
      glue::glue("{from}{dash}{to}")
    }
  Encoding(response) <- "UTF-8"
  response
}

#' Print an enye
#'
#' @param uppercase Logical. Use TRUE for uppercase, FALSE for lowercase.
#'
#' @return Character. A UTF-8 enye.
#' @export
#'
#' @examples
#' enye()
#' enye(uppercase = TRUE)
enye <- function(uppercase = FALSE) {
  response <- ifelse(uppercase, "\u00d1", "\u00f1")
  Encoding(response) <- "UTF-8"
  response
}

#' Print a plus/minus character.
#'
#' @param margin The number after the plus/minus. Can be a character.
#' @param x The number, if any, before the plus/minus. Can be a character.
#' @param format Function. If provided, formats \code{x}.
#' @param margin_format Function. If provided, formats \code{margin}.
#' @param parentheses Logical. Should \code{margin} be wrapped in parentheses?
#' @param contract Logical. If \code{x} is provided, should spaces be removed?
#'
#' @return Character. \code{x} plus or minus a \code{margin} (UTF-8).
#'
#' \code{ish} uses \code{\link[glue]{glue}},
#' so you can use curly bracket notation in \code{x} and \code{margin},
#' just like with \code{\link[glue]{glue}}.
#'
#' @export
#' @examples
#' library(scales)
#' ish()
#' ish("5%")
#' ish(.05, format = percent_format(accuracy = 1))
#' ish(
#'   x = 100,
#'   margin = .05,
#'   margin_format = percent_format(accuracy = 1),
#'   parentheses = TRUE
#' )
#' ish(
#'   x = 1e3,
#'   margin = .05,
#'   format = comma,
#'   margin_format = percent_format(accuracy = 1),
#'   parentheses = TRUE
#' )
#' ish(x = 5, margin = 5)
#' ish(x = 5, margin = 5, contract = TRUE)
ish <- function(margin = "",
                x = "",
                format = NA,
                margin_format = format,
                parentheses = FALSE,
                contract = FALSE) {
  symbol <- "\u00b1" # Obiwan :)
  if (class(format) == "function") {
    x <-
      # if x is a zero-length string, leave it as is.
      # but check its class 1st so we don"t inadvertently convert it to a string
      if (class(x) == "character") {
        if (x == "") {
          ""
        } else {
          format(glue::glue(x))
        }
      } else {
        format(x)
      }
  }
  if (class(margin_format) == "function") {
    margin <-
      # if margin is a zero-length string, leave it as is.
      # but check its class 1st so we don"t inadvertently convert it to a string
      if (class(margin) == "character") {
        if (margin == "") {
          ""
        } else {
          margin_format(glue::glue(margin))
        }
      } else {
        margin_format(margin)
      }
  }
  response <-
    if (x == "" & margin == "") {
      symbol                               # just the +/- character itself
    } else if (x == "" & !parentheses) {
      glue::glue("{symbol}{margin}")       # e.g. +/-5
    } else if (x == "" & parentheses) {
      glue::glue("({symbol}{margin})")     # e.g. (+/-5)
    } else if (x != "" & parentheses) {
      glue::glue("{x} ({symbol}{margin})") # e.g. 5 (+/-5)
    } else if (x != "" & !parentheses & !contract) {
      glue::glue("{x} {symbol} {margin}")  # e.g. 5 +/- 5
    } else if (x != "" & !parentheses & contract) {
      glue::glue("{x}{symbol}{margin}")    # e.g. 5+/-5
    } else {
      stop("not sure what to do with this combination of x, symbol, & margin.")
    }
  Encoding(response) <- "UTF-8"
  response
}

#' Print an obelus
#'
#' @param numerator Character. If the obelus separates two strings,
#' the first one.
#' @param denominator Character. If the obelus separates two strings,
#' the second one.
#' @param format Function. If provided,
#' formats \code{numerator} and \code{denominator}.
#' @param contract Logical. Remove spaces?
#'
#' @return Character. The numerator and denominator (if provided),
#' formatted if necessary, separated by an obelus (UTF-8).
#'
#' \code{obelus} is a wrapper for \code{\link[glue]{glue}},
#' so you can use curly bracket notation
#' in \code{numerator} and \code{denominator},
#' just like with \code{\link[glue]{glue}}.
#'
#' @export
#' @examples
#' library(scales)
#' obelus()
#' obelus(1234, 5678)
#' obelus(1234, 5678, format = comma)
#' obelus(1234, 5678, format = comma, contract = TRUE)
obelus <- function(numerator = "",
                   denominator = "",
                   format = NA,
                   contract = F) {
  symbol <- "\u00F7"
  if (class(format) == "function") {
    # if numerator and denominator are zero-length strings, leave them as is.
    # but check classes first so we don"t inadvertently convert them to strings
    numerator <-
      if (class(numerator) == "character") {
        if (numerator == "") {
          ""
        } else {
          format(glue::glue(numerator))
        }
      } else {
        format(numerator)
      }
    denominator <-
      if (class(denominator) == "character") {
        if (denominator == "") {
          ""
        } else {
          format(glue::glue(denominator))
        }
      } else {
        format(denominator)
      }
  }
  response <-
    if (numerator == "" & denominator == "") {
      symbol
    } else if (contract) {
      glue::glue("{numerator}{symbol}{denominator}")
    } else {
      glue::glue("{numerator} {symbol} {denominator}")
    }
  Encoding(response) <- "UTF-8"
  response
}

#' Print a paragraph symbol
#'
#' @param paragraph_number Numeric or character value to print after the symbol.
#' @param nbsp Logical. Include a non-breaking space between the symbol
#' and the paragraph number? Defaults to TRUE if there is a paragraph number.
#'
#' @return Character (UTF-8): A pilcrow and, if indicated, a paragraph number.
#'
#' For multiple paragraphs, two pilcrows are traditionally used.
#' Accordingly, the function returns two pilcrows if \code{paragraph_number}
#' includes an en-dash, a comma, or (not recommended) a hyphen,
#' or if \code{paragraph_number} is a vector of length greater than one.
#' In that last scenario, paragraphs will be separated by commas and spaces.
#'
#' @export
#'
#' @examples
#' pilcrow()
#' pilcrow(8)
#' pilcrow("8")
#' pilcrow("8", nbsp = FALSE)
#' pilcrow(en("8", "9"))
#' pilcrow("8 - 9") # hyphen not recommended; use en-dash for ranges
#' pilcrow("8, 22")
#' pilcrow(c("8", "22"))
pilcrow <- function(paragraph_number = "", nbsp = paragraph_number != "") {
  symbol <- "\u00B6"
  if (
    length(paragraph_number) > 1
    | grepl("[\u2014,\\-]", paragraph_number[1])
  ) {
    symbol <- paste(rep(symbol, 2), collapse = "")
  }
  if (length(paragraph_number) > 1) {
    paragraph_number <- paste(paragraph_number, collapse = ", ")
  }
  space <- ifelse(nbsp, "\u00A0", "")
  response <- paste0(symbol, space, paragraph_number)
  Encoding(response) <- "UTF-8"
  response
}

#' Wrap text in curly quotes
#'
#' @param ... Character. Text to go between curly quotes,
#' and any other arguments to be passed to \code{\link[glue]{glue}}.
#' @param type Character. Type of quotes: "single" or "double".
#' @param curly Logical. Set to FALSE for straight quotes.
#'
#' @return Character. String surrounded in quotation marks.
#'
#' \code{quotes} is a wrapper for \code{\link[glue]{glue}},
#' so you can use curly bracket notation and multiple character strings
#' just like with \code{\link[glue]{glue}}.
#'
#' @export
#' @examples
#' library(glue)
#' quotes("hello world")
#' quotes("hello world", type = "single")
#' quotes("hello ", "world", type = "single")
#' fname <- glue("I{enye()}igo")
#' lname <- "Montoya"
#' quotes("hello, my name is {fname} {lname}", type = "single")
#' quotes(
#'   "Hello, my name is {fname} {lname}. ",
#'   "You killed my father. Prepare to die."
#' )
#' quotes(
#'   "Hello, my name is {fname} {lname}. ",
#'   "I'm boring, and I use straight quotes.",
#'   curly = FALSE
#' )
quotes <- function(..., type = c("double", "single"), curly = TRUE) {
  if (class(type) != "character") {
    stop("`type` must be either 'single' or 'double'")
  } else if (!(type[1] %in% c("single", "double"))) {
    stop("`type` must be either 'single' or 'double'")
  }
  qts <- switch(
    type[1],
    "single" = if (curly) c("\u2018", "\u2019") else rep("'", 2),
    "double" = if (curly) c("\u201C", "\u201D") else rep('"', 2)
  )
  response <- glue::glue(qts[1], glue::glue(...), qts[2])
  Encoding(response) <- "UTF-8"
  response
}

#' Add plus signs or proper minus signs
#'
#' The true minus sign (UTF 2212) -- neither an em dash, nor an en dash,
#' nor the usual hyphen-minus -- is highly underrated.
#' It just makes everything look better!
#' This function builds on the formatting functions
#' from the \code{scales} package by replacing the hyphen-minus
#' with a true minus, and adding a plus sign if desired.
#'
#' @param ... Numeric vector and any other arguments passed to format.
#' @param format Function that formats a numeric vector,
#' such as \code{scales::number}, \code{scales::comma}, \code{scales::percent},
#' or a function returned by \code{scales::unit_format}
#' (all of which are documented at \code{\link[scales]{number_format}}).
#' @param plus_sign Logical. Should positive values start with plus signs?
#'
#' @return A \code{UTF-8} character vector
#' @import scales
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' signs(seq(-5, 5))
#' signs(seq(-5, 5), plus_sign = TRUE)
#' signs(seq(-5, 5) / 100, format = scales::percent, plus_sign = TRUE)
signs <- function(..., format = scales::number, plus_sign = FALSE) {
  response <- format(...) %>% {
    ifelse(
      . > 0 & plus_sign,
      as.character(.) %>% paste0("+", .),
      as.character(.) %>% sub("-", "\u2212", x = ., fixed = TRUE)
    )
  }
  Encoding(response) <- "UTF-8"
  response
}

#' Print a section symbol
#'
#' @param section_number Numeric or character value to print after the symbol.
#' Can also be a vector of such values.
#' @param nbsp Logical. Include a non-breaking space between the symbol
#' and the section number? Defaults to TRUE if there is a section number.
#'
#' @return Character (UTF-8): A silcrow and, if indicated, a section number.
#'
#' For multiple sections, two silcrows are traditionally used.
#' Accordingly, the function returns two silcrows if \code{section_number}
#' includes an en-dash, a comma, or (not recommended) a hyphen,
#' or if \code{section_number} is a vector of length greater than one.
#' In that last scenario, sections will be separated by commas and spaces.
#'
#' @export
#'
#' @examples
#' silcrow()
#' silcrow(8.1)
#' silcrow("8.1.1")
#' silcrow("8.1.1", nbsp = FALSE)
#' silcrow(en("8.1", "8.2"))
#' silcrow("8.1 - 8.2") # hyphen not recommended; use en-dash for ranges
#' silcrow("8.1, 8.7")
#' silcrow(c("8.1", "8.7"))
silcrow <- function(section_number = "",
                    nbsp = section_number != "") {
  symbol <- "\u00A7"
  if (length(section_number) > 1 | grepl("[\u2014,\\-]", section_number[1])) {
    symbol <- paste(rep(symbol, 2), collapse = "")
  }
  if (length(section_number) > 1) {
    section_number <- paste(section_number, collapse = ", ")
  }
  space <- ifelse(nbsp, "\u00A0", "")
  response <- paste0(symbol, space, section_number)
  Encoding(response) <- "UTF-8"
  response
}

#' Wrap text in backticks
#'
#' @param ... Character. Text to go between backticks,
#' and any other arguments to be passed to \code{\link[glue]{glue}}.
#' @param r Logical. Add \code{r } at the beginning, for R Markdown?
#'
#' @return Character. String surrounded in backticks.
#'
#' \code{ticks} is a wrapper for \code{\link[glue]{glue}},
#' so you can use curly bracket notation and multiple character strings
#' just like with \code{\link[glue]{glue}}.
#'
#' \code{ticks} is particularly useful for R Markdown,
#' where you don"t want to use a backtick
#' because it would end the inline code block.
#'
#' Sometimes you want to actually show an inline R code block, as text
#' (instead of evaluating it).
#' For that we"ve included a handy shortcut for adding the initial \code{r }.
#'
#' @export
#' @examples
#' ticks("foobar")
#' ticks("foo", "bar")
#' word1 <- "foo"
#' word2 <- "bar"
#' ticks("{word1}{word2} = 'stupid'", r = TRUE)
#' word3 <- "random"
#' ticks(
#'   "c(",
#'   "{word1}{word2} = 'surprisingly', ",
#'   "{word3} = 'painless'",
#'   ")",
#'   r = TRUE
#' )
ticks <- function(..., r = FALSE) {
  if (class(r) != "logical") {
    stop("`r` must be either TRUE or FALSE.")
  }
  r <- ifelse(r, "r ", "")
  backtick <- "\u0060"
  glue::glue(backtick, r, glue::glue(...), backtick)
}

#' Print a multiplication symbol
#'
#' @param multiplier Character. If the multiplication symbol
#' separates two strings, the first one.
#' @param multiplicand Character. If the multiplication symbol
#' separates two strings, the second one.
#' @param format Function. If provided,
#' formats \code{multiplier} and \code{multiplicand}.
#' @param contract Logical. Remove spaces?
#'
#' @return Character. The multiplier and multiplicand (if provided),
#' formatted if necessary, separated by a multiplication symbol (UTF-8).
#'
#' \code{times} is a wrapper for \code{\link[glue]{glue}},
#' so you can use curly bracket notation
#' in \code{multiplier} and \code{multiplicand},
#' just like with \code{\link[glue]{glue}}.
#'
#' @export
#' @examples
#' library(scales)
#' times()
#' times(1234, 5678)
#' times(1234, 5678, format = comma)
#' times(1234, 5678, format = comma, contract = TRUE)
times <- function(multiplier = "",
                  multiplicand = "",
                  format = NA,
                  contract = F) {
  symbol <- "\u00D7"
  if (class(format) == "function") {
    # if multiplier and denominator are zero-length strings, leave them as is.
    # but check classes first so we don"t inadvertently convert them to strings
    multiplier <-
      if (class(multiplier) == "character") {
        if (multiplier == "") {
          ""
        } else {
          format(glue::glue(multiplier))
        }
      } else {
        format(multiplier)
      }
    multiplicand <-
      if (class(multiplicand) == "character") {
        if (multiplicand == "") {
          ""
        } else {
          format(glue::glue(multiplicand))
        }
      } else {
        format(multiplicand)
      }
  }
  response <-
    if (multiplier == "" & multiplicand == "") {
      symbol
    } else if (contract) {
      glue::glue("{multiplier}{symbol}{multiplicand}")
    } else {
      glue::glue("{multiplier} {symbol} {multiplicand}")
    }
  Encoding(response) <- "UTF-8"
  response
}
