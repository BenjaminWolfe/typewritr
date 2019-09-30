#' Title
#'
#' @param label
#' @param character
#' @param infix
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
make_snippet <- function(label,
                         character,
                         infix = getOption("typewritr.infix"),
                         prefix = getOption("typewritr.prefix")) {
  tab <- "\t"
  glue::glue("snippet {prefix}{infix}{label}\n{tab}{character}")
}

#' Create a
#'
#' @param character_table
#' @param labels
#' @param characters
#' @param infixes
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
make_snippets <- function(character_table,
                          labels,
                          characters,
                          infixes = NULL,
                          prefix = getOption("typewritr.prefix")) {
  character_table %>%
    dplyr::mutate(
      snippet = make_snippet(
        label     = {{labels}},
        character = {{characters}},
        infix     = {{infixes}},
        prefix    = prefix
      )
    ) %>%
    pull(snippet)
}

#' Title
#'
#' Note that this function does not check if your snippets file
#' ends in a newline character.
#'
#' @param encoding
#' @param snippet_location
#'
#' @return
#' @export
#'
#' @examples
read_snippets <- function(encoding = getOption("typewritr.encoding"),
                          snippet_location = getOption(
                            "typewritr.snippet.location"
                          )) {
  readLines(
    get_snippet_location(snippet_location = snippet_location),
    encoding = encoding
  )
}

#' TODO: Maybe make another function for reading the snippet names!
#' That way we can watch out for conflicts.

#' Title
#'
#' @param snippet_location
#'
#' @return
#' @export
#'
#' @examples
backup_snippets <- function(
  snippet_location = getOption("typewritr.snippet.location")
) {
  file.copy(get_snippet_location(), get_snippet_location(backup = TRUE))
}

#' Title
#'
#' @param snippets
#' @param backed_up
#' @param force
#' @param snippet_location
#'
#' @return
#' @export
#'
#' @examples
write_snippets <- function(snippets,
                           backed_up = FALSE,
                           force = FALSE,
                           encoding = getOption("typewritr.encoding"),
                           snippet_location = getOption(
                             "typewritr.snippet.location"
                           )) {
  if (!backed_up && !force) {
    stop(glue::glue("It looks like {snippet_location} is not backed up."))
  }

  snippet_location <-
    snippet_location %>%
    get_snippet_location() %>%
    file(encoding = encoding)

  writeLines(snippets, snippet_location)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
register_snippets_from_table <- function(character_table,
                                         labels,
                                         characters,
                                         infixes = NULL,
                                         prefix = getOption("typewritr.prefix"),
                                         encoding = getOption(
                                           "typewritr.encoding"
                                         ),
                                         snippet_location = getOption(
                                           "typewritr.snippet.location"
                                         )) {
  current_snippets <-
    read_snippets(
      encoding         = encoding,
      snippet_location = snippet_location
    )

  new_snippets <-
    make_snippets(
      character_table  = character_table,
      labels           = {{labels}},
      characters       = {{characters}},
      infixes          = {{infixes}},
      prefix           = prefix
    )

  backed_up <-
    backup_snippets(
      snippet_location = snippet_location
    )

  write_snippets(
    c(current_snippets, new_snippets),
    backed_up          = backed_up,
    snippet_location   = snippet_location,
    encoding           = encoding
  )
}

#' TODO: Write a function for registering them from just a vector.
#' And then maybe make it S3 so it's way easier to use.

get_greeks <- function(greek_infix = getOption("typewritr.greek.infix")) {
  greek_letters %>%
    tidyr::gather(
      key   = "case",
      value = "character",
      -label
    ) %>%
    dplyr::filter(
      !is.na(character)
    ) %>%
    dplyr::mutate(
      infix = dplyr::if_else(case == "uppercase", greek_infix, "")
    )
}

register_greeks <- function(greek_infix = getOption("typewritr.greek.infix"),
                            prefix = getOption("typewritr.prefix"),
                            encoding = getOption("typewritr.encoding"),
                            snippet_location = getOption(
                              "typewritr.snippet.location"
                            )) {
  get_greeks(greek_infix = getOption("typewritr.greek.infix")) %>%
    register_snippets_from_table(
      labels           = label,
      characters       = character,
      infixes          = infix,
      prefix           = prefix,
      encoding         = encoding,
      snippet_location = snippet_location
    )
}

#' TODO: The damn encoding is killing me here. I get U+ and angle brackets.
