.onAttach <- function(libname, pkgname) {

  # consider if there's anything I want to add here
}

.onLoad <- function(libname, pkgname) {

  # adapted from http://r-pkgs.had.co.nz/r.html
  set_these <- function(ops_list) {
    op <- options()
    toset <- !(names(ops_list) %in% names(op))
    if(any(toset)) options(ops_list[toset])
  }
  set_these(
    ops_list = list(
      typewritr.prefix = "lkj",
      typewritr.infix = ".",
      typewritr.snippet.location = "~/.R/snippets/r.snippets",
      typewritr.encoding = "UTF-8"
    )
  )
  set_these(
    ops_list = list(
      typewritr.greek.infix = getOption("typewritr.infix"),
      typewritr.backup.infix = getOption("typewritr.infix")
    )
  )
  set_these(
    ops_list = list(
      typewritr.backup.timestamp.format = paste(
        "%Y%m%d",
        "%H%M%S",
        sep = getOption("typewritr.backup.infix")
      )
    )
  )

  invisible()
}



typewritr.backup.timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
