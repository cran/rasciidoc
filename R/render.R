#' Render an \command{asciidoc} File
#'
#' This is the basic interface to \command{asciidoc}. Not more than a call to
#' \code{\link{system2}} and checks on \command{asciidoc} and
#' \command{source-highlight}.
#' You should usually not call it directly, see
#' \code{\link{render}} for a wrapper.
#'
#' @template cran
#' @param file_name The file to run \command{asciidoc} on.
#' @param ... arguments passed to \command{asciidoc} via \code{\link{system2}}.
#' @return \code{\link[base:invisible]{Invisibly}} \command{asciidoc}'a return
#' value.
#' @export
#' @seealso \code{\link{render}}
#' @examples
#' if (!is_windows()) { # CRAN windows complains about elapsed times
#'     wd <- file.path(tempdir(), "rasciidoc")
#'     dir.create(wd)
#'     file  <- system.file("files", "minimal", "knit.asciidoc",
#'                          package = "rasciidoc")
#'     file.copy(file, wd)
#'     rasciidoc::rasciidoc(file.path(wd, basename(file)),
#'                          write_to_disk = getOption("write_to_disk"),
#'                          "-b html")
#'     if (isTRUE(getOption("write_to_disk"))) {
#'         dir(wd, full.names = TRUE)
#'     } else {
#'         dir(tempdir(), full.names = TRUE)
#'     }
#'     unlink(wd, recursive = TRUE)
#' }
rasciidoc <- function(file_name, 
                      write_to_disk = getOption("write_to_disk"),
                      ...) {
    status <- 1
    checkmate::assert_logical(write_to_disk, null.ok = TRUE)
    checkmate::assert_file_exists(file_name)
    options <- list(...)
    if (isTRUE(write_to_disk)) {
        adoc_file <- file_name
    } else {
        # Do  _not_ mess with setting --out-file for asciidoc, as you would have
        # to parse all options to find possible settings for the backend or
        # such. Just get a temporary copy of the input file.
        if (identical(file.path(tempdir(), basename(file_name)),
                      normalizePath(file_name))) {
            # WARN: do not copy a file over itself, target would be empty
            adoc_file <- normalizePath(file_name)
        } else {
            message(hint_writing(file_name))
            file.copy(from = file_name, to = tempdir(), overwrite = TRUE)
            adoc_file <- file.path(tempdir(), basename(file_name))
        }
    }
    if (is_installed("source-highlight")) {
        report_sys_errors <- TRUE
    } else {
        report_sys_errors <- FALSE
        warning("Can't find program `source-highlight`. ",
                "Please install first",
                " (http://www.gnu.org/software/src-highlite/).\n", 
                "Deactivating system calls errors!")
    }
    if (is_installed("asciidoc")) {
        status <- system2("asciidoc", args = unlist(c(options, adoc_file)), 
                          stderr = report_sys_errors)
    } else {
        warning("Can't find program `asciidoc`. ",
                "Please install first (www.asciidoc.org).")
        python <- discover_python()
        if (is_installed(python)) {
            ad <- get_asciidoc()
            status <- system2(ad[["python_cmd"]],
                              args = unlist(c(ad[["asciidoc_source"]], 
                                              options, adoc_file)),
                              stderr = report_sys_errors)
        } else {
            throw(paste("Can't find `python`. ",
                        "Please install first (https://www.python.org/)."))
        }
    }
    return(invisible(status))
}

#' Spin or Knit and Render a `Rasciidoc` File
#'
#' Spin or Knit (if required) and render an `Rasciidoc` file.
#' @template cran
#' @inheritParams adjust_asciidoc_hooks
#' @param file_name The file to render.
#' @param knit Knit the file first using \code{\link[knitr:knit]{knitr::knit}}?
#' If set to \code{\link{NA}}, knitting is based on the file's contents or name.
#' Set to \code{\link{TRUE}}
#' to force knitting or to \code{\link{FALSE}}, to disable knitting.
#' @param envir The frame in which to render.
#' @param asciidoc_args arguments passed to \command{asciidoc} via
#' \code{\link{system2}}.
#' @param clean Remove temporary file(s)?
#' @param what What is to be rendered? \code{"all"} renders everything,
#' \code{"no_slides"} renders parts that are not meant for slides,
#' \code{"slides"} renders parts that are meant for slides. 
#' The defaults looks
#' for any in- or exclusion tagging and renders parts that are not meant for
#' slides if found any, else it renders everything.
#' @param ... Only there to register as vignette engine. Do not use!
#' @return The return value of \code{\link{rasciidoc}}.
#' @export
#' @seealso \code{\link{rasciidoc}}
#' @examples
#' if (!is_windows()) { # CRAN windows complains about elapsed times
#'     wd <- file.path(tempdir(), "rasciidoc")
#'     dir.create(wd)
#'     file  <- system.file("files", "minimal", "knit.Rasciidoc",
#'                          package = "rasciidoc")
#'     file.copy(file, wd)
#'     rasciidoc::render(file.path(wd, basename(file)), 
#'                       write_to_disk = getOption("write_to_disk"),
#'                       asciidoc_args = "-b slidy")
#'     if (isTRUE(getOption("write_to_disk"))) {
#'         dir(wd, full.names = TRUE)
#'     } else {
#'         dir(tempdir(), full.names = TRUE)
#'     }
#'     unlink(wd, recursive = TRUE)
#' }
render <- function(file_name, knit = NA,
                   write_to_disk = getOption("write_to_disk"),
                   envir = parent.frame(),
                   hooks = c("message", "error", "warning"),
                   replacement = "source", asciidoc_args = NULL,
                   what = c("auto", "all", "no_slides", "slides"),
                   clean = FALSE, ...) {
    status <- 1
    checkmate::assert_file_exists(file_name)
    checkmate::qassert(knit, "b1")
    checkmate::assert_environment(envir)
    checkmate::assert_character(hooks, null.ok = TRUE)
    checkmate::assert_character(replacement, null.ok = TRUE)
    checkmate::qassert(clean, "B1")
    checkmate::assert_character(what, null.ok = FALSE)
    what <- match.arg(what)
    on.exit(if (isTRUE(clean)) file.remove(excerpt, adoc))
    if (what == "auto") {
        lines <- readLines(file_name)
        if (any(grepl("^// *(begin|end)_only_slide", lines),
                grepl("// *slide_only", lines),
                grepl("// *no_slide", lines)))
            what <- "no_slides"
    }

    excerpt <- switch(what,
                      "slides" = 
                          excerpt_slides(file_name, 
                                         write_to_disk = write_to_disk),
                      "no_slides" = 
                          excerpt_no_slides(file_name, 
                                            write_to_disk = write_to_disk),
                      file_name)


    tryCatch(
             adoc <- run_knitr(file_name = excerpt,
                               knit = knit, envir = envir,
                               hooks = hooks, replacement = replacement,
                               write_to_disk = write_to_disk),
             error = function(e) throw(e)

             )
    status <- rasciidoc(file_name = adoc, 
                        write_to_disk = write_to_disk, asciidoc_args)
    return(status)
}
