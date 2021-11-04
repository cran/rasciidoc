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
#' # CRAN complains about elapsed times
#' if (fritools::is_running_on_fvafrcu_machines()) {
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
                   replacement = "source", asciidoc_args = "-b html",
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
