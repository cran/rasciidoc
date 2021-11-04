#' Render an \command{asciidoc} File
#'
#' This is the basic interface to \command{asciidoc}. Not more than a call to
#' \code{\link{system2}} and checks on \command{asciidoc} and
#' \command{source-highlight}.
#' You should usually not call it directly, see
#' \code{\link{render}} for a wrapper.
#'
#' @template cran
#' @param enforce_requirements Should system requirements be enforced (CRAN
#' requires packages to pass checks if system requirements (external commands)
#' are not met)? Set to \code{\link{TRUE}} to enforce.
#' @param file_name The file to run \command{asciidoc} on.
#' @param ... arguments passed to \command{asciidoc} via \code{\link{system2}}.
#' @return \code{\link[base:invisible]{Invisibly}} \code{\link{TRUE}} or
#' \code{\link{FALSE}}, depending on success.
#' @export
#' @seealso \code{\link{render}}
#' @examples
#' # CRAN complains about elapsed times
#' if (fritools::is_running_on_fvafrcu_machines()) {
#'     wd <- file.path(tempdir(), "rasciidoc")
#'     dir.create(wd)
#'     file  <- system.file("files", "minimal", "knit.asciidoc",
#'                          package = "rasciidoc")
#'     file.copy(file, wd)
#'     rasciidoc::rasciidoc(file_name = file.path(wd, basename(file)),
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
                      ...,
                      write_to_disk = getOption("write_to_disk"),
                      enforce_requirements = getOption("enforce_requirements")
                      ) {
    status <- FALSE
    msg <- NULL
    checkmate::assert_logical(write_to_disk, null.ok = TRUE)
    checkmate::assert_file_exists(file_name)
    options <- list(...)
    #% Deal with temporary or persistant files
    if (isTRUE(write_to_disk)) {
        adoc_file <- file_name
    } else {
        # This is forced by CRAN. Nobody wants this.

        # Do  _not_ mess with setting --out-file for asciidoc, as you would have
        # to parse all options to find possible settings for the backend or
        # such. Just get a temporary copy of the input file.

        # windows potentially long and abbreviated paths, so we need to
        # normalize them, else they will produce an empty file.
        # As occurred on CRAN windows (winbuilder).
        if (identical(normalizePath(file.path(tempdir(), basename(file_name))),
                      normalizePath(file_name))) {
            # WARN: do not copy a file over itself, target would be empty
            adoc_file <- normalizePath(file_name)
        } else {
            message(hint_writing(file_name))
            file.copy(from = file_name, to = tempdir(), overwrite = TRUE)
            adoc_file <- file.path(tempdir(), basename(file_name))
        }
    }
    python <- discover_python(stop_on_error = FALSE)
    if (fritools::is_installed(python)) {
        #% Check for source-highlight
        if (!fritools::is_installed("source-highlight")) {
            msg <- c(msg,
                     paste0("Can't find program `source-highlight`. ",
                            "Please install first ",
                            "(http://www.gnu.org/software/src-highlite/)."))
            if (isTRUE(enforce_requirements)) {
                warning(paste(msg, collapse = "\n"))
            } else {
                message(paste(msg, collapse = "\n"))
            }
        }
        #% render the input file
        if (fritools::is_installed("asciidoc")) {
            status <- tryCatch(system2("asciidoc",
                                       args = unlist(c(options, adoc_file)),
                                       stderr = TRUE, stdout = TRUE
                                       ),
                               error = identity, warning = identity)

        } else {
            msg <- c(msg, paste0("Can't find program `asciidoc`. ",
                                 "Please install first ",
                                 "(http://www.asciidoc.org)."))
            if (isTRUE(enforce_requirements)) {
                warning(paste(msg, collapse = "\n"))
            } else {
                message(paste(msg, collapse = "\n"))
            }
            ad <- get_asciidoc()
            status <- tryCatch(system2(ad[["python_cmd"]],
                                       args = unlist(c(ad[["asciidoc_source"]],
                                                       options, adoc_file)),
                                       stderr = TRUE, stdout = TRUE
                                       ),
                               error = identity, warning = identity)

        }
        #% set return values, default was FALSE
        # warnings and errors get a FALSE, default output is provided
        condition <- inherits(status, "error") || inherits(status, "warning") ||
            isFALSE(status) # ie: nothing was done at all!
        if (isTRUE(condition)) {
            status <- FALSE
            status <- write_default_output(msg, adoc_file)
        }
        # with stderr = TRUE we get character(0) on success, don't know why, so
        # we set it to TRUE:
        if (fritools::is_of_length_zero(status, class = "character"))
            status <- TRUE
        # capture all other return values
        if (!is.logical(status)) {
            if (!fritools::is_installed("source-highlight")) {
                # missing source-highlight gives no warning or error that could
                # be caught but a string as return value
                status <- TRUE
            } else {
                status <- FALSE
            }
        }
    } else {
        msg <- c("Python is a system requirement, please install first.",
                 "No output created.")
        if (isTRUE(enforce_requirements)) {
            throw(paste(msg, collapse = "\n"))
        } else {
            message(paste(msg, collapse = "\n"))
            status <- write_default_output(msg, adoc_file)

        }
    }
    return(invisible(status))
}
