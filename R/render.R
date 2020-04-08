.onAttach <- function(...) {
    if (!isTRUE(getOption("write_to_disk")))
        packageStartupMessage("\n", hint_writing())
}

is_installed <- function(program) {
    is_installed <- nchar(Sys.which(program)) > 0
    is_installed <- unname(is_installed)
    return(is_installed)
}

hint_writing <- function(path = "the input file") {
    t <- paste0("Due to the CRAN policy of not writing \"anywhere else on the",
            " file system apart from the R session's temporary directory\"",
            " we work on a temporary copy of ", path, ".",
            "\n", "Thus all internal sourcing and internal links will be",
            " broken and any output is written to ", tempdir(), ".",
            " Set the option \"write_to_disk\" to TRUE (using",
            "\n", "\toptions(\"write_to_disk\" = TRUE)", "\n",
            ") to bypass this. You may want to include the above line into",
            " your ~/.Rprofile.")
    return(t)
}

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
#' wd <- file.path(tempdir(), "rasciidoc")
#' dir.create(wd)
#' file  <- system.file("files", "minimal", "knit.asciidoc",
#'                      package = "rasciidoc")
#' file.copy(file, wd)
#' rasciidoc::rasciidoc(file.path(wd, basename(file)), "-b docbook")
#' if (isTRUE(getOption("write_to_disk"))) {
#'     dir(wd, full.names = TRUE)
#' } else {
#'     dir(tempdir(), full.names = TRUE)
#' }
#' unlink(wd, recursive = TRUE)
rasciidoc <- function(file_name, 
                      write_to_disk = getOption("write_to_disk"),
                      ...) {
    status <- 1
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
    if (!is_installed("source-highlight"))
        warning("Can't find program `source-highlight`. ",
                "Please install first",
                " (http://www.gnu.org/software/src-highlite/).")
    if (is_installed("asciidoc")) {
        status <- system2("asciidoc",
                          args = unlist(c(options, adoc_file)))
    } else {
        warning("Can't find program `asciidoc`. ",
                "Please install first (www.asciidoc.org).")
        if (is_installed("python")) {
            ad <- get_asciidoc()
            system2(ad[["python_cmd"]],
                    args = unlist(c(ad[["asciidoc_source"]], 
                                    options, adoc_file)))
        } else {
            throw(paste("Can't find `ptyhon`. ",
                        "Please install first (https://www.python.org/)."))
        }
    }
    return(invisible(status))
}


get_asciidoc <- function() {
    local_asciidoc_path <- file.path(tempdir(), "asciidoc")
    local_asciidoc_path <- normalizePath(local_asciidoc_path, mustWork = FALSE)
    config_file <- file.path(local_asciidoc_path, "rasciidoc_config.R")

    if (file.exists(config_file)) {
        source(config_file, local = TRUE)
    } else {
    unlink(local_asciidoc_path, recursive = TRUE)
    dir.create(local_asciidoc_path)
    if (is_installed("python")) {
        python <- Sys.which("python")
        python_version <- sub("Python ", "",
                              system2(python, "--version",
                                      stderr = TRUE, stdout = TRUE))
        python_version <- as.character(package_version(python_version)[[c(1, 
                                                                          1)]])
        if (python_version == "3" && is_installed("python2")) {
            # asciidoc was origninally written in python2, so python2 wins.
            # TODO: if python2 is available, but the version is not sufficient,
            # should I fall back to python3?
            python <- Sys.which("python2")
            python_version <- "2"
        }
        switch(python_version,
               "2" = repo <- git2r::clone(url = "https://github.com/asciidoc/asciidoc", 
                                          local_path = local_asciidoc_path),
               "3" = repo <- git2r::clone(url = "https://github.com/asciidoc/asciidoc-py3", 
                                          local_path = local_asciidoc_path),
               throw(paste("Could not find python version 2",
                           "nor python version 3."))
               )

    } else {
            throw("Python is a system requirement.")
    }
    dir(local_asciidoc_path)

    asciidoc_source <- file.path(local_asciidoc_path, "asciidoc.py") 
    py_version <- sub("Python ", "",
                      system2(python, "--version",
                              stderr = TRUE, stdout = TRUE))
    required <- grep("^MIN_PYTHON_VERSION", readLines(asciidoc_source), 
                     value = TRUE)
    min_py_version <- sub("'.*", "", sub("^MIN_PYTHON_VERSION = '", "",
                                         required))
    is_sufficient <- utils::compareVersion(py_version, min_py_version) >= 0
    if (!isTRUE(is_sufficient)) 
        throw(paste0("Could find not find python >= ", min_py_version, "."))
    res <- list("python_cmd" = python, 
                "asciidoc_source" = asciidoc_source
                )
    dump("res", config_file)
    }
    return(res)
}



run_knit <- function(file_name, knit = NA,
                     write_to_disk = getOption("write_to_disk"),
                     envir = parent.frame()) {
    if (is.na(knit)) {
        r_code_pattern <- "//begin.rcode"
        if (any(grepl(r_code_pattern, readLines(file_name)))) {
            knit <- TRUE
            warning("Setting option knit to TRUE based on the file contents!")
        }
    }
    if (is.na(knit)) {
        if (grepl("\\.R.*$", file_name)) {
            knit <- TRUE
            warning("Setting option knit to TRUE based on the file name given!")
        }
    }
    if (isTRUE(knit)) {
        output_basename <- sub("\\.[Rr](.*)", ".\\1", basename(file_name))
        if (isTRUE(write_to_disk)) {
            knit_out_file <- file.path(dirname(file_name), output_basename)
        } else {
            message(hint_writing(file_name))
            knit_out_file <- file.path(tempdir(), output_basename)
        }
        ops <- options() ## TODO: knitr changes the options?!
        file_name <- knitr::knit(file_name, output = knit_out_file,
                                 envir = envir)
        options(ops) ## restore old options
    }
    return(file_name)
}

run_knitr <- function(file_name, working_directory = dirname(file_name),
                      knit = NA,
                      hooks = NULL,
                      write_to_disk = getOption("write_to_disk"),
                      replacement = NULL,
                      envir = parent.frame()) {
    current_hooks <- knitr::knit_hooks$get()
    adjust_asciidoc_hooks(hooks = hooks, replacement = replacement)
    on.exit(knitr::knit_hooks$set(current_hooks))
    file_name <- normalizePath(file_name)
    withr::with_dir(working_directory, {
                    if (is_spin_file(file_name)) {
                        content <- knitr::spin(text = readLines(file_name),
                                               knit = TRUE,
                                            report = FALSE, envir = envir)
                        output_basename <- sub("\\.[Rr]", ".asciidoc",
                                               basename(file_name))
                        if (isTRUE(write_to_disk)) {
                            out_file <- file.path(dirname(file_name),
                                                  output_basename)
                        } else {
                            message(hint_writing(file_name))
                            out_file <- file.path(tempdir(), output_basename)
                        }
                        writeLines(content, out_file)
                    } else {
                        out_file <- run_knit(file_name, knit = knit,
                                             envir = envir, 
                                             write_to_disk = write_to_disk)
                    }
                    out_file <- normalizePath(out_file)
                    })
    return(out_file)
}

is_spin_file <- function(file_name) {
    is_r_file <- grepl("^.*\\.[rR]$", file_name)
    has_roxygen_comment <- any(grepl("^#'", readLines(file_name)))
    has_spin_knitr_chunk_options <- any(grepl("^#-|^#\\+",
                                              readLines(file_name)))
    is_spin <- is_r_file && has_roxygen_comment || has_spin_knitr_chunk_options
    return(is_spin)
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
#' to force knitting or to \code{\link{FALSE}} (anything apart from
#' \code{\link{TRUE}} or \code{\link{NA}}, really), to
#' disable knitting.
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
#' wd <- file.path(tempdir(), "rasciidoc")
#' dir.create(wd)
#' file  <- system.file("files", "minimal", "knit.Rasciidoc",
#'                      package = "rasciidoc")
#' file.copy(file, wd)
#' rasciidoc::render(file.path(wd, basename(file)), asciidoc_args = "-b slidy")
#' if (isTRUE(getOption("write_to_disk"))) {
#'     dir(wd, full.names = TRUE)
#' } else {
#'     dir(tempdir(), full.names = TRUE)
#' }
#' unlink(wd, recursive = TRUE)
render <- function(file_name, knit = NA,
                   write_to_disk = getOption("write_to_disk"),
                   envir = parent.frame(),
                   hooks = c("message", "error", "warning"),
                   replacement = "source", asciidoc_args = NULL,
                   what = c("auto", "all", "no_slides", "slides"),
                   clean = FALSE, ...) {
    status <- 1
    on.exit(if (isTRUE(clean)) file.remove(excerpted_file, adoc))
    what <- match.arg(what)
    if (what == "auto") {
        lines <- readLines(file_name)
        if (any(grepl("^// *(begin|end)_only_slide", lines),
                grepl("// *slide_only", lines),
                grepl("// *no_slide", lines)))
            what <- "no_slides"
    }

    excerpted_file <- switch(what,
                             "slides" = excerpt_slides(file_name, 
                                                       write_to_disk = write_to_disk),
                             "no_slides" = excerpt_no_slides(file_name, 
                                                             write_to_disk = write_to_disk),
                             file_name)


    tryCatch(
             adoc <- run_knitr(file_name = excerpted_file,
                               knit = knit, envir = envir,
                               hooks = hooks, replacement = replacement,
                               write_to_disk = write_to_disk),
             error = function(e) throw(e)

    )
    status <- rasciidoc(file_name = adoc, 
                        write_to_disk = write_to_disk, asciidoc_args)
    return(status)
}

excerpt_to_file <- function(file_name,
                            begin_pattern, end_pattern,
                            exclusion_pattern, inclusion_pattern,
                            write_to_disk = getOption("write_to_disk"),
                            output_name = NA) {
    if (is.na(output_name))
        output_name <- basename(tempfile(fileext = ".Rasciidoc"))
    if (isTRUE(write_to_disk)) {
       output_directory <- dirname(file_name)
    } else {
        message(hint_writing(file_name))
        output_directory <- tempdir()
    }
    glbt <- document::get_lines_between_tags
    excerpt <- glbt(file_name = file_name, keep_tagged_lines = FALSE,
                    begin_pattern = begin_pattern,
                    end_pattern = end_pattern,
                    from_first_line = TRUE, to_last_line = TRUE)
    excerpt <- grep(exclusion_pattern, excerpt, invert = TRUE, value = TRUE)
    excerpt <- sub(paste0(inclusion_pattern, ".*"), "", excerpt)
    # The asciidoc file has to be _here_ for sourcing to work!
    excerpt_file <- file.path(output_directory, output_name)
    writeLines(excerpt, excerpt_file)
    return(excerpt_file)
}

excerpt_no_slides <- function(file_name, 
                              write_to_disk = getOption("write_to_disk")
                              ) {
    return(excerpt_to_file(file_name = file_name,
                      begin_pattern = "^// *end_only_slide",
                      end_pattern = "^// *begin_only_slide",
                      inclusion_pattern = "// *no_slide",
                      exclusion_pattern = "// *slide_only",
                      write_to_disk = write_to_disk,
                      output_name = paste0(basename(file_name), "_ex"))
    )
}

excerpt_slides <- function(file_name,
                           write_to_disk = getOption("write_to_disk")
                           ) {
    return(excerpt_to_file(file_name = file_name,
                      begin_pattern = "^// *end_no_slide",
                      end_pattern = "^// *begin_no_slide",
                      inclusion_pattern = "// *slide_only",
                      exclusion_pattern = "// *no_slide",
                      write_to_disk = write_to_disk,
                      output_name = sub("(^.*)(\\.[rR]?asc.*)$",
                                        "\\1_slides\\2",
                                        basename(file_name)))
    )
}
