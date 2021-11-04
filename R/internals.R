.onAttach <- function(...) { # Exclude Linting
    if (!isTRUE(getOption("write_to_disk")))
        packageStartupMessage("\n", hint_writing())
}

write_default_output <- function(msg, adoc_file) {
        lines <- readLines(system.file("files", "default.html",
                                       package = "rasciidoc"))
        lines <- sub("DEFAULT_TEXT", paste(msg, collapse = "<br>"), lines)
        writeLines(lines, con = sub("\\.[A-z]*$", ".html", adoc_file))
        status <- FALSE
        return(status)
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

discover_python <- function(first_only = TRUE, stop_on_error = TRUE) {
    candidates <- sapply(c("python", "python2", "python3"),
                         function(x) return(as.character(Sys.which(x))))
    ## <<--- Adapted from reticulate(1.16)::py_discover_config()
    # provide other common locations
    if (is_windows()) {
        candidates <- c(candidates,
                        reticulate::py_versions_windows()$executable_path)
    } else {
        candidates <- c(candidates,
                        "/usr/bin/python3",
                        "/usr/local/bin/python3",
                        "/opt/python/bin/python3",
                        "/opt/local/python/bin/python3",
                        "/usr/bin/python",
                        "/usr/local/bin/python",
                        "/opt/python/bin/python",
                        "/opt/local/python/bin/python",
                        path.expand("~/anaconda3/bin/python"),
                        path.expand("~/anaconda/bin/python")
                        )
    }
    candidates <- unique(candidates)
    # filter locations by existence
    if (length(candidates) > 0)
        python_versions <- candidates[file.exists(candidates)]
    ## --->>
    if (isTRUE(first_only)) python_versions <- python_versions[1]
    if (is.na(python_versions) && isTRUE(stop_on_error))
        throw("Found no python installation!")
    return(python_versions)
}


get_asciidoc <- function(python = NA) {
    if (is.na(python)) python <- tryCatch(discover_python(), error = identity)
    if (!inherits(python, "error")) {
        local_asciidoc_path <- file.path(tempdir(), "asciidoc")
        local_asciidoc_path <- normalizePath(local_asciidoc_path,
                                             mustWork = FALSE)
        config_file <- normalizePath(file.path(local_asciidoc_path,
                                               "rasciidoc_config.R"),
                                     mustWork = FALSE)

        if (file.exists(config_file)) {
            source(config_file, local = TRUE)
        } else {
            unlink(local_asciidoc_path, recursive = TRUE, force = TRUE)
            dir.create(local_asciidoc_path)
            python_version <- sub("Python ", "",
                                  system2(python, "--version",
                                          stderr = TRUE, stdout = TRUE))
            # NOTE: I remove release candidate markers from the current python
            # version. I do so because python 2.7.18rc1 is
            # currently (2020-04-14)
            # installed on some CRAN maschines
            #(r-devel-linux-x86_64-debian-clang).
            # And package_version can't deal with release candidate markers.
            # Since release candidates "can only have bugfixes applied that have
            # been reviewed by other core developers"
            # (https://devguide.python.org/devcycle/#release-candidate-rc).
            # So it should be pretty save to do so. And I do not know any way to
            # determine the last stable version before an rc
            # (3.4.0rc1 gives what?).
            python_version <- sub("rc.*$", "", python_version)
            python_major <- package_version(python_version)[[c(1, 1)]]
            python_major <- as.character(python_major)
            if (python_major == "3" && fritools::is_installed("python2")) {
                # asciidoc was origninally written in python2, so python2 wins.
                # TODO: if python2 is available, but the version is not
                # sufficient,should I fall back to python3?
                python <- Sys.which("python2")
                python_major <- "2"
            }
            url <- switch(python_major,
                          "2" = "https://github.com/asciidoc-py/asciidoc-py2",
                          "3" = "https://github.com/asciidoc-py/asciidoc-py",
                          throw(paste("Could not find python version 2",
                                      "nor python version 3."))
                          )
            if (fritools::is_installed("git")) {
                # gert fails to clone on some machines, so try to use a system
                # installation of git first.
                if (fritools::is_running_on_fvafrcu_machines() &&
                    fritools::is_windows()) {
                    # FVAFR messes with its proxies...
                    # this is a private local setting.
                    # Don't bother.
                    url <- sub("^(http)s", "\\1", url)
                }
                system(paste("git clone", url, local_asciidoc_path))

            } else {
                gert::git_clone(url = url, path = local_asciidoc_path)
            }
            # reset to the last tagged release: we don't want any unfunctional
            # devel stuff in there.
            tags <- gert::git_tag_list(repo = local_asciidoc_path)[["name"]]
            if (any(grepl("[[:alpha:]]", tags))) {
                tags <- tags[-grep("[[:alpha:]]", tags)]
            }
            if (identical(python_major, "2")) {
                last_tag <- sort(package_version(tags), decreasing = TRUE)[1]
            } else {
                # NOTE: Matthew Peveler messes with the current asciidoc
                # python3-implementation, so we use a historic working version.
                last_tag <- "9.1.0"
            }
            gert::git_reset_hard(repo = local_asciidoc_path,
                                 ref = as.character(last_tag))

            asciidoc_source <- normalizePath(list.files(local_asciidoc_path,
                                                        pattern =
                                                            "^asciidoc.py$",
                                                        recursive = TRUE,
                                                        full.names = TRUE))
            min_py_version <- query_min_py_version(file = asciidoc_source,
                                                   python_version =
                                                       python_major)
            if (!is_version_sufficient(python_version, min_py_version))
                throw(paste0("Could find not find python >= ", min_py_version,
                             "."))
            res <- list("python_cmd" = python,
                        "asciidoc_source" = asciidoc_source
                        )
            dump("res", config_file)
        }
        return(res)
    } else {
        throw("Python is a system requirement.")
    }
}



query_min_py_version <- function(file, python_version) {
    required <- grep("^MIN_PYTHON_VERSION", readLines(file),
                     value = TRUE)
    min_py_version <- switch(python_version,
                             "2" = sub("'.*", "",
                                       sub("^MIN_PYTHON_VERSION = '",
                                           "",
                                           required)),
                             "3" = sub(", ", ".", sub(".*\\((.*)\\).*",
                                                      "\\1",
                                                      required)),
                             throw(paste("Could not find python version 2",
                                         "nor python version 3."))
                             )
    return(min_py_version)
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
                                                   report = FALSE,
                                                   envir = envir)
                            output_basename <- sub("\\.[Rr]", ".asciidoc",
                                                   basename(file_name))
                            if (isTRUE(write_to_disk)) {
                                out_file <- file.path(dirname(file_name),
                                                      output_basename)
                            } else {
                                message(hint_writing(file_name))
                                out_file <- file.path(tempdir(),
                                                      output_basename)
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
