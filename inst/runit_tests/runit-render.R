if (interactive()) pkgload::load_all()
remove_dates <- function(x) {
  grep("^ *20[0-9]{2}.[0-9]{2}.[0-9]{2}", value = TRUE, invert = TRUE,
    grep(".*CE[S]?T$", value = TRUE, invert = TRUE,
         grep(".*UTC$", value = TRUE, invert = TRUE, x)
         )
  )
}

remove_if_true <- function(x) {
    ## covr::package_coverage inserts `if (TRUE) {` ...
    return(gsub("if\\(TRUE\\)\\{", "", x))
}
skip <- checkmate::test_os("solaris") &&
    !(fritools::is_running_on_fvafrcu_machines() ||
      fritools::is_running_on_gitlab_com()
  )

if (!skip) {
        test_rasciidoc_simple <- function() {
            folder  <- system.file("runit_tests", "files",
                                   package = "rasciidoc")
            adoc <- file.path(folder, "simple.asciidoc")
            wdir <- tempfile()
            dir.create(wdir)
            on.exit(unlink(wdir, recursive = TRUE))
            file.copy(adoc, wdir)
            # misspecified arguments
            withr::with_dir(tempdir(),
                            status <-
                                rasciidoc(file.path(wdir,
                                                    basename(adoc)),
                                          write_to_disk = FALSE,
                                          enforce_requirements = FALSE,
                                          "-b does_not_exists")
                            )
            RUnit::checkTrue(!status)

            withr::with_dir(tempdir(),
                            status <-
                                rasciidoc(file.path(wdir,
                                                    basename(adoc)))
                            )
            if (fritools::is_installed(rasciidoc:::discover_python())) {
                RUnit::checkTrue(status)
                result <- readLines(file.path(tempdir(), "simple.html"))
                pattern <- "(^([-:[]|$)|\\t)"
                if (!fritools::is_installed("source-highlight")) {
                    pattern <- paste0(pattern, "|\\(.*\\)")
                }
                expected <- grep(value = TRUE, invert = TRUE,
                                 pattern = pattern,
                                 readLines(adoc))
                expected <- trimws(sub("^=* ", "", expected))
                hit <- sapply(expected,
                              function(x) any(grepl(x, result, fixed = TRUE)))
                RUnit::checkTrue(all(hit))
            } else {
                RUnit::checkTrue(!status)
            }
        }

        if (interactive()) test_rasciidoc_simple()
    test_render_simple <- function() {
        folder <- system.file("runit_tests", "files", package = "rasciidoc")
        file.copy(folder, tempdir(), recursive = TRUE)
        on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
        #% render
        withr::with_dir(file.path(tempdir(), "files"),
                        status <- render("simple.Rasciidoc"))
        infile <- file.path(tempdir(), "files", "simple.Rasciidoc")
        if (fritools::is_installed(rasciidoc:::discover_python())) {
            RUnit::checkTrue(status)
            result <- readLines(file.path(tempdir(), "simple.html"))
            pattern <- "(^([:/]|$)|\\t)"
            if (!fritools::is_installed("source-highlight")) {
                pattern <- paste0(pattern, "|\\(.*\\)")
            }
            data(mtcars)
            expected <- c(readLines(infile), capture.output(str(mtcars)))
            expected <- grep(value = TRUE, invert = TRUE,
                             pattern = pattern,
                             expected)
            expected <- trimws(sub("^=* ", "", expected))
            hit <- sapply(expected,
                          function(x) any(grepl(x, result, fixed = TRUE)))
            RUnit::checkTrue(all(hit))
        } else {
            RUnit::checkTrue(!status)
        }

    }
    if (interactive()) test_render_simple()
    test_render_slides <- function() {
        ##% render slides
        message("FIXME: need coverage for slides?")
        folder <- system.file("runit_tests", "files", package = "rasciidoc")
        file.copy(folder, tempdir(), recursive = TRUE)
        on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))

        # file contains no R code
        withr::with_dir(file.path(tempdir(), "files"),
                        status <- render("fake.Radoc", knit = NA))
        if (!fritools::is_installed(rasciidoc:::discover_python())) {
            RUnit::checkTrue(!status)
        } else {
            RUnit::checkTrue(status)
            result <- remove_dates(readLines(file.path(tempdir(),
                                                       "fake.html")))
            if (FALSE) {
                file.copy(file.path(tempdir(), "files", "fake.html"),
                          file.path("inst", "runit_tests", "files", "expected"),
                          overwrite = TRUE
                          )
            }
            expectation <- remove_dates(readLines(file.path(tempdir(), "files",
                                                            "expected",
                                                            "fake.html")))
            # don't know which version of asciidoc will be used on remote
            # machines
            if (fritools::is_running_on_fvafrcu_machines())
                RUnit::checkIdentical(result, expectation)
        }
    }
    if (interactive()) test_render_slides()

    test_knit_spin <- function() {
        old_wd <- setwd(tempdir())
        on.exit(setwd(old_wd))
        dir <- system.file("files", "simple", package = "rasciidoc")
        file.copy(list.files(dir, full.names = TRUE), ".", recursive = TRUE)
        # lintr inevitably reads spin.R and crashes
        # (I tried all kindes of exlusions...).
        # I therefore moved spin.R to
        # spin.R_nolint to make lintr not read the file.
        # But I need it to end on R or r when deciding
        # whether to knit or spin. So I rename here:
        file.rename("spin.R_nolint", "spin.R")
        render("spin.R")
        file.copy("spin.asciidoc", "foo.asciidoc")
        status <- rasciidoc("foo.asciidoc")
        if (!fritools::is_installed(rasciidoc:::discover_python())) {
            RUnit::checkTrue(!status)
        } else {
            RUnit::checkTrue(status)
            spin <- remove_dates(readLines("spin.html"))
            ascii_md <- remove_dates(readLines("foo.html"))
            # don't know which version of asciidoc will be
            # used on remote machines
            if (fritools::is_running_on_fvafrcu_machines())
                RUnit::checkIdentical(spin, ascii_md)
        }
        render(file_name = "knit.Rasciidoc")
        render("knit.Rasciidoc", clean = FALSE,
               what = "all")
        file.copy("knit.asciidoc", "bar.asciidoc")
        status <- rasciidoc("bar.asciidoc")
        if (!fritools::is_installed(rasciidoc:::discover_python())) {
            RUnit::checkTrue(!status)
        } else {
            RUnit::checkTrue(status)
            knit <- remove_dates(readLines("knit.html"))
            ascii <- remove_dates(readLines("bar.html"))
            # don't know which version of asciidoc will be
            # used on remote machines
            if (fritools::is_running_on_fvafrcu_machines())
                RUnit::checkIdentical(knit, ascii)
        }
    }

    test_adjusting_hooks <- function() {
        if (grepl(paste0("^", dirname(tempdir()), ".*$"), getwd()) && FALSE) {
            warning("skipping test for covr")
        } else {
            # if this fails, insert prints or messages,
            # run covr::package_coverage(path = ".", clean = FALSE), read
            # the output, it will point you to something like
            # /tmp/RtmpXXX/R_LIBSXXX/rasciidoc/rasciidoc-tests/runit.Rout.fail
            on.exit(knitr::knit_hooks$restore())
            # covr infects functions, so we deparse an grep them first

            clean <- function(x) {
                r <- gsub(" ", "", paste0(grep("covr:::count|   \\{$|   \\}$",
                                               invert = TRUE, value = TRUE,
                                               deparse(x)),
                                       collapse = ""))
                return(remove_if_true(r))
            }
            # get a verbartim copy from adjust_knitr_hooks:
            hook_source <- function(x, options) {
                x <- paste(c(hilight_source(x, "asciidoc", options), ""),
                           collapse = "\n")
                sprintf("\n[source,%s]\n----\n%s----\n",
                        tolower(options$engine),
                        x)
            }
            hook_message <- function(x, options) {
                sprintf("\n[NOTE]\n====\n.Message\n%s\n====\n",
                        substring(x, comment_length(options$comment)))
            }
            hook_warning <- function(x, options) {
                sprintf("\n[WARNING]\n====\n.Warning\n%s\n====\n",
                        gsub("^.*Warning: ", "", x))
            }
            hook_error <- function(x, options) {
                sprintf("\n[CAUTION]\n====\n.Error\n%s\n====\n",
                        gsub("^.*Error: ", "", x))
            }
            hook_output <- function(x, options) sprintf("\n----\n%s----\n", x)
            knitr::knit_hooks$restore()
            adjust_asciidoc_hooks(replacement = NULL)
            cs <- knitr::knit_hooks$get("source")
            co <- knitr::knit_hooks$get("output")
            cm <- knitr::knit_hooks$get("message")
            cw <- knitr::knit_hooks$get("warning")
            ce <- knitr::knit_hooks$get("error")
            RUnit::checkEquals(clean(cs), clean(hook_source))
            RUnit::checkEquals(clean(co), clean(hook_output))
            RUnit::checkEquals(clean(cm), clean(hook_message))
            RUnit::checkEquals(clean(cw), clean(hook_warning))
            RUnit::checkEquals(clean(ce), clean(hook_error))
            adjust_asciidoc_hooks(hooks = c("message", "warning",
                                            "error"),
                                  replacement = "source")
            cs <- knitr::knit_hooks$get("source")
            co <- knitr::knit_hooks$get("output")
            cm <- knitr::knit_hooks$get("message")
            cw <- knitr::knit_hooks$get("warning")
            ce <- knitr::knit_hooks$get("error")
            RUnit::checkEquals(clean(cs), clean(hook_source))
            RUnit::checkEquals(clean(co), clean(hook_output))
            RUnit::checkEquals(clean(cm), clean(hook_source))
            RUnit::checkEquals(clean(cw), clean(hook_source))
            RUnit::checkEquals(clean(ce), clean(hook_source))
            adjust_asciidoc_hooks(hooks = c("source"),
                                  replacement = "error")
            cs <- knitr::knit_hooks$get("source")
            RUnit::checkEquals(clean(cs), clean(hook_error))
        }
    }
}
