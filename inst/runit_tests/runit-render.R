if (interactive()) pkgload::load_all()
remove_dates <- function(x) {
    grep(".*CE[S]?T$", value = TRUE, invert = TRUE,
         grep(".*UTC$", value = TRUE, invert = TRUE, x)
         )
}
condition <- !checkmate::test_os("solaris") && !checkmate::test_os("windows") ||
    fritools::get_run_r_tests()
if (condition) {
    # CRAN solaris has a buggy asciidoc installation, so we skip these test for
    # solaris.
    # What a pitty!
    test_rasciidoc_simple <- function() {
        folder  <- system.file("runit_tests", "files", package = "rasciidoc")
        adoc <- file.path(folder, "simple.asciidoc")
        wdir <- tempfile()
        dir.create(wdir)
        on.exit(unlink(wdir, recursive = TRUE))
        file.copy(adoc, wdir)
        # misspecified arguments
        withr::with_dir(tempdir(),
                        status <-
                            rasciidoc::rasciidoc(file.path(wdir,
                                                           basename(adoc)),
                                                 write_to_disk = FALSE,
                                                 enforce_requirements = FALSE,
                                                 "-b does_not_exists")
                        )
        RUnit::checkTrue(!status)

        withr::with_dir(tempdir(), {
                        rara <- rasciidoc::rasciidoc
                        status <- rara(file.path(wdir, basename(adoc)))
                        })
        if (!rasciidoc:::is_installed(rasciidoc:::discover_python())) {
            RUnit::checkTrue(!status)
        } else {
            RUnit::checkTrue(status)
            if (fritools::is_running_on_fvafrcu_machines() &&
                rasciidoc:::is_installed("rasciidoc")) {
                # don't know which version of asciidoc will be used on remote
                # machines
                if (rasciidoc:::is_installed("source-highlight")) {
                    infile <- file.path(folder, "expected", "simple.html")
                    expectation <- remove_dates(readLines(infile))
                } else {
                    infile <- file.path(folder, "expected",
                                        "no_source_highlight",
                                        "simple.html")
                    expectation <- remove_dates(readLines(infile))
                }
                result <- remove_dates(readLines(file.path(tempdir(),
                                                           "simple.html")))
                RUnit::checkIdentical(result, expectation)
            }
        }
    }

    no_test_render_simple <- function() {
        folder  <- system.file("runit_tests", "files", package = "rasciidoc")
        file.copy(folder, tempdir(), recursive = TRUE)
        on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
        #% render
        withr::with_dir(file.path(tempdir(), "files"),
                        status <- rasciidoc::render("simple.Rasciidoc"))
        if (!rasciidoc:::is_installed(rasciidoc:::discover_python())) {
            RUnit::checkTrue(!status)
        } else {
            RUnit::checkTrue(status)
            result <- remove_dates(readLines(file.path(tempdir(),
                                                       "simple.html")))
            if (FALSE) {
                # in case you install a new version of asciidoc or the like...
                file.copy(file.path(tempdir(), "files", "simple.html"),
                          file.path("inst", "runit_tests", "files", "expected"),
                          overwrite = TRUE
                          )
            }
            if (rasciidoc:::is_installed("source-highlight")) {
                expectation <- remove_dates(readLines(file.path(folder,
                                                                "expected",
                                                                "simple.html")))
            } else {
                infile <- file.path(folder, "expected", "no_source_highlight",
                                    "simple.html")
                expectation <- remove_dates(readLines(infile))
            }
            # don't know which version of asciidoc will be used on remote
            # machines
            if (fritools::is_running_on_fvafrcu_machines())
                RUnit::checkIdentical(result, expectation)
        }
        #% render slides
        message("FIXME: need coverage for slides?")

        # file contains no R code
        withr::with_dir(file.path(tempdir(), "files"),
                        status <- rasciidoc::render("fake.Radoc", knit = NA))
        if (!rasciidoc:::is_installed(rasciidoc:::discover_python())) {
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
    if (interactive()) test_render_simple()

    no_test_knit_spin <- function() {
        rdp <- rasciidoc:::discover_python ## for lintr
        withr::with_dir(tempdir(), {
                            dir <- system.file("files", "simple",
                                               package = "rasciidoc")
                            file.copy(list.files(dir, full.names = TRUE),
                                      ".", recursive = TRUE)
                            # lintr inevitably reads spin.R and crashes
                            # (I tried all kindes of exlusions...).
                            # I therefore moved spin.R to
                            # spin.R_nolint to make lintr not read the file.
                            # But I need it to end on R or r when deciding
                            # whether to knit or spin. So I rename here:
                            file.rename("spin.R_nolint", "spin.R")
                            rasciidoc::render("spin.R")
                            file.copy("spin.asciidoc", "foo.asciidoc")
                            status <- rasciidoc::rasciidoc("foo.asciidoc")
                            if (!rasciidoc:::is_installed(rdp())) {
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
                            rasciidoc::render(file_name = "knit.Rasciidoc")
                            rasciidoc::render("knit.Rasciidoc", clean = FALSE,
                                              what = "all")
                            file.copy("knit.asciidoc", "bar.asciidoc")
                            status <- rasciidoc::rasciidoc("bar.asciidoc")
                            if (!rasciidoc:::is_installed(rdp())) {
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
                          })
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
            remove_if_true <- function(x) {
                ## covr::package_coverage inserts `if (TRUE) {` ...
                return(gsub("if\\(TRUE\\)\\{", "", x))
            }

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
            rasciidoc::adjust_asciidoc_hooks(replacement = NULL)
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
            rasciidoc::adjust_asciidoc_hooks(hooks = c("message", "warning",
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
            rasciidoc::adjust_asciidoc_hooks(hooks = c("source"),
                                             replacement = "error")
            cs <- knitr::knit_hooks$get("source")
            RUnit::checkEquals(clean(cs), clean(hook_error))
        }
    }
}
