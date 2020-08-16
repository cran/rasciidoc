if (interactive()) {
    pkgload::load_all()
    is_probably_my_machine <- function() {
        me <- Sys.info()[["nodename"]] %in% c("h6") &&
            .Platform[["OS.type"]] == "unix"
        return(me)

    }
    if (is_probably_my_machine()) options("write_to_disk" = TRUE)
}
is_installed_asciidoc <- function() return(rasciidoc:::is_installed("asciidoc"))
remove_dates <- function(x) {
    grep(".*CE[S]?T$", value = TRUE, invert = TRUE,
         grep(".*UTC$", value = TRUE, invert = TRUE, x)
         )
}

test_render_simple <- function() {
    if (is_probably_my_machine() && isTRUE(getOption("write_to_disk"))) {
        folder  <- system.file("runit_tests", "files", package = "rasciidoc")
        file.copy(folder, tempdir(), recursive = TRUE)
        on.exit(unlink(file.path(tempdir(), "files"), recursive = TRUE))
        #% render
        withr::with_dir(file.path(tempdir(), "files"),
                        result <- rasciidoc::render("simple.Rasciidoc"))
        if (! is_installed_asciidoc()) {
            RUnit::checkTrue(! identical(result, as.integer(0)))
        } else {
            result <- remove_dates(readLines(file.path(tempdir(), "files",
                                                       "simple.html")))
            if (FALSE) {
                # in case you install a new version of asciidoc or the like...
                file.copy(file.path(tempdir(), "files", "simple.html"),
                          file.path("inst", "runit_tests", "files", "expected"),
                          overwrite = TRUE
                          )
            }
            expectation <- remove_dates(readLines(file.path(tempdir(), "files",
                                                            "expected",
                                                            "simple.html")))
                RUnit::checkIdentical(result, expectation)
            #% render slides
            message("FIXME: need coverage for slides?")

            # file contains no R code
            withr::with_dir(file.path(tempdir(), "files"),
                            rasciidoc::render("fake.Radoc", knit = NA))
            result <- remove_dates(readLines(file.path(tempdir(), "files",
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
            RUnit::checkIdentical(result, expectation)
        }
    }
}
if (interactive()) test_render_simple()

test_knit_spin <- function() {
    condition <- is_installed_asciidoc() && is_probably_my_machine() &&
        isTRUE(getOption("write_to_disk"))
    if (condition) {
        withr::with_dir(tempdir(), {
               file.copy(list.files(system.file("files", "simple",
                                                package = "rasciidoc"),
                                    full.names = TRUE
                                    ),
                         ".", recursive = TRUE)
               # lintr inevitably reads spin.R and crashes (I tried all
               # kindes of exlusions...). I therefore moved spin.R to
               # spin.R_nolint to make lintr not read the file.
               # But I need it to end on R or r when deciding whether
               # to knit or spin. So I rename here:
               file.rename("spin.R_nolint", "spin.R")
               rasciidoc::render("spin.R")
               file.copy("spin.asciidoc", "foo.asciidoc")
               rasciidoc::rasciidoc("foo.asciidoc")
               spin <- remove_dates(readLines("spin.html"))
               ascii_md <- remove_dates(readLines("foo.html"))
               RUnit::checkIdentical(spin, ascii_md)
               rasciidoc::render(file_name = "knit.Rasciidoc")
               rasciidoc::render("knit.Rasciidoc", clean = FALSE, what = "all")
               file.copy("knit.asciidoc", "bar.asciidoc")
               rasciidoc::rasciidoc("bar.asciidoc")
               knit <- remove_dates(readLines("knit.html"))
               ascii <- remove_dates(readLines("bar.html"))
               RUnit::checkIdentical(knit, ascii)
             })
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
    on.exit( knitr::knit_hooks$restore())
    # covr infects functions, so we deparse an grep them first
    remove_if_TRUE <- function(x) {
        ## covr::package_coverage inserts `if(TRUE){` ...
        return(gsub("if\\(TRUE\\)\\{", "", x))
    }

    clean <- function(x) {
        r <- gsub(" ", "", paste0(grep("covr:::count|   \\{$|   \\}$",
                                       invert = TRUE, value = TRUE, deparse(x)),
                               collapse = ""))
        return(remove_if_TRUE(r))
    }
    # get a verbartim copy from adjust_knitr_hooks:
    hook.source <- function(x, options) {
        x <- paste(c(hilight_source(x, "asciidoc", options), ""),
                   collapse = "\n")
        sprintf("\n[source,%s]\n----\n%s----\n", tolower(options$engine),
                x)
    }
    hook.message <- function(x, options) {
        sprintf("\n[NOTE]\n====\n.Message\n%s\n====\n",
                substring(x, comment_length(options$comment)))
    }
    hook.warning <- function(x, options) {
        sprintf("\n[WARNING]\n====\n.Warning\n%s\n====\n",
                gsub("^.*Warning: ", "", x))
    }
    hook.error <- function(x, options) {
        sprintf("\n[CAUTION]\n====\n.Error\n%s\n====\n",
                gsub("^.*Error: ", "", x))
    }
    hook.output <- function(x, options) sprintf("\n----\n%s----\n", x)
    knitr::knit_hooks$restore()
    rasciidoc::adjust_asciidoc_hooks(replacement = NULL)
    cs <- knitr::knit_hooks$get("source")
    co <- knitr::knit_hooks$get("output")
    cm <- knitr::knit_hooks$get("message")
    cw <- knitr::knit_hooks$get("warning")
    ce <- knitr::knit_hooks$get("error")
    RUnit::checkEquals(clean(cs), clean(hook.source))
    RUnit::checkEquals(clean(co), clean(hook.output))
    RUnit::checkEquals(clean(cm), clean(hook.message))
    RUnit::checkEquals(clean(cw), clean(hook.warning))
    RUnit::checkEquals(clean(ce), clean(hook.error))
    rasciidoc::adjust_asciidoc_hooks(hooks = c("message", "warning", "error"),
                                     replacement = "source")
    cs <- knitr::knit_hooks$get("source")
    co <- knitr::knit_hooks$get("output")
    cm <- knitr::knit_hooks$get("message")
    cw <- knitr::knit_hooks$get("warning")
    ce <- knitr::knit_hooks$get("error")
    RUnit::checkEquals(clean(cs), clean(hook.source))
    RUnit::checkEquals(clean(co), clean(hook.output))
    RUnit::checkEquals(clean(cm), clean(hook.source))
    RUnit::checkEquals(clean(cw), clean(hook.source))
    RUnit::checkEquals(clean(ce), clean(hook.source))
    rasciidoc::adjust_asciidoc_hooks(hooks = c("source"), replacement = "error")
    cs <- knitr::knit_hooks$get("source")
    RUnit::checkEquals(clean(cs), clean(hook.error))
}
}
