if (interactive()) pkgload::load_all(".")
testthat::context("Testing get_asciidoc")
rdp <- rasciidoc:::discover_python
testthat::test_that("discover python", {
                        result <- tryCatch(rdp(),
                                           error = identity)
                        if (fritools::is_installed(rdp(stop_on_error = FALSE))
                            ) {
                            testthat::expect_true(is.character(result))
                        } else {
                            testthat::expect_true(inherits(result, "error"))
                        }
}
)
testthat::test_that("get asciidoc", {
                        result <- tryCatch(rasciidoc:::get_asciidoc(),
                                           error = identity)
                        if (fritools::is_installed(rdp(stop_on_error = FALSE))
                            ) {
                            testthat::expect_type(result[["asciidoc_source"]],
                                                  "character")
                            source_file <- result[["asciidoc_source"]]
                            testthat::expect_true(file.exists(source_file))
                        } else {
                            testthat::expect_true(inherits(result, "error"))
                        }
}
)

testthat::test_that("run asciidoc", {
            folder  <- system.file("runit_tests", "files",
                                   package = "rasciidoc")
            adoc <- file.path(folder, "simple.asciidoc")
            wdir <- tempfile()
            dir.create(wdir)
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
            testthat::expect_true(!status)

            withr::with_dir(tempdir(),
                            status <-
                                rasciidoc(file.path(wdir,
                                                    basename(adoc)))
                            )
            if (fritools::is_installed(rdp(stop_on_error = FALSE))) {
                if (isTRUE(status)) {
                    testthat::expect_true(status)
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
                                  function(x) any(grepl(x, result,
                                                        fixed = TRUE)))
                    testthat::expect_true(all(hit))
                } else {
                    print(sessionInfo())
                    message(paste(names(unlist(res)), unlist(res),
                                  collapse = "; ", sep = ": "))
                }
            } else {
                testthat::expect_true(!status)
            }
}
)

testthat::test_that("render", {

        folder <- system.file("runit_tests", "files", package = "rasciidoc")
        file.copy(folder, tempdir(), recursive = TRUE)
        #% render
        withr::with_dir(file.path(tempdir(), "files"),
                        status <- render("simple.Rasciidoc"))
        infile <- file.path(tempdir(), "files", "simple.Rasciidoc")
        if (fritools::is_installed(rdp(stop_on_error = FALSE))) {
            if (isTRUE(status)) {
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
                testthat::expect_true(all(hit))
            } else {
                print(sessionInfo())
                message(paste(names(unlist(res)), unlist(res),
                              collapse = "; ", sep = ": "))
            }
        } else {
            testthat::expect_true(!status)
        }
}
)
