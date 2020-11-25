if (interactive()) pkgload::load_all()
test_excerption <- function() {

    file.copy(system.file("runit_tests", "files", "excerption_file.Rasciidoc",
              package = "rasciidoc"), tempdir(), overwrite = TRUE)

   # slides
    path <- rasciidoc:::excerpt_slides(file.path(tempdir(),
                                                 "excerption_file.Rasciidoc"))
    result <- readLines(path)
    expectation <- readLines(system.file("runit_tests", "files",
                                         "expected", "excerpted_slides.txt",
                                         package = "rasciidoc"))
    RUnit::checkIdentical(result, expectation)

    # no slides
    path <- rasciidoc:::excerpt_no_slides(file.path(tempdir(),
                                                    "excerption_file.Rasciidoc")
    )
    result <- readLines(path)
    expectation <- readLines(system.file("runit_tests", "files",
                                         "expected", "excerpted_no_slides.txt",
                                         package = "rasciidoc"))
    RUnit::checkIdentical(result, expectation)

}
