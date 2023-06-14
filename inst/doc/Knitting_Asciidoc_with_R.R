## -----------------------------------------------------------------------------
file_name <- system.file("files", "minimal", "plot.Rasciidoc",
                         package = "rasciidoc")
cat(readLines(file_name), sep = "\n")


## -----------------------------------------------------------------------------
withr::with_dir(tempdir(), {
                    file.copy(file_name, ".")
                    rasciidoc::render(basename(file_name))
})


## -----------------------------------------------------------------------------
file_name <- system.file("files", "simple", "knit.Rasciidoc",
                         package = "rasciidoc")
cat(readLines(file_name), sep = "\n")


## -----------------------------------------------------------------------------
my_directory <- file.path(tempdir(), "simple")
dir.create(my_directory)
withr::with_dir(my_directory, {
                    file.copy(file_name, ".")
                    file.copy(file.path(dirname(file_name), "src"), ".",
                              recursive = TRUE)
})
dir(my_directory, recursive = TRUE, full.names = TRUE)


## -----------------------------------------------------------------------------
rasciidoc::render(file.path(my_directory, basename(file_name)))


## -----------------------------------------------------------------------------
file_name <- system.file("files", "simple", "spin.R_nolint",
                         package = "rasciidoc")
cat(readLines(file_name), sep = "\n")


## -----------------------------------------------------------------------------
withr::with_dir(tempdir(), {
                    file.copy(file_name, ".", overwrite = TRUE)
                    file.copy(file.path(dirname(file_name), "src"), ".",
                              recursive = TRUE)
                    rasciidoc::render(basename(file_name))
})

