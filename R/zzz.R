.onLoad <- function(libname, pkgname) { # Exclude Linting
    tools::vignetteEngine("rasciidoc", package = pkgname,
                          pattern = "[.][Rr]asciidoc$",
                          weave = vweave,
                          tangle = vtangle
                          )
    fritools::run_r_tests_for_known_hosts()
}
