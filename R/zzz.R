.onLoad <- function(libname, pkgname) { # Exclude Linting
    tools::vignetteEngine("rasciidoc", package = pkgname,
                          pattern = "[.][Rr]asciidoc$",
                          weave = vweave,
                          tangle = vtangle
                          )
}
