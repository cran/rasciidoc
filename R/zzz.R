.onLoad <- function(libname, pkgname) {
    tools::vignetteEngine("rasciidoc", package=pkgname,
                          pattern="[.][Rr]asciidoc$",
                          weave=vweave,
                          tangle=vtangle
                          )
}
