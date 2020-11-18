is_R_CMD_check <- function () {
    ("CheckExEnv" %in% search()) || 
        any(c("_R_CHECK_TIMINGS_", "_R_CHECK_LICENSE_") %in% 
            names(Sys.getenv()))
}

vtangle <- function (file, ..., encoding = "UTF-8", quiet = FALSE) {
    if (is_R_CMD_check()) {
        file = xfun::with_ext(file, "R")
        file.create(file)
        return(file)
    }
    knitr::purl(file, encoding = encoding, quiet = quiet, ...)
}

vweave <- function(file, ...) {
    render(file, envir = globalenv(), working_directory = getwd(), 
           write_to_disk = TRUE, ...)
}


