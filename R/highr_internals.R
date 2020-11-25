# Verbatim copies of internals from package highr version 0.6.1.
# Changed assignments from `=` to `<-` and quoting from single to double
# to soothe lintr.
try_parse <- function(code, silent = TRUE) {
  !inherits(
    try(parse(text = code, keep.source = FALSE), silent = silent), "try-error"
  )
}

group_src <- function(code) {
  if ((n <- length(code)) < 1) return(list(code))
  i <- i1 <- i2 <- 1
  x <- list()
  while (i2 <= n) {
    piece <- code[i1:i2]
    if (try_parse(piece)) {
      x[[i]] <- piece; i <- i + 1
      i1 <- i2 + 1 # start from the next line
    }
    i2 <- i2 + 1
  }
  if (i1 <= n) parse(text = piece)  # must be an error there
  x
}

escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash", x)
  x <- gsub("([{}])", "\\\\\\1", x)
  gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  x <- gsub("\"", "&quot;", x)
  x
}
