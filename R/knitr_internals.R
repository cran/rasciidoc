# Verbatim copies of internals from package knitr version 1.18.7.
# Changed assignments from `=` to `<-` and quoting from single to double
# to soothe lintr.
merge_list <- function(x, y) {
  x[names(y)] <- y
  x
}

line_prompt <- function(x, prompt = getOption("prompt"),
                       continue = getOption("continue")) {
  # match a \n, then followed by any character (use zero width assertion)
  paste0(prompt, gsub("(?<=\n)(?=.|\n)", continue, x, perl = TRUE))
}

comment_length <- function(x) {
  (if (is.null(x) || !nzchar(x) || is.na(x)) 0L else nchar(x)) + 1L
}

highlight_header <- function() {
  set_header(highlight.extra = paste(c(
    sprintf("\\let\\hl%s\\hlstd", c("esc", "pps", "lin")),
    sprintf("\\let\\hl%s\\hlcom", c("slc", "ppc"))
  ), collapse = " "))
}
