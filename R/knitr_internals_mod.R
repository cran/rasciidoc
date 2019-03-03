# Verbatim copy of internal from package knitr version 1.18.7, 
# referencing knitr via `::`.
set_header = function(...) {
    knitr::opts_knit$set(header = merge_list(knitr::opts_knit$get('header'), 
                                             c(...)))
}


# Verbatim copy of internal from package knitr version 1.18.7, 
# referencing knitr via `::` and
# replacing the calls to highr's internals with calls to the above copies.
hilight_source = function(x, format, options) {
  if ((format %in% c('latex', 'html')) && options$highlight) {
    if (options$engine == 'R') {
      opts = knitr::opts_knit$get('highr.opts')
      highr::hilight(x, format, prompt = options$prompt, markup = opts$markup)
    } else {
      res = try(highr::hi_andre(x, options$engine, format))
      if (inherits(res, 'try-error')) {
        if (format == 'html') escape_html(x) else escape_latex(x)
      } else {
        highlight_header()
        n = length(res)
        # do not touch font size
        if (res[n] == '\\normalsize') res = res[-n]
        res
      }
    }
  } else if (options$prompt) {
    # if you did not reformat or evaluate the code, I have to figure out which
    # lines belong to one complete expression first (#779)
    if (options$engine == 'R' && !options$tidy && isFALSE(options$eval))
      x = vapply(group_src(x), paste, character(1), collapse = '\n')
    line_prompt(x)
  } else x
}


#' Adjust \pkg{knitr}'s Hooks for \command{asciidoc}
#'
#' By default, \pkg{knitr} renders messages, warnings and errors to
#' [NOTE|WARNING|ERROR]-blocks in \command{asciidoc}, which is ... not my
#' choice.
#' To restore \pkg{knitr}'s behaviour, set \code{hooks} or \code{replacement} to
#' \code{\link{NULL}}.
#'
#' This is a modified version of
#' \code{\link[knitr:render_asciidoc]{knitr::render_asciidoc}} of  \pkg{knitr}
#' version 1.18.7.
#'
#' @param hooks Character vector naming the output hooks to be replaced by
#' the \code{replacement}'s hooks.
#' @param replacement The hook with which to replace the hooks given by
#' \code{hooks}.
#' @return The return value of
#' \code{\link[knitr:knit_hooks]{knitr::knit_hooks$set}},
#' \code{\link[base:invisible]{invisibly}} \code{\link{NULL}}, currently.
#' @export 
adjust_asciidoc_hooks <- function(hooks = c("message", "error", "warning"),
                                  replacement = "source") {
    # Verbatim copy of a part of knitr::render_asciidoc() version 1.18.7, 
    # formatted to fit lines of length 80 and replace assignment by "=" with
    # assignment by "<-" to soothe lintr.
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
    # Modification starts here.
    if (! is.null(replacement)) {
        replacement_hook <- get(paste0("hook.", replacement))
        for (i in hooks) {
            assign(paste0("hook.", i), replacement_hook)
        } 
    }
    res <- knitr::knit_hooks$set(source = hook.source, output = hook.output, 
                                 message = hook.message, warning = hook.warning,
                                 error = hook.error, 
                                 plot = knitr::hook_plot_asciidoc)
    return(invisible(res))
}
