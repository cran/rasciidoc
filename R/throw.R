#' Throw a Condition
#'
#' Throws a condition of class c("error", "rasciidoc", "condition").
#'
#' We use this condition as an error dedicated to \pkg{ rasciidoc.}
#'
#' @param message_string The message to be thrown.
#' @param system_call The call to be thrown.
#' @param ... Arguments to be passed to
#' \code{\link[base:structure]{base::structure}}.
#' @return The function does never return anything, it stops with a
#' condition of class c("error", "rasciidoc", "condition").
#' @keywords internal
#' @examples
#' tryCatch(rasciidoc:::throw("Hello error!"),
#'          rasciidoc = function(e) return(e))
throw <- function(message_string, system_call = sys.call(-1), ...) {
    condition <- structure(class = c("error", "rasciidoc", "condition"),
                           list(message = message_string, call = system_call),
                           ...)
    stop(condition)
}
