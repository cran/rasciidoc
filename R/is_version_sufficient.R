#' Is a Version Requirement Met?
#'
#' Just a wrapper to \code{\link{compareVersion}}, I regularly forget how to use
#' it.
#' @param installed The version installed.
#' @param required The version required.
#' @return  \code{\link{TRUE}}, if so,  \code{\link{FALSE}} otherwise.
#' @export
#' @keywords internal
#' @examples
#' is_version_sufficient(installed = "1.0.0", required = "2.0.0")
#' is_version_sufficient(installed = "1.0.0", required = "1.0.0")
is_version_sufficient <- fritools::is_version_sufficient
