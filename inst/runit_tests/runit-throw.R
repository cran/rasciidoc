test_exception <- function() {
    RUnit::checkException(rasciidoc:::throw("Hello, error!"))
}
