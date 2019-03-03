if (interactive()) devtools::load_all()
test_try_parse <- function() {
    RUnit::checkTrue(rasciidoc:::try_parse("asdf"))
    RUnit::checkTrue(! rasciidoc:::try_parse("a(df"))
}

test_escape_latex <- function() {
    RUnit::checkIdentical(rasciidoc:::escape_latex("\\{"),
                          "\\textbackslash{}\\{")
}

test_escape_html <- function() {
    RUnit::checkIdentical(rasciidoc:::escape_html("& < > \""),
                          "&amp; &lt; &gt; &quot;")
}

test_group_src <- function()  {
    # adapted from highr/tests/testit/test-utils.R
    RUnit::checkIdentical(rasciidoc:::group_src("1+1"), list("1+1"))
    RUnit::checkIdentical(rasciidoc:::group_src(c("1+1+", "1")),
                          list(c("1+1+", "1")))
    RUnit::checkIdentical(rasciidoc:::group_src(c("1+1+", "1", "TRUE")),
                          list(c("1+1+", "1"), "TRUE"))
}
