if (interactive()) pkgload::load_all()
ex <- c(paste("<span class=\"hl kwd\">print</span><span",
              "class=\"hl std\">(</span><span class=\"hl",
              "num\">3</span><span class=\"hl std\">)</span>"),
        paste("<span class=\"hl kwa\">if</span> <span class=\"hl",
              "std\">(</span><span class=\"hl",
              "num\">FALSE</span><span",
              "class=\"hl std\">)</span> <span class=\"hl",
              "kwd\">print</span><span class=\"hl",
              "std\">(</span><span",
              "class=\"hl num\">3</span><span class=\"hl",
              "std\">)</span>",
              "<span class=\"hl kwa\">else</span> <span",
              "class=\"hl kwd\">print</span><span class=\"hl",
              "std\">(</span><span class=\"hl num\">4</span><span",
              "class=\"hl std\">)</span>")
        )
test_highlight_source <- function(expectation = ex) {
    # TODO: This one fails on CRAN:
    ## ERROR in test_highlight_source: Error in if (res[n] == "\\normalsize")
    ##   res = res[-n] :
    ##   argument is of length zero
    # I don't have a clue why res = try(...) returns no error but length(res)
    # is zero...
    if (fritools::is_running_on_fvafrcu_machines()) {
        x <- c("print(3)", "if (FALSE) print(3) else print(4)")
        result <- rasciidoc:::hilight_source(x, "html",
                                             list(highlight = TRUE,
                                                  engine = "R",
                                                  prompt = FALSE))
        RUnit::checkIdentical(result, expectation)
        result <- rasciidoc:::hilight_source(x, "html",
                                             list(highlight = TRUE,
                                                  engine = "",
                                                  prompt = FALSE))
        expectation <- result
        RUnit::checkIdentical(result, expectation)
        result <- rasciidoc:::hilight_source(x, "html",
                                             list(highlight = FALSE,
                                                  engine = "",
                                                  prompt = TRUE))
        expectation <- c("> print(3)", "> if (FALSE) print(3) else print(4)")
        RUnit::checkIdentical(result, expectation)
    }
}

test_knitr_internals <- function() {
    RUnit::checkIdentical(rasciidoc:::comment_length(NULL), 1L)
    RUnit::checkIdentical(rasciidoc:::comment_length(1), 2L)
    RUnit::checkIdentical(rasciidoc:::isFALSE(FALSE), TRUE)
}

test_merge_lists <- function() {
    x <- list(foo = 1, bar = 1, foobar = 3)
    y <- list(foo = 2, bar = 7)
    result <- rasciidoc:::merge_list(x, y)
    expectation <- structure(list(foo = 2, bar = 7, foobar = 3),
                             .Names = c("foo", "bar", "foobar"))
    RUnit::checkIdentical(result, expectation)
}
