[![pipeline status](https://gitlab.com/fvafrCU/rasciidoc/badges/master/pipeline.svg)](https://gitlab.com/fvafrCU/rasciidoc/-/commits/master)    
[![coverage report](https://gitlab.com/fvafrCU/rasciidoc/badges/master/coverage.svg)](https://gitlab.com/fvafrCU/rasciidoc/-/commits/master)
<!-- 
    [![Build Status](https://travis-ci.org/fvafrCU/rasciidoc.svg?branch=master)](https://travis-ci.org/fvafrCU/rasciidoc)
    [![Coverage Status](https://codecov.io/github/fvafrCU/rasciidoc/coverage.svg?branch=master)](https://codecov.io/github/fvafrCU/rasciidoc?branch=master)
-->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rasciidoc)](https://cran.r-project.org/package=rasciidoc)
[![RStudio_downloads_monthly](https://cranlogs.r-pkg.org/badges/rasciidoc)](https://cran.r-project.org/package=rasciidoc)
[![RStudio_downloads_total](https://cranlogs.r-pkg.org/badges/grand-total/rasciidoc)](https://cran.r-project.org/package=rasciidoc)

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rasciidoc
## Introduction
Please read the
[vignette](https://fvafrCU.gitlab.io/rasciidoc/doc/An_Introduction_to_rasciidoc.html).
<!-- 
[vignette](https://CRAN.R-project.org/package=rasciidoc/vignettes/An_Introduction_to_rasciidoc.html).

-->

Or, after installation, the help page:

```r
help("rasciidoc-package", package = "rasciidoc")
```

```
#> Create Reports Using R and 'asciidoc'
#> 
#> Description:
#> 
#>      Inspired by Karl Broman`s reader on using 'knitr' with 'asciidoc'
#>      (<URL: https://kbroman.org/knitr_knutshell/pages/asciidoc.html>),
#>      this is merely a wrapper to 'knitr' and 'asciidoc'.
#> 
#> Arguments:
#> 
#> write_to_disk: Write to disk? See *Warning*.
#> 
#> Details:
#> 
#>      You will find the details in
#>      'vignette("An_Introduction_to_rasciidoc", package = "rasciidoc")'.
#> 
#> Warning:
#> 
#>      Due to the CRAN policy of not writing "anywhere else on the file
#>      system apart from the R session's temporary directory", we work on
#>      a temporary copy of 'file_name'. Thus all internal sourcing and
#>      internal links will be broken and any output is written to
#>      'tempdir()'. Set the option "write_to_disk" to TRUE (using
#>      'options(write_to_disk = TRUE)'
#>      to bypass this. You may want to include the above line into your
#>      ~/.Rprofile.
```

## Installation

You can install rasciidoc from gitlab via:


```r
if (! require("remotes")) install.packages("remotes")
remotes::install_gitlab("fvafrCU/rasciidoc")
```


