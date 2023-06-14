# rasciidoc 4.1.0

* Removed `rasciidoc` from field `Suggests` in DESCRIPTION.
* Bumped version due to package `fritools` back on CRAN.

# rasciidoc 4.0.2

* Fixed test for failing asciidoc command that occurs on CRAN's 
  `r-patched-solaris-x86`.

# rasciidoc 4.0.1

* Fixed typo in unit testing.

# rasciidoc 4.0.0

* **`rasciidoc()`'s return value changed: it now has an attribute `info`.**
  If you need to test whether it is `TRUE` or not, use `isTRUE().`
* Fixed testing for failing calls to python:asciidoc.

# rasciidoc 3.2.0

* In case only python3 is installed and asciidoc is missing, rasciidoc downloads
  [asciidoc from github](https://github.com/asciidoc-py/asciidoc-py).
  The current versions of asciidoc fail when called via 
  `python3 /path/to/repo/asccidoc.py /path/to/input.asciidoc`, so we **hard code
  checking out version 9.1.0, which is from 2020/02/11.**
* Extended throwing and catching conditions: rasciidoc now catches and throws
  all errors due to python not being installed. It should thus pass tests even
  if python (which is a system requirement) is not installed.
* Added tests using package testthat running basic tests on all machines.

# rasciidoc 3.1.4

* Skip tests on unknown machines.

# rasciidoc 3.1.3

* Skip tests on unknown windows machines to avoid errors for CRAN's
  `r-devel-windows-x86_64-gcc10-UCRT`.

# rasciidoc 3.1.2

* Removed LazyData from file DESCRIPTION.

# rasciidoc 3.1.1

* Extended testing on spinning and slides.
* Fixed missing asciidoc installation:
  - Now resetting asciidoc's github repo to the last tagged release version.
* Now using a system's installation of git, if available, to clone asciidoc if
  needed.
* Fixed comparison of paths by normalizing them. Formerly, long (e.g. temporary)
  paths (at least on windows) got abbreviated, thus failing to compare.

# rasciidoc 3.1.0

* Use `tools` instead of `xfun`.
* Fixed missing asciidoc installation:
  - Adapted to new github url for asciidoc.

# rasciidoc 3.0.2

* Fix: catching strange `tempdir()`s on CRAN winbuilder on `x86_64-w64-mingw32`.

# rasciidoc 3.0.1

* Switched from `git2r` to `gert`.
* Added an example to properly create plots with `asciidoc` to the minimal
  example in the vignette.
* Now importing package `fritools`.

# rasciidoc 3.0.0

* **`rasciidoc()`'s return value changed: it now gives `TRUE` on success and
  `FALSE` otherwise.**
* `rasciidoc()` now catches all system errors and warnings from `system2()`.
   A missing installation of the recommended `source-highlight` does not throw a
   condition, so albeit the rendered output is not what you might expect it to
   be, the call to `asciidoc` is considered successful.
* Skip tests using `asciidoc` as CRAN's Solaris 10 (r-patched-solaris-x86) fails
  with "asciidoc: FAILED: configuration file asciidoc.conf missing": there seems
  to be a buggy asciidoc installation I cannot bypass.


# rasciidoc 2.2.2

* `rasciidoc()` now does not give `warnings()` or throws errors for missing SystemRequirements,
  adhering to "Writing R Extensions", version 4.0.3 (2020-10-10): 
  "A package should pass its checks without warnings nor errors without the external command being present."
  To restore the warnings and errors, pass argument `enforce_requirements = TRUE` to
  `render()` (or `rasciidoc()`) or set `options("enforce_requirements" = TRUE)`
  to restore the original behaviour.


# rasciidoc 2.2.1

* Added checkmate's argument checks to all exported functions (`rasciidoc()`,
  `render()`, and `adjust_asciidoc_hooks()`).
* Added the vignette.


# rasciidoc 2.2.0

* Added internal `discover_python()` to deal with python installations that
  will not be found using `Sys.which("python")`.

# rasciidoc 2.1.3

* Removed vignette written in rasciidoc to avoid errors on
  CRAN/solaris/R-patched.
* Added function `is_windows()` to suppress running examples with elapsed times
  on CRAN/windows.

# rasciidoc 2.1.2

* Suppress error reporting if `source-highlight` is not installed to fix errors
  reported by solaris running r-patched on CRAN.

# rasciidoc 2.1.1

* Fixed crashing calls to `package_version()` for python release candidates.

# rasciidoc 2.1.0

* Converted vignette to Rasciidoc.
* If asciidoc is not installed (like on CRAN), rasciidoc tries to run the
  asciidoc code using python.
  This is needed for vignettes on CRAN written in (r)asciidoc.
* Added a vignette engine.

# rasciidoc 2.0.1

* Fixed link to online vignette.
* Fixed tests for new version of package coverage.

# rasciidoc 2.0.0

* Fixed quotation in DESCRIPTION.
* Fixed excerption for input files mixing slidy and standard asciidoc.
* Refactored render().
* Got rid of render\_slides().
* Remove tagged lines from input docs.
* Now by default using tempdir(), which breaks internal links and source().

# rasciidoc 1.0.0

* Improve testing.
* Checked for spelling, lints and cleanr.
* Allow for skipping single lines in slidy output by tagging them with
  "//no_slide".
* Now keeping intermediate files from render\_slides(), too.

# rasciidoc 0.9.0

* knitr's output hooks are now reset to their former values after knitting.
* Added an argument 'clean' to render() to clean intermediate files.

# rasciidoc 0.8.0

* render() now passed arguments to adjust knitr's asciidoc hooks.

# rasciidoc 0.7.0

* render() now uses a working directory that defaults to the input file's
  directory, allowing for the default file to source code.
# rasciidoc 0.6.0

* render() now works for R files with markup in roxygen comments that are
  parsed trough knitr::spin().

# rasciidoc 0.5.0

* Passed the parent.frame() down to knitr to always be in .GlobalEnv.

# rasciidoc 0.4.0

* Add `//[begin|end]\_only\_slide`-blocks and `//slide\_only`-comments to allow 
  for content for slidy only.
* Fix broken code inclusions for slidy.

# rasciidoc 0.3.0

* Made adjusting knitr's hooks (see rasciidoc 0.2.0) the default behaviour.

# rasciidoc 0.2.0

* Added function to adjust knitr's hooks for asciidoc files, providing a work
  around for not using asciidoc's [MESSAGE|WARNING|ERROR] when knitting produces
  a message|warning|error.

# rasciidoc 0.1.0

* Added core functionality.
