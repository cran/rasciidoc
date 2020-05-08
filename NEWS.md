# rasciidoc 2.1.2

* Suppress error reporting if `source-highlight` is not installed to fix errors
  reported by solaris running r-patched on CRAN.

# rasciidoc 2.1.1

* Fixed crashing calls to `package_version()` for python release canditates.

# rasciidoc 2.1.0

* Converted vignette to Rasciidoc.
* If asciidoc is not installed (like on CRAN), rasciidoc tries to run the
  asciidoc code using python.
  This is needed for vigenttes on CRAN written in (r)asciidoc.
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

# rascidoc 0.9.0

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
