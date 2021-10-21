\name{NEWS}
\title{NEWS}

\section{Changes in version 3.1.4}{
\itemize{
\item Skip tests on unkown machines
}
}

\section{Changes in version 3.1.3}{
\itemize{
\item Skip tests on unkown windows machines to avoid errors for CRAN's \code{r-devel-windows-x86_64-gcc10-UCRT}.
}
}

\section{Changes in version 3.1.2}{
\itemize{
\item Removed LazyData from file DESCRIPTION.
}
}

\section{Changes in version 3.1.1}{
\itemize{
\item Extended testing on spinning and slides.
\item Fixed missing asciidoc installation:
\itemize{
\item Now resetting asciidoc's github repo to the last tagged release version.
}
\item Now using a system's installation of git, if available, to clone asciidoc if
needed.
\item Fixed comparison of paths by normalizing them. Formerly, long (e.g. temporary)
paths (at least on windows) got abbreviated, thus failing to compare.
}
}

\section{Changes in version 3.1.0}{
\itemize{
\item Use \code{tools} instead of \code{xfun}.
\item Fixed missing asciidoc installation:
\itemize{
\item Adapted to new github url for asciidoc.
}
}
}

\section{Changes in version 3.0.2}{
\itemize{
\item Fix: catching strange \code{tempdir()}s on CRAN winbuilder on \code{x86_64-w64-mingw32}.
}
}

\section{Changes in version 3.0.1}{
\itemize{
\item Switched from \code{git2r} to \code{gert}.
\item Added an example to properly create plots with \code{asciidoc} to the minimal
example in the vignette.
\item Now importing package \code{fritools}.
}
}

\section{Changes in version 3.0.0}{
\itemize{
\item \strong{\code{rasciidoc()}'s return value changed: it now gives \code{TRUE} on success and
\code{FALSE} otherwise.}
\item \code{rasciidoc()} now catches all system errors and warnings from \code{system2()}.
A missing installation of the recommended \code{source-highlight} does not throw a
condition, so albeit the rendered output is not what you might expect it to
be, the call to \code{asciidoc} is considered successful.
\item Skip tests using \code{asciidoc} as CRAN's Solaris 10 (r-patched-solaris-x86) fails
with "asciidoc: FAILED: configuration file asciidoc.conf missing": there seems
to be a buggy asciidoc installation I cannot bypass.
}
}

\section{Changes in version 2.2.2}{
\itemize{
\item \code{rasciidoc()} now does not give \code{warnings()} or throws errors for missing SystemRequirements,
adhering to "Writing R Extensions", version 4.0.3 (2020-10-10):
"A package should pass its checks without warnings nor errors without the external command being present."
To restore the warnings and errors, pass argument \code{enforce_requirements = TRUE} to
\code{render()} (or \code{rasciidoc()}) or set \code{options("enforce_requirements" = TRUE)}
to restore the original behaviour.
}
}

\section{Changes in version 2.2.1}{
\itemize{
\item Added checkmate's argument checks to all exported functions (\code{rasciidoc()},
\code{render()}, and \code{adjust_asciidoc_hooks()}).
\item Added the vignette.
}
}

\section{Changes in version 2.2.0}{
\itemize{
\item Added internal \code{discover_python()} to deal with python installations that
will not be found using \code{Sys.which("python")}.
}
}

\section{Changes in version 2.1.3}{
\itemize{
\item Removed vignette written in rasciidoc to avoid errors on
CRAN/solaris/R-patched.
\item Added function \code{is_windows()} to suppress running examples with elapsed times
on CRAN/windows.
}
}

\section{Changes in version 2.1.2}{
\itemize{
\item Suppress error reporting if \code{source-highlight} is not installed to fix errors
reported by solaris running r-patched on CRAN.
}
}

\section{Changes in version 2.1.1}{
\itemize{
\item Fixed crashing calls to \code{package_version()} for python release candidates.
}
}

\section{Changes in version 2.1.0}{
\itemize{
\item Converted vignette to Rasciidoc.
\item If asciidoc is not installed (like on CRAN), rasciidoc tries to run the
asciidoc code using python.
This is needed for vignettes on CRAN written in (r)asciidoc.
\item Added a vignette engine.
}
}

\section{Changes in version 2.0.1}{
\itemize{
\item Fixed link to online vignette.
\item Fixed tests for new version of package coverage.
}
}

\section{Changes in version 2.0.0}{
\itemize{
\item Fixed quotation in DESCRIPTION.
\item Fixed excerption for input files mixing slidy and standard asciidoc.
\item Refactored render().
\item Got rid of render\_slides().
\item Remove tagged lines from input docs.
\item Now by default using tempdir(), which breaks internal links and source().
}
}

\section{Changes in version 1.0.0}{
\itemize{
\item Improve testing.
\item Checked for spelling, lints and cleanr.
\item Allow for skipping single lines in slidy output by tagging them with
"//no_slide".
\item Now keeping intermediate files from render\_slides(), too.
}
}

\section{Changes in version 0.9.0}{
\itemize{
\item knitr's output hooks are now reset to their former values after knitting.
\item Added an argument 'clean' to render() to clean intermediate files.
}
}

\section{Changes in version 0.8.0}{
\itemize{
\item render() now passed arguments to adjust knitr's asciidoc hooks.
}
}

\section{Changes in version 0.7.0}{
\itemize{
\item render() now uses a working directory that defaults to the input file's
directory, allowing for the default file to source code.
}
}

\section{Changes in version 0.6.0}{
\itemize{
\item render() now works for R files with markup in roxygen comments that are
parsed trough knitr::spin().
}
}

\section{Changes in version 0.5.0}{
\itemize{
\item Passed the parent.frame() down to knitr to always be in .GlobalEnv.
}
}

\section{Changes in version 0.4.0}{
\itemize{
\item Add \verb{//[begin|end]\\_only\\_slide}-blocks and \verb{//slide\\_only}-comments to allow
for content for slidy only.
\item Fix broken code inclusions for slidy.
}
}

\section{Changes in version 0.3.0}{
\itemize{
\item Made adjusting knitr's hooks (see rasciidoc 0.2.0) the default behaviour.
}
}

\section{Changes in version 0.2.0}{
\itemize{
\item Added function to adjust knitr's hooks for asciidoc files, providing a work
around for not using asciidoc's \link{MESSAGE|WARNING|ERROR} when knitting produces
a message|warning|error.
}
}

\section{Changes in version 0.1.0}{
\itemize{
\item Added core functionality.
}
}

