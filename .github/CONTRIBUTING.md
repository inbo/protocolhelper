# Contributing to protocolhelper

This outlines how to propose a change to protocolhelper. For more detailed
info about contributing to this package, please see the
[**development contributing guide**](https://rstd.io/tidy-contrib).

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
for documentation.  
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.  
*  For user-facing changes, add a bullet to the top of `NEWS.md` below the
current development version header describing the changes made followed by your
GitHub username, and links to relevant issue(s)/PR(s).
* We use [checklist](https://github.com/inbo/checklist) to check if the package passes all quality checks. To check for errors, warnings and notes when working in a local branch, you can proceed as follows:

    1. [_Build_](https://r-pkgs.org/whole-game.html#install) the package.
    1. Run `x <- checklist::check_package()`.
      Fix any issues that arise during the checks.
    1. Run `x <- checklist::check_package()` again until you get _No problems found_ at the end of the checklist output.
    1. Commit any remaining changes.


### Code of Conduct

Please note that the protocolhelper project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.

