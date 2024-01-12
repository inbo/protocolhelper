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
    1. Commit and push any remaining changes.

### Testing a function from the package

If you need to test some changes you made to an existing function, you can probably use one of the unit tests (in folder `./tests/testthat`).
It's possible the unit test itself needs to be updated to reflect the changes you made.

If you contribute a new function, you should also consider writing unit tests for that function (or ask for help with this process).
To test the functions in this package it will often be necessary to create a protocol in an empty project in a temporary directory.
To do this, you can use the following code:

```
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")

  author_df <- data.frame(
    stringsAsFactors = FALSE,
    given = c("Hans"),
    family = c("Van Calster"),
    email = c("hans.vancalster@inbo.be"),
    orcid = c("0000-0001-8595-8426"),
    affiliation = c("Research Institute for Nature and Forest (INBO)")
  )
  reviewer_df <- data.frame(
    stringsAsFactors = FALSE,
    given = c("Els"),
    family = c("Lommelen"),
    email = c("els.lommelen@inbo.be"),
    orcid = c("0000-0002-3481-5684"),
    affiliation = c("Research Institute for Nature and Forest (INBO)")
  )
  file_manager_df <- data.frame(
    stringsAsFactors = FALSE,
    given = c("Pieter"),
    family = c("Verschelde"),
    email = c("pieter.verschelde@inbo.be"),
    orcid = c("0000-0002-9199-421X"),
    affiliation = c("Instituut voor Natuur- en Bosonderzoek (INBO)")
  )

  local_mocked_bindings(
    ui_yeah = function(...) FALSE,
    use_author = function(...) author_df,
    use_reviewer = function(...) reviewer_df,
    use_file_manager = function(...) file_manager_df,
    readline = function(...) "Een titel"
  )

  # create a protocol in the temporary directory
  version_number <- "2020.01"
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", lang = "nl"
  )
```




### Code of Conduct

Please note that the protocolhelper project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.

