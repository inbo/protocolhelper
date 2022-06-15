
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![GitHub](https://img.shields.io/github/license/inbo/protocolhelper)
[![R build
status](https://github.com/inbo/protocolhelper/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/protocolhelper/actions)
[![Codecov test
coverage](https://app.codecov.io/gh/inbo/protocolhelper/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/protocolhelper?branch=main)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/protocolhelper.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/protocolhelper.svg)

# protocolhelper

The goal of protocolhelper is to provide helper functions and templates
to manage the [INBO/protocolsource](https://github.com/inbo/protocolsource) repository.

## Installation

You can install protocolhelper with:

``` r
# Enable inbo r-universe
options(repos = c(
    inbo = 'https://inbo.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install the package
install.packages('protocolhelper')

# alternatively: install from github
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("inbo/protocolhelper")
```
