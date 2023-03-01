
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: The project has reached a stable, usable state and is
being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![GitHub](https://img.shields.io/github/license/inbo/protocolhelper)
[![R build
status](https://github.com/inbo/protocolhelper/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/protocolhelper/actions)
[![codecov](https://codecov.io/gh/inbo/protocolhelper/branch/main/graph/badge.svg?token=6W8WRNAZPA)](https://app.codecov.io/gh/inbo/protocolhelper)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/protocolhelper.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/protocolhelper.svg)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7181250.svg)](https://doi.org/10.5281/zenodo.7181250)

# protocolhelper

The goal of protocolhelper is to provide helper functions and templates
to manage the
[INBO/protocolsource](https://github.com/inbo/protocolsource)
repository.

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
