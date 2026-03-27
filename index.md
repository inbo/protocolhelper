# protocolhelper

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7181250.svg)](https://doi.org/10.5281/zenodo.7181250)

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
