ignore_unused_imports <- function() {
  slickR::slickR
  reactable::reactable
}

# for mocking a base function
# https://testthat.r-lib.org/reference/local_mocked_bindings.html#base-functions
readline <- NULL
