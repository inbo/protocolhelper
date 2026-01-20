# modified to cope with windows build on inbo r-universe (no stop() calls)
# Default: Skip TeX tests unless we have a working environment
skip_tex <- TRUE

# Check the Environment Variable from .Renviron
allow_install <- identical(Sys.getenv("PROTOCOL_INSTALL_TEX"), "true")

# Quietly check for the tinytex package
if (requireNamespace("tinytex", quietly = TRUE)) {
  # Check if a TeX distribution is actually detected on the system path
  # error = FALSE to ensure this line never crashes the test suite
  tex_path <- tinytex::tinytex_root(error = FALSE)

  if (tex_path != "") {

    # Check for ONE core package to see if the environment is "ready"
    if (!tinytex::check_installed("amsmath")) {
      if (allow_install) {
        # Only runs if you manually enabled it
        try(install_protocol_tex())
      }
    }

    # Final verification: If amsmath is there, we assume we can proceed
    if (isTRUE(tinytex::check_installed("amsmath"))) {
      skip_tex <- FALSE
    }
  }
}

# Global option for use in test_that() blocks
options(protocolhelper.skip_tex = skip_tex)

if (skip_tex && !allow_install) {
  cli::cli_alert_info(
    "Skipping TeX tests. To enable them locally,
    make sure you have a working TeX environment and
    add {.code PROTOCOL_INSTALL_TEX=true}
    to your {.file .Renviron} and restart R."
  )
}
