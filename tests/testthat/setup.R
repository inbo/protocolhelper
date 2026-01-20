# Quietly check for the tinytex package
if (requireNamespace("tinytex", quietly = TRUE)) {
  # Check if a TeX distribution is actually detected on the system path
  # error = FALSE to ensure this line never crashes the test suite
  tex_path <- tinytex::tinytex_root(error = FALSE)
  if (tex_path != "") {
    try(install_protocol_tex())
  } else {
    cli::cli_alert_info(
      "You need a TeX Live installation on your system. Run
      {.code tinytex::install_tinytex()} in a clean R session to install it."
    )
  }
} else {
  cli::cli_alert_info("tinytex R package not installed")
}
