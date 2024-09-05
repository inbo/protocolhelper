if (!requireNamespace("gert", quietly = TRUE)) {
  stop("please install 'gert' package for these tests to work")
}
if (!requireNamespace("tinytex", quietly = TRUE)) {
  stop("please install the 'tinytex' R package for these tests to work")
}
if (tinytex::tinytex_root(error = FALSE) == "") {
  stop("You need a TeX Live installation on your system.",
       "Run tinytex::install_tinytex() in a clean R session to install it.")
} else {
  tex_pkgs <- c("booktabs", "orcidlink", "pgf")
  installed_tex_pkgs <- tinytex::check_installed(tex_pkgs)
  tinytex::tlmgr_install(tex_pkgs[!installed_tex_pkgs])
}
