if (!requireNamespace("tinytex", quietly = TRUE)) {
  stop("please install the 'tinytex' R package for these tests to work")
}
if (tinytex::tinytex_root(error = FALSE) == "") {
  stop(
    "You need a TeX Live installation on your system.",
    "Run tinytex::install_tinytex() in a clean R session to install it."
  )
} else {
  tex_pkgs <- c(
    "amscls", "amsfonts", "amsmath", "atbegshi", "atveryend", "auxhook",
    "babel", "bibtex", "bigintcalc", "bitset", "bookmark", "booktabs",
    "caption", "cm", "colortbl", "ctablestack", "datetime2", "dehyph",
    "doi", "draftwatermark", "dvipdfmx", "dvips", "ec", "emptypage",
    "environ", "epstopdf", "epstopdf-pkg", "eso-pic", "etex", "etexcmds",
    "etoolbox", "euenc", "extractbb", "extsizes", "fancyhdr", "fancyvrb",
    "filehook", "firstaid", "float", "fontawesome5", "fontspec",
    "footmisc", "fp", "framed", "geometry", "gettitlestring", "glyphlist",
    "graphics", "graphics-cfg", "graphics-def", "grfext", "grffile",
    "helvetic", "hycolor", "hyperref", "hyph-utf8", "hyphen-base",
    "hyphen-dutch", "hyphen-french", "iftex", "inconsolata", "infwarerr",
    "intcalc", "knuth-lib", "koma-script", "kpathsea", "kvdefinekeys",
    "kvoptions", "kvsetkeys", "l3backend", "l3kernel", "l3packages",
    "lastpage", "latex", "latex-amsmath-dev", "latex-bin", "latex-fonts",
    "latex-tools-dev", "latexconfig", "latexmk", "letltxmacro", "lineno",
    "lm", "lm-math", "ltxcmds", "lua-alt-getopt", "lua-uni-algos",
    "luahbtex", "lualatex-math", "lualibs", "luaotfload", "luatex",
    "luatexbase", "makecell", "mdwtools", "metafont", "mfware", "modes",
    "multirow", "natbib", "needspace", "oberdiek", "orcidlink", "parskip",
    "pdfescape", "pdflscape", "pdfpages", "pdftex", "pdftexcmds",
    "pgf", "placeins", "plain", "psnfss", "refcount", "rerunfilecheck",
    "scheme-infraonly", "selnolig", "stringenc", "symbol", "tabu",
    "tex", "tex-ini-files", "texlive-scripts", "texlive-scripts-extra",
    "texlive.infra", "threeparttable", "threeparttablex", "times",
    "tipa", "titlesec", "tocloft", "tools",
    "tracklang", "trimspaces", "ulem", "unicode-data", "unicode-math",
    "uniquecounter", "url", "varwidth", "wrapfig", "xcolor", "xetex",
    "xetexconfig", "xkeyval", "xpatch", "xunicode", "zapfding"
  )
  installed_tex_pkgs <- tinytex::check_installed(tex_pkgs)
  tinytex::tlmgr_install(tex_pkgs[!installed_tex_pkgs])
}
