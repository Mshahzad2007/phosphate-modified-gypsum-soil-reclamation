# Install the R packages needed to regenerate the archived figures.
# From the cloned repository root:
#   Rscript R/install_dependencies.R
#
# Map boundary data for Supplementary Figure S01 require an extra GitHub
# package. To install that optional resource:
#   INSTALL_MAP_HIRES=1 Rscript R/install_dependencies.R
# On Windows PowerShell:
#   $env:INSTALL_MAP_HIRES = "1"; Rscript R/install_dependencies.R
#
# This script installs software only. It does not modify the experimental
# files or any figure scripts.

cran_packages <- c(
  "readxl", "readr", "dplyr", "tidyr", "stringr", "tibble",
  "ggplot2", "patchwork", "scales", "forcats", "viridis",
  "viridisLite", "janitor", "purrr", "minpack.lm", "openxlsx",
  "vegan", "writexl", "cowplot", "gridExtra", "png", "jpeg",
  "multcompView", "broom", "Matrix", "sf", "rnaturalearth",
  "rnaturalearthdata", "soiltexture", "ggplotify", "remotes"
)

missing <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly=TRUE)
]

if (length(missing)) {
  message("Installing ", length(missing), " required CRAN package(s): ",
          paste(missing, collapse=", "))
  install.packages(missing, repos="https://cloud.r-project.org")
} else {
  message("All required CRAN packages are already available.")
}

unavailable <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly=TRUE)
]
if (length(unavailable)) {
  stop("Packages unavailable after installation: ",
       paste(unavailable, collapse=", "),
       ". See your R installation and system-library requirements.")
}

if (identical(Sys.getenv("INSTALL_MAP_HIRES"), "1")) {
  if (!requireNamespace("rnaturalearthhires", quietly=TRUE)) {
    remotes::install_github("ropensci/rnaturalearthhires", upgrade="never")
  }
  if (!requireNamespace("rnaturalearthhires", quietly=TRUE)) {
    stop("rnaturalearthhires did not install. Figure S01 needs high-resolution map data.")
  }
}

message("Dependency setup complete.")
message("R version: ", R.version.string)
