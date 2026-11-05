# Packages and shared settings ---------------------------------------------------
pkgs <- c("dplyr", "readr", "tidyr", "ggplot2", "survey", "scales")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install these packages first: ", paste(missing_pkgs, collapse = ", "))
}
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

SEED <- 2021
B <- 5000        # Monte Carlo replicates per design
