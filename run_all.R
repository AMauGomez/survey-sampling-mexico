# Reproduce everything. Run from the repository root:  Rscript run_all.R
# First run downloads ~40 MB of public data from INE and INEGI.
source("R/00_setup.R")
source("R/01_get_data.R")
source("R/02_ine_frame.R")
source("R/03_ine_designs.R")
source("R/04_envi_estimates.R")
source("R/05_figures.R")
