# ------------------------------------------------------------------------------
# common.R — shared set-up sourced by every paper script
#
# Loads packages, the GCAM-KSA scenario database, constants, the plotting
# theme and all data-preparation functions. Paper scripts call
# `source(file.path(REPO_ROOT, "R", "common.R"))` and then only assemble figures.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(rgcam)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})
options(dplyr.summarise.inform = FALSE)

if (!exists("REPO_ROOT")) REPO_ROOT <- "."

source(file.path(REPO_ROOT, "R", "constants.R"))
source(file.path(REPO_ROOT, "R", "theme.R"))
source(file.path(REPO_ROOT, "R", "data.R"))
source(file.path(REPO_ROOT, "R", "prep.R"))
source(file.path(REPO_ROOT, "R", "plots.R"))
source(file.path(REPO_ROOT, "R", "save.R"))
