# ==============================================================================
# Reproduce every figure for all papers.
#   Rscript run_all.R
# Each paper script runs in a fresh R process, exactly as when run on its own;
# its figures/ and source_data/ folders are regenerated from scratch.
# ==============================================================================

if (!file.exists(file.path("R", "common.R"))) stop("Run from the repository root.")

papers <- list.dirs("papers", recursive = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")
failed <- character(0)
for (p in papers) {
  unlink(file.path(p, c("figures", "source_data")), recursive = TRUE)
  status <- system2(rscript, file.path(p, "make_figures.R"))
  if (status != 0) failed <- c(failed, p)
}

suppressPackageStartupMessages({ library(rgcam); library(dplyr); library(tidyr); library(ggplot2); library(patchwork) })
writeLines(capture.output(sessionInfo()), "session_info.txt")

n <- length(list.files(file.path(papers, "figures"), pattern = "\\.png$"))
message(sprintf("\n%d figures written across %d papers.", n, length(papers)))
if (length(failed)) stop("Failed: ", paste(failed, collapse = ", "))
