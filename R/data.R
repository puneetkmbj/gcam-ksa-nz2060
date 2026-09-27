# ------------------------------------------------------------------------------
# data.R — access to the GCAM-KSA scenario database (data/ksa_nz.dat)
#
# The .dat file is an rgcam project file (xz-compressed R serialisation) holding
# 83 pre-run queries for four scenarios, region "Saudi Arabia", 1975-2060.
# ------------------------------------------------------------------------------

DATA_FILE <- file.path(REPO_ROOT, "data", "ksa_nz.dat")

if (!exists(".ksa_project")) {
  if (!file.exists(DATA_FILE)) stop("Scenario database not found: ", DATA_FILE)
  .ksa_project <- rgcam::loadProject(DATA_FILE)
}

#' Retrieve one query for all scenarios with paper scenario labels attached.
#' @param query  query title as stored in the project (see rgcam::listQueries)
#' @param min_year,max_year reporting window (some land queries run to 2100)
gq <- function(query, min_year = 2015, max_year = 2060) {
  rgcam::getQuery(.ksa_project, query) %>%
    filter(year >= min_year, year <= max_year) %>%
    mutate(scenario = factor(SCENARIO_LABELS[scenario], levels = SCENARIO_LEVELS))
}
