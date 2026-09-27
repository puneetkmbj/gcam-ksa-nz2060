# ------------------------------------------------------------------------------
# save.R — write a figure and the exact data behind it
#
# Every figure is written as <paper>/figures/<name>.png (300 dpi) and the data
# plotted in it as <paper>/source_data/<name>.csv, so values can be checked or
# re-plotted without R.
# ------------------------------------------------------------------------------

save_figure <- function(plot, name, data, width, height, dir = PAPER_DIR, dpi = 300) {
  fig_dir <- file.path(dir, "figures")
  csv_dir <- file.path(dir, "source_data")
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)

  ggsave(file.path(fig_dir, paste0(name, ".png")), plot,
         width = width, height = height, dpi = dpi, bg = "white")

  if (is.data.frame(data)) data <- list(data)
  sfx <- if (length(data) > 1) paste0("_", names(data) %||% letters[seq_along(data)]) else ""
  for (i in seq_along(data)) {
    out <- data[[i]] %>% ungroup() %>% mutate(across(where(is.numeric), ~ signif(.x, 6)))
    utils::write.csv(out, file.path(csv_dir, paste0(name, sfx[i], ".csv")), row.names = FALSE)
  }
  message("  wrote ", name)
  invisible(plot)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
