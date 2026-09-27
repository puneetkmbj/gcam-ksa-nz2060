# ==============================================================================
# Figures for:
#   Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2023).
#   Long-Term Decarbonization Pathway for Saudi Arabia To Reach Net Zero GHG
#   Emissions by 2060. KAPSARC Discussion Paper. https://doi.org/10.30573/KS--2023-DP30
#
# Run from anywhere inside the repository:
#   Rscript papers/ks2023-dp30_long-term-pathway/make_figures.R
# ==============================================================================

REPO_ROOT <- local({
  d <- normalizePath(getwd())
  while (!file.exists(file.path(d, "R", "common.R"))) {
    if (dirname(d) == d) stop("Run this script from inside the repository.")
    d <- dirname(d)
  }
  d
})
source(file.path(REPO_ROOT, "R", "common.R"))
PAPER_DIR <- file.path(REPO_ROOT, "papers", "ks2023-dp30_long-term-pathway")
message("KS--2023-DP30  Long-term decarbonization pathway")

# Fig. 1 is a model schematic.

# ---- Fig. 2  GHG pathways, with the 2030 NDC gap -------------------------------
ghg <- ghg_total()
at <- function(s, y) ghg$value[ghg$scenario == s & ghg$year == y]
gap2030 <- tibble(
  item  = c("NDC target", "Current Policy", "Gap"),
  value = c(-NDC_2030_REDUCTION,
            at("Current Policy", 2030) - at("No Policy", 2030),
            (at("Current Policy", 2030) - at("No Policy", 2030)) + NDC_2030_REDUCTION),
  Units = "MtCO2e relative to No Policy, 2030"
)
inset <- ggplot(gap2030 %>% filter(item != "Gap"), aes(item, value)) +
  geom_col(fill = "#8FA8DC", width = 0.5) +
  geom_text(aes(label = round(value)), vjust = 1.4, size = 3) +
  scale_y_continuous(limits = c(-320, 0)) +
  labs(x = NULL, y = NULL, title = sprintf("2030 vs No Policy: gap %.0f", -gap2030$value[3])) +
  theme_ksa(base_size = 8) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), plot.margin = margin(2, 2, 2, 2))
p2 <- plot_ghg_pathway(ghg) + inset_element(inset, left = 0.03, bottom = 0.03, right = 0.3, top = 0.4)
save_figure(p2, "fig02_ghg_emissions", list(a = ghg, inset = gap2030), width = 8, height = 6)

# ---- Fig. 3  Decomposition of the 2060 No Policy -> NZ 2060 reduction ----------
# External input: sector contributions come from single-measure sensitivity runs.
dec <- ghg_decomposition()
steps <- c("Power", "Buildings", "Industry", "Transport", "LUC", "Other GHGs", "Resource Production")
cats  <- c("Emissions in 2015", "Emissions in 2060", steps, "CDR")
xlab  <- c("GHG Emissions\nin 2015", "GHG Addition\nby 2060", steps, "CDR")
base15 <- sum(dec$value_MtCO2eq[dec$group == "Emissions in 2015"])
add60  <- sum(dec$value_MtCO2eq[dec$group == "Emissions in 2060"])
# Level at the top of each column: 2015 bar and CDR start at zero, the addition
# sits on the 2015 total, each measure hangs from the level left by the previous one.
reduction <- sapply(steps, function(g) sum(dec$value_MtCO2eq[dec$group == g]))
top <- c(base15, base15 + add60, base15 + add60 - c(0, head(cumsum(reduction), -1)))
wf <- dec %>%
  mutate(x = match(group, cats)) %>%
  group_by(group) %>%
  mutate(rising = group %in% c("Emissions in 2015", "Emissions in 2060", "CDR"),
         start  = case_when(group == "Emissions in 2015" ~ 0,
                            group == "Emissions in 2060" ~ base15,
                            group == "CDR" ~ 0,
                            TRUE ~ top[x[1]]),
         ymin = ifelse(rising, start + cumsum(value_MtCO2eq) - value_MtCO2eq,
                       start - cumsum(value_MtCO2eq)),
         ymax = ymin + value_MtCO2eq) %>%
  ungroup() %>%
  mutate(fill = ifelse(group %in% c("Emissions in 2015", "Emissions in 2060"),
                       ifelse(component == "CO2 Emissions", "CO2", "Other GHGs (base)"), group))
pal_dec <- c("CO2" = "#B7AEAE", "Other GHGs (base)" = "#E3DEDE", "Power" = "#D4A017",
             "Buildings" = "#F2A97E", "Industry" = "#5874B8", "Transport" = "#8E6BB0",
             "LUC" = "#7CB342", "Other GHGs" = "#D98CC0", "Resource Production" = "#7B1F6A",
             "CDR" = "#3AA8E0")
p3 <- ggplot(wf) +
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, ymin = ymin, ymax = ymax, fill = fill),
            colour = "white", linewidth = 0.3, show.legend = FALSE) +
  geom_text(aes(x = x + 0.43, y = (ymin + ymax) / 2,
                label = ifelse(group %in% steps, sprintf("%s  %.0f", component, value_MtCO2eq), "")),
            hjust = 0, size = 2.3, lineheight = 0.8) +
  geom_text(aes(x = x, y = (ymin + ymax) / 2,
                label = ifelse(!group %in% steps & value_MtCO2eq >= 40, round(value_MtCO2eq), "")),
            size = 2.8) +
  scale_fill_manual(values = pal_dec) +
  scale_x_continuous(breaks = seq_along(cats), labels = xlab, expand = expansion(add = c(0.5, 0.9))) +
  scale_y_continuous(limits = c(0, 1600), breaks = seq(0, 1600, 200), expand = c(0, 0)) +
  labs(x = NULL, y = expression(Mt ~ CO[2] * eq),
       title = "Breakdown of emission reduction from the No Policy baseline in 2060") +
  theme_ksa() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))
save_figure(p3, "fig03_ghg_reduction_decomposition",
            wf %>% select(group, component, value_MtCO2eq, ymin, ymax), width = 14, height = 7)

# ---- Fig. 4  End-use electricity demand ----------------------------------------
ed <- elec_demand()
save_figure(plot_elec_demand(ed), "fig04_electricity_demand", ed, width = 8, height = 6)

# ---- Fig. 5  Final energy (top) and sectoral shares, CP and NZ 2060 (bottom) ----
fe  <- final_energy_total()
fes <- final_energy_by_sector() %>% filter(scenario %in% c("Current Policy", "NZ 2060"))
p5a <- plot_scenario_lines(fe, "EJ", limits = c(0, 16), breaks = seq(0, 16, 2))
p5b <- plot_fe_share_sector(fes, facet = TRUE)
save_figure(p5a / p5b, "fig05_final_energy", list(a = fe, b = fes), width = 9, height = 10)
