# ==============================================================================
# Figures for:
#   Kamboj, P., Hejazi, M., Alhadhrami, K., Qiu, Y., Kyle, P., Iyer, G. (2023).
#   Saudi Arabia Net Zero GHG Emissions by 2060: Transformation of the Electricity
#   Sector. KAPSARC Discussion Paper. https://doi.org/10.30573/KS--2023-DP31
#
# Run from anywhere inside the repository:
#   Rscript papers/ks2023-dp31_electricity/make_figures.R
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
PAPER_DIR <- file.path(REPO_ROOT, "papers", "ks2023-dp31_electricity")
message("KS--2023-DP31  Electricity sector")

# Figs. 1-2 are model schematics.

# ---- Fig. 3  Economy-wide GHG (a) and NZ 2060 power-sector CO2 (b) --------------
ghg  <- ghg_total()
eco2 <- co2_by_assigned_sector() %>% filter(sector == "Electricity", scenario == "NZ 2060")
p3a <- plot_ghg_pathway(ghg) + labs(title = "KSA GHG Emissions")
p3b <- ggplot(eco2, aes(factor(year), value)) +
  geom_col(fill = "#FEE12B", width = 0.7) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50), expand = c(0, 0)) +
  labs(x = NULL, y = expression(Mt ~ CO[2]),
       title = expression(NZ ~ 2060 - Electricity ~ Generation ~ CO[2] ~ Emissions)) +
  theme_ksa()
save_figure(p3a + p3b + plot_layout(widths = c(1.2, 1)), "fig03_ghg_and_power_co2",
            list(a = ghg, b = eco2), width = 14, height = 6)

# ---- Fig. 4  Electricity generation by technology -------------------------------
gen <- elec_gen_by_tech() %>% filter(value > 0)
save_figure(plot_elec_gen_tech(gen, ncol = 2), "fig04_electricity_generation_by_technology",
            gen, width = 10, height = 10)

# ---- Fig. 5  Installed capacity (a) and new-capacity investment (b) -------------
# External input: capacity and investment were post-processed outside the model
# database (generation / capacity factor; capital cost x new capacity).
cap_inv <- power_capacity_investment() %>%
  mutate(technology = factor(technology, levels = names(PAL_ELEC_TECH)),
         scenario = factor(scenario, levels = SCENARIO_LEVELS))
cap <- cap_inv %>% filter(type == "cumulative_capacity")
inv <- cap_inv %>% filter(type == "investment")
bars <- function(d, ylab) {
  ggplot(d, aes(year, value, fill = technology)) +
    geom_col(width = 0.5) +
    facet_wrap(~ scenario) +
    scale_fill_manual(values = PAL_ELEC_TECH, drop = TRUE) +
    scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100), expand = c(0, 0)) +
    labs(x = NULL, y = ylab) +
    theme_ksa() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    guides(fill = guide_legend(nrow = 1))
}
p5 <- bars(cap, "Total installed capacity (GW)") /
      bars(inv, "New capacity investment\n(billion 2020 USD)") +
      plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_figure(p5, "fig05_capacity_and_investment", list(capacity = cap, investment = inv),
            width = 10, height = 9)

# ---- Fig. 6  Total electricity generation, Current Policy vs NZ 2060 ------------
gtot <- elec_gen_total() %>% filter(scenario %in% c("Current Policy", "NZ 2060"))
cp <- gtot$value[gtot$scenario == "Current Policy" & gtot$year == 2060]
nz <- gtot$value[gtot$scenario == "NZ 2060" & gtot$year == 2060]
p6 <- plot_scenario_lines(gtot, "TWh", limits = c(0, 1600), breaks = seq(0, 1500, 250),
                          label_end = c("Current Policy", "NZ 2060")) +
  annotate("segment", x = 2060, xend = 2060, y = cp, yend = nz, colour = "blue",
           linewidth = 0.3, arrow = arrow(length = unit(0.08, "inches"))) +
  annotate("text", x = 2059.5, y = (cp + nz) / 2, label = sprintf("%.0fX", nz / cp),
           colour = "blue", hjust = 1, size = 3.5) +
  labs(title = "KSA Electricity Generation")
save_figure(p6, "fig06_total_electricity_generation", gtot, width = 8, height = 6)

# ---- Fig. 7  Final energy by fuel share, NZ 2060 --------------------------------
# Fuels below 0.05% (biomass) are omitted, as in the published figure.
fuel <- final_energy_by_fuel() %>%
  filter(scenario == "NZ 2060", year %in% c(2015, 2060), share >= 0.05)
p7 <- ggplot(fuel, aes(factor(year), share, fill = fuel)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = PAL_FE_FUEL, drop = TRUE) +
  scale_y_continuous(limits = c(0, 100.01), breaks = seq(0, 100, 25), expand = c(0, 0)) +
  labs(x = NULL, y = "Final Energy Share by Fuel (%)", title = "Final Energy Share by Fuel, NZ 2060") +
  theme_ksa(legend_position = "right")
save_figure(p7, "fig07_final_energy_fuel_share", fuel, width = 7, height = 6)
