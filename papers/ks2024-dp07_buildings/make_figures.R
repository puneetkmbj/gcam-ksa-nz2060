# ==============================================================================
# Figures for:
#   Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2024).
#   Saudi Arabia Net-Zero GHG Emissions by 2060: Transformation of the Building
#   Sector. KAPSARC Discussion Paper. https://doi.org/10.30573/KS--2024-DP07
#
# Run from anywhere inside the repository:
#   Rscript papers/ks2024-dp07_buildings/make_figures.R
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
PAPER_DIR <- file.path(REPO_ROOT, "papers", "ks2024-dp07_buildings")
message("KS--2024-DP07  Building sector")

# Figs. 1-2 are model schematics.

# ---- Fig. 3  Socio-economic assumptions (a) and floor space (b) -----------------
# Socio-economic drivers are common to all scenarios; Current Policy is shown.
pop <- population() %>% filter(scenario == "Current Policy")
gdp_ <- gdp() %>% filter(scenario == "Current Policy")
socio <- bind_rows(mutate(pop, variable = "Population"), mutate(gdp_, variable = "GDP"))
p3a <- ggplot() +
  geom_line(data = pop, aes(year, value, colour = "Population", linetype = "Population"), linewidth = 0.8) +
  geom_line(data = gdp_, aes(year, value * 10, colour = "GDP", linetype = "GDP"), linewidth = 0.8) +
  scale_colour_manual(NULL, values = c(Population = "#E31A1C", GDP = "#1F6FB5")) +
  scale_linetype_manual(NULL, values = c(Population = "dashed", GDP = "solid")) +
  scale_x_years() +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 10), expand = c(0, 0),
                     name = "Population (million)",
                     sec.axis = sec_axis(~ . / 10, name = "GDP (trillion 2020 SAR)", breaks = 0:9)) +
  labs(x = NULL, title = "Socioeconomic Assumptions") +
  theme_ksa()
fs <- floorspace() %>% filter(scenario == "Current Policy")
p3b <- ggplot(fs, aes(factor(year), value, fill = building)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = PAL_BLD_TYPE) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), expand = c(0, 0)) +
  labs(x = NULL, y = expression(Billion ~ m^2), title = "Total Floorspace Area") +
  theme_ksa()
save_figure(p3a + p3b, "fig03_socioeconomics_and_floorspace", list(a = socio, b = fs),
            width = 12, height = 5.5)

# ---- Fig. 4  Building final energy (a) and NZ 2060 fuel mix (b) -----------------
bfe   <- building_final_energy()
bfuel <- building_final_energy_by_fuel() %>% filter(scenario == "NZ 2060")
save_figure(plot_building_fe(bfe) + plot_building_fuel(bfuel), "fig04_building_final_energy",
            list(a = bfe, b = bfuel), width = 12, height = 5)

# ---- Fig. 5  Cooling demand (a) and electricity for cooling (b) -----------------
# External input: building service output (cooling load) and cooling electricity
# by scenario are not part of ksa_nz.dat.
cd <- cooling_demand()
ce <- cooling_electricity()
p5 <- (plot_cooling_demand(cd) + labs(title = "Cooling demand in the Kingdom")) +
      (plot_cooling_electricity(ce) + labs(title = "Electricity consumption for cooling", y = "TWh"))
save_figure(p5, "fig05_cooling_demand_and_savings", list(a = cd, b = ce), width = 12, height = 5)

# ---- Fig. 6  Direct building GHG emissions ---------------------------------------
bg <- building_ghg()
tot <- bg %>% filter(scenario %in% c("No Policy", "NZ 2060")) %>%
  group_by(scenario, year) %>% summarise(value = sum(value), .groups = "drop") %>%
  mutate(Units = "MtCO2e")
nz <- bg %>% filter(scenario == "NZ 2060", gas != "CH4 and N2O")   # CH4+N2O < 0.02 Mt
p6a <- plot_scenario_lines(tot, expression(Mt ~ CO[2] * eq), limits = c(0, 150),
                           breaks = seq(0, 150, 25)) +
  labs(title = "Direct GHG Emissions - Building Sector")
p6b <- ggplot(nz, aes(factor(year), value, fill = gas)) +
  geom_col(width = 0.85) +
  scale_fill_manual(values = PAL_BLD_GHG, labels = c(CO2 = expression(CO[2]), "F-Gases" = "F-Gases")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  labs(x = NULL, y = expression(Mt ~ CO[2] * eq), title = "Building Direct GHG Emissions - NZ 2060") +
  theme_ksa()
save_figure(p6a + p6b, "fig06_building_ghg_emissions", list(a = tot, b = nz), width = 12, height = 5)
