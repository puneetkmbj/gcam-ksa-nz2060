# ==============================================================================
# Figures for:
#   Qiu, Y., Kamboj, P., Hejazi, M., Iyer, G., Kyle, P. (2024).
#   Reaching Net-Zero GHG Emissions in Saudi Arabia by 2060: Transformation of the
#   Industrial Sector. KAPSARC Discussion Paper. https://doi.org/10.30573/KS--2024-DP18
#
# Data: industry results for this paper are read from data/external/industry/
# (the published figure data). Their NDCs Continued and NZ 2060 values come from a
# model run that differs slightly from ksa_nz.dat; see README, "Data provenance".
# DAC sequestration (Fig. 12, line) is read from ksa_nz.dat.
#
# Run from anywhere inside the repository:
#   Rscript papers/ks2024-dp18_industry/make_figures.R
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
PAPER_DIR <- file.path(REPO_ROOT, "papers", "ks2024-dp18_industry")
message("KS--2024-DP18  Industrial sector")

# Figs. 1-6 are model schematics.

yr_axis <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ---- Fig. 7  Industrial CO2 emissions --------------------------------------------
ind_tot <- industry_co2_total()
ind_typ <- industry_co2_by_type()
ind_wf  <- industry_co2_waterfall()
p7a <- plot_scenario_lines(ind_tot, expression(CO[2] ~ Emission ~ (MtCO[2])), from = 2020,
                           limits = c(0, 400), breaks = seq(0, 400, 80)) +
  labs(title = "a. Annual CO2 emission of the industry sector") + yr_axis
p7 <- (p7a + plot_industry_by_type(ind_typ)) / plot_industry_waterfall(ind_wf)
save_figure(p7, "fig07_industry_co2", list(a = ind_tot, b = ind_typ, c = ind_wf),
            width = 14, height = 11)

# ---- Fig. 8  Industrial final energy by fuel: level (a) and share (b) ------------
ife <- industry_final_energy_by_fuel()
fuel_bars <- function(y, ylab, lim, brk) {
  ggplot(ife, aes(factor(year), .data[[y]], fill = fuel)) +
    geom_col(width = 0.8) +
    facet_wrap(~ scenario) +
    scale_fill_manual(values = PAL_IND_FUEL, drop = FALSE) +
    scale_y_continuous(limits = lim, breaks = brk, expand = c(0, 0)) +
    scale_x_discrete(breaks = as.character(seq(2020, 2060, 10))) +
    labs(x = NULL, y = ylab) +
    theme_ksa() + yr_axis + guides(fill = guide_legend(nrow = 1, reverse = TRUE))
}
p8 <- (fuel_bars("value", "EJ", c(0, 8.5), seq(0, 8, 2)) +
         labs(title = "a. Annual industrial energy consumption")) /
      (fuel_bars("share", "Percent (%)", c(0, 100.01), seq(0, 100, 20)) +
         labs(title = "b. Share of energy type in annual industrial energy consumption")) +
      plot_layout(guides = "collect") & theme(legend.position = "bottom")
save_figure(p8, "fig08_industry_final_energy_by_fuel", ife, width = 10, height = 9)

# ---- Fig. 9  Cement production technology shares ---------------------------------
cem <- cement_tech_share()
save_figure(plot_share_bars(cem, "technology", PAL_CEMENT) +
              scale_x_discrete(breaks = as.character(seq(2020, 2060, 10))) +
              labs(title = "Share of technology in cement production"),
            "fig09_cement_technology_share", cem, width = 10, height = 5)

# ---- Fig. 10  Iron and steel production technology shares -------------------------
stl <- steel_tech_share()
save_figure(plot_share_bars(stl, "technology", PAL_STEEL) +
              scale_x_discrete(breaks = as.character(seq(2020, 2060, 10))) +
              guides(fill = guide_legend(nrow = 1)),
            "fig10_steel_technology_share", stl, width = 10, height = 5.5)

# ---- Fig. 11  Refined liquids production -----------------------------------------
ref <- refining_output()
p11 <- ggplot(ref, aes(factor(year), value)) +
  geom_col(fill = "#2F5F2F", width = 0.8) +
  facet_wrap(~ scenario) +
  scale_y_continuous(limits = c(0, 8.5), breaks = seq(0, 8, 2), expand = c(0, 0)) +
  scale_x_discrete(breaks = as.character(seq(2020, 2060, 10))) +
  labs(x = NULL, y = "EJ") +
  theme_ksa() + yr_axis
save_figure(p11, "fig11_refined_liquids_production", ref, width = 9, height = 4.5)

# ---- Fig. 12  DAC energy use (bars) and CO2 captured (line), NZ 2060 --------------
dace <- dac_energy()
dacs <- dac_sequestration() %>% filter(scenario == "NZ 2060", year >= 2020)
k <- 200   # MtCO2 per EJ on the secondary axis (2.0 EJ <-> 400 MtCO2)
p12 <- ggplot() +
  geom_col(data = dace, aes(year, value, fill = fuel), width = 4) +
  geom_line(data = dacs, aes(year, value / k), linewidth = 0.8) +
  scale_fill_manual(values = PAL_DAC_FUEL, name = "Fuel type") +
  scale_x_continuous(limits = c(2017, 2063), breaks = seq(2020, 2060, 10)) +
  scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2, 0.5), expand = c(0, 0),
                     name = "DACCS energy consumption (EJ)",
                     sec.axis = sec_axis(~ . * k, name = expression(DACCS ~ annual ~ sequestration ~ (MtCO[2])),
                                         breaks = seq(0, 400, 100))) +
  labs(x = NULL, title = "DACCS annual energy consumption and sequestration, NZ 2060") +
  theme_ksa(legend_position = c(0.2, 0.8)) + yr_axis +
  theme(legend.title = element_text(), legend.background = element_rect(fill = "white", colour = "grey30"))
save_figure(p12, "fig12_dac_energy_and_sequestration", list(energy = dace, sequestration = dacs),
            width = 8, height = 5.5)
