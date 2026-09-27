# ==============================================================================
# Figures for:
#   Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2024).
#   The path to 2060: Saudi Arabia's long-term pathway for GHG emission reduction.
#   Energy Strategy Reviews 55, 101537. https://doi.org/10.1016/j.esr.2024.101537
#
# Run from anywhere inside the repository:  Rscript papers/esr2024_path-to-2060/make_figures.R
# Output: figures/*.png and source_data/*.csv in this folder.
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
PAPER_DIR <- file.path(REPO_ROOT, "papers", "esr2024_path-to-2060")
message("Kamboj et al. (2024), Energy Strategy Reviews")

# Fig. 1 is a model schematic.

# ---- Fig. 2  Economy-wide GHG emissions ---------------------------------------
ghg <- ghg_total()
save_figure(plot_ghg_pathway(ghg), "fig02_ghg_emissions", ghg, width = 8, height = 6)

# Fig. 3 (sector-wise emission-reduction wedges over time) needs annual output from
# single-measure sensitivity runs that are not part of this archive. The 2060
# values of the same decomposition are reproduced as Fig. 3 of KS--2023-DP30
# (papers/ks2023-dp30_long-term-pathway).

# ---- Fig. 4  Electricity generation by technology ------------------------------
gen <- elec_gen_by_tech() %>% filter(value > 0)
save_figure(plot_elec_gen_tech(gen, ncol = 4), "fig04_electricity_generation_by_technology",
            gen, width = 13, height = 6.5)

# ---- Fig. 5  Building final energy (a) and NZ 2060 fuel mix (b) ----------------
bfe  <- building_final_energy()
bfuel <- building_final_energy_by_fuel() %>% filter(scenario == "NZ 2060", value > 0)
p5 <- plot_building_fe(bfe) + plot_building_fuel(bfuel)
save_figure(p5, "fig05_building_final_energy", list(a = bfe, b = bfuel), width = 12, height = 5)

# ---- Fig. 6  Cooling demand (a) and electricity for cooling (b) ----------------
cd <- cooling_demand()
ce <- cooling_electricity()
p6 <- plot_cooling_demand(cd) + plot_cooling_electricity(ce)
save_figure(p6, "fig06_cooling_demand_and_savings", list(a = cd, b = ce), width = 12, height = 5)

# ---- Fig. 7  Industrial CO2 emissions ------------------------------------------
ind_tot <- industry_co2_total()
ind_typ <- industry_co2_by_type()
ind_wf  <- industry_co2_waterfall()
p7a <- plot_scenario_lines(ind_tot, expression(CO[2] ~ Emission ~ (MtCO[2])), from = 2020,
                           limits = c(0, 400), breaks = seq(0, 400, 80),
                           legend_position = c(0.2, 0.25)) +
  labs(title = "a. Annual CO2 emission of the industry sector") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.background = element_rect(fill = "white", colour = NA))
p7 <- (p7a + plot_industry_by_type(ind_typ)) / (plot_industry_waterfall(ind_wf) + plot_spacer()) +
  plot_layout(widths = c(1, 1))
save_figure(p7, "fig07_industry_co2", list(a = ind_tot, b = ind_typ, c = ind_wf),
            width = 15, height = 11)

# ---- Fig. 8  Transport service demand by mode ----------------------------------
trn <- transport_service_by_mode()
save_figure(plot_transport_service(trn), "fig08_transport_service_demand", trn,
            width = 13, height = 7.5)

# ---- Fig. 9  End-use electricity demand ----------------------------------------
ed <- elec_demand()
save_figure(plot_elec_demand(ed), "fig09_electricity_demand", ed, width = 8, height = 6)

# ---- Fig. 10  Final energy (a) and NZ 2060 sectoral shares (b) -----------------
fe  <- final_energy_total()
fes <- final_energy_by_sector() %>% filter(scenario == "NZ 2060")
p10a <- plot_scenario_lines(fe, "Final Energy Demand (EJ)", limits = c(0, 16),
                            breaks = seq(0, 16, 4), legend_position = c(0.75, 0.3)) +
  labs(title = "(a)") + theme(legend.background = element_rect(fill = "white", colour = NA))
p10b <- plot_fe_share_sector(fes) + labs(title = "(b)")
save_figure(p10a + p10b, "fig10_final_energy", list(a = fe, b = fes), width = 12, height = 5.5)
