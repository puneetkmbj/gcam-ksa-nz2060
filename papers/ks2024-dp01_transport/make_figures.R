# ==============================================================================
# Figures for:
#   Kamboj, P., Hejazi, M., Qiu, Y., Kyle, P., Iyer, G. (2024).
#   Saudi Arabia Net-Zero GHG Emissions by 2060: Transformation of the Transport
#   Sector. KAPSARC Discussion Paper. https://doi.org/10.30573/KS--2024-DP01
#
# Run from anywhere inside the repository:
#   Rscript papers/ks2024-dp01_transport/make_figures.R
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
PAPER_DIR <- file.path(REPO_ROOT, "papers", "ks2024-dp01_transport")
message("KS--2024-DP01  Transport sector")

# Figs. 1-3 are model schematics.
cp_nz <- c("Current Policy", "NZ 2060")

# ---- Fig. 4  Passenger service demand by mode, Current Policy vs NZ 2060 ----------
pass <- transport_service_by_mode(combine_aviation = TRUE) %>%
  filter(segment == "Passenger", scenario %in% cp_nz)
p4 <- ggplot(pass, aes(year, value, fill = mode)) +
  geom_area() +
  facet_wrap(~ scenario) +
  scale_fill_manual(values = PAL_TRN_MODE) +
  scale_x_years() +
  scale_y_continuous(limits = c(0, 1400), breaks = seq(0, 1400, 200), expand = c(0, 0)) +
  labs(x = NULL, y = "billion pkm", title = "KSA Passenger Transportation Service") +
  theme_ksa() + guides(fill = guide_legend(nrow = 1))
save_figure(p4, "fig04_passenger_service_demand", pass, width = 11, height = 5.5)

# ---- Fig. 5  Four-wheeler new sales by powertrain, NZ 2060 ------------------------
car <- new_sales_share(CAR_SUBSECTORS) %>%
  filter(scenario == "NZ 2060", year %in% c(2020, 2030, 2060)) %>% mutate(Units = "billion pkm")
save_figure(plot_sales_share(car), "fig05_car_new_sales_share", car, width = 7, height = 5.5)

# ---- Fig. 6  Domestic freight and international shipping demand ------------------
frt <- transport_service_by_mode() %>%
  filter(segment == "Freight", scenario %in% cp_nz) %>%
  mutate(panel = ifelse(mode == "International Shipping", "International Shipping Service Demand",
                        "Domestic Freight Service Demand")) %>%
  group_by(scenario, panel, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(Units = "billion tkm")
p6 <- ggplot(frt, aes(year, value, linetype = scenario, shape = scenario)) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
  facet_wrap(~ panel) +
  scale_linetype_manual(values = c("Current Policy" = "solid", "NZ 2060" = "dashed")) +
  scale_shape_manual(values = c("Current Policy" = 16, "NZ 2060" = 17)) +
  scale_x_years() +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, 700), expand = c(0, 0)) +
  labs(x = NULL, y = "billion tkm") +
  theme_ksa() + theme(panel.spacing.x = unit(1.2, "lines"))
save_figure(p6, "fig06_freight_service_demand", frt, width = 11, height = 5.5)

# ---- Fig. 7  Truck new sales by powertrain, NZ 2060 ------------------------------
trk <- new_sales_share(TRUCK_SUBSECTORS) %>%
  filter(scenario == "NZ 2060", year %in% c(2020, 2030, 2060)) %>% mutate(Units = "billion tkm")
save_figure(plot_sales_share(trk), "fig07_truck_new_sales_share", trk, width = 7, height = 5.5)

# ---- Fig. 8  Transport CO2 (a) and NZ 2060 modal share of CO2 (b) ----------------
tco2 <- co2_by_assigned_sector() %>% filter(sector == "Transport")
mode <- transport_co2_by_mode() %>% filter(scenario == "NZ 2060", year %in% c(2020, 2030, 2060))
p8a <- plot_scenario_lines(tco2, expression(Mt ~ CO[2]), limits = c(0, 500),
                           breaks = seq(0, 500, 100), label_end = SCENARIO_LEVELS) +
  labs(title = expression(KSA ~ Transport ~ CO[2] ~ Emissions)) +
  guides(colour = guide_legend(nrow = 2))
p8b <- ggplot(mode, aes(factor(year), share, fill = mode)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = PAL_TRN_MODE) +
  scale_y_continuous(limits = c(0, 100.01), breaks = seq(0, 100, 25), expand = c(0, 0)) +
  labs(x = NULL, y = "Emissions share by mode (%)", title = "NZ 2060 - Emissions by Mode") +
  theme_ksa() + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  guides(fill = guide_legend(nrow = 3))
save_figure(p8a + p8b, "fig08_transport_co2", list(a = tco2, b = mode), width = 12, height = 5.5)
